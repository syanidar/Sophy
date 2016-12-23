module Move (
        toLAN
    ,   nullMove
    ,   getFromSquare
    ,   getToSquare
    ,   getPromotionPiece
    ,   getMoveType
    ,   isCapture
    ,   isQuiet
    ,   isTactical
    ,   isCheck
    ,   materialGain
    ,   see
    ,   pass
    ,   makeMove
    ,   play
    ,   legalPositions
    ,   notSuicidal
    ,   legalMoves
    ,   winningCaptures
    ,   noisyMoves) where

import Alias
import qualified AttackTable as T
import Bitwise
import BitBoard
import BasicData
import BasicConstant
import Position
import Data.Bits
import Data.Maybe
import Data.Char
import Data.Composition
import Data.Function
import Data.List
import Control.Monad
import Control.Lens hiding(without)

toLAN :: Move -> String
toLAN x =   let from    = squareNameFrom . bit $ getFromSquare x
                to      = squareNameFrom . bit $ getToSquare x
                piece   =   maybeToList
                        .   fmap pieceChar
                        .   mfilter (const (getMoveType x == Promotion))
                        .   Just
                        $   getPromotionPiece x
            in  if x == nullMove then "0000" else from ++ to ++ piece

nullMove :: Move
nullMove = 0

normalMove,enPassantMove,castlingMove :: From -> To -> Move
normalMove x y      =   fromIntegral (countTrailingZeros x) `shiftL` 10
                    .|. fromIntegral (countTrailingZeros y) `shiftL` 4
enPassantMove x y   =   normalMove x y
                    .|. fromIntegral (fromEnum EnPassant)
castlingMove x y    =   normalMove x y
                    .|. fromIntegral (fromEnum Castling)

promotionMove :: From -> To -> PieceType -> Move
promotionMove x y z =   normalMove x y
                    .|. fromIntegral (fromEnum z - 1) `shiftL` 2
                    .|. fromIntegral (fromEnum Promotion)

getFromSquare,getToSquare :: Move -> Square
getFromSquare   x   = fromIntegral $ (x.&.0xfc00)`shiftR`10
getToSquare     x   = fromIntegral $ (x.&.0x03f0)`shiftR`4

getPromotionPiece :: Move -> PieceType
getPromotionPiece x = toEnum.fromIntegral $ (x.&.0x000c)`shiftR`2 + 1

getMoveType :: Move -> MoveType
getMoveType x = toEnum.fromIntegral $ x.&.0x0003

isCapture,isQuiet,isCheck :: Position -> Move -> Bool
isCapture x y =  (getMoveType y == EnPassant ||) . isOccupied x . bit $ getToSquare y
isQuiet x y = getMoveType y /= Promotion && not (isCapture x y)
isTactical = not .: isQuiet
isCheck x y = if attacker == Pawn
    then T.pawnAttack (_activeColor x) (getToSquare y) `joint` x<~!~>King
    else T.pieceAttack attacker (occupancy x) (getToSquare y) `joint` x<~!~>King
    where
        attacker = fromMaybe (error "No piece on the from-square.") $ x`pieceAt`bit (getFromSquare y)


materialGain :: Position -> Move -> Int
materialGain x y = case getMoveType y of
    EnPassant   -> 0
    _           -> toMaterial - fromMaterial
    where
        fromMaterial    =   material
                        .   fromMaybe (error "No piece on the from-square.")
                        .   pieceAt x
                        .   bit
                        $   getFromSquare y
        toMaterial      =   maybe 0 material
                        .   pieceAt x
                        .   bit
                        $   getToSquare y

pass :: Position -> Position
pass x  = x'    & checkers .~ checkers'
                & pinnedByBishops .~ pinnedByBishops'
                & pinnedByRooks .~ pinnedByRooks'
                & defendMap .~ defendMap'
    where
        x' = x  & disableEPSquare
                & changeColor
        checkers'   = findCheckers x'
        (pinnedByBishops',pinnedByRooks')
            = findPinners x'
        defendMap'  = calcDefendMap x'

makeMove :: Move -> Position -> Position
makeMove x y = y'   & checkers .~ checkers'
                    & pinnedByBishops .~ pinnedByBishops'
                    & pinnedByRooks .~ pinnedByRooks'
                    & defendMap .~ defendMap'
    where
        fromSqr     = getFromSquare x
        toSqr       = getToSquare x
        promoteTo   = getPromotionPiece x
        movePieces  = case getMoveType x of
            Normal      -> move fromSqr toSqr
            EnPassant   -> enPassant fromSqr toSqr
            Castling    -> castle fromSqr toSqr
            Promotion   -> promotion promoteTo fromSqr toSqr
        y'= y   & disableCastling fromSqr toSqr
                & arrangeEPSquare fromSqr toSqr
                & arrangeHalfMoveClock fromSqr toSqr
                & plyCount%~(+ 1)
                & movePieces
                & changeColor
                & history%~((y^.zobristKey):)
        checkers'   = findCheckers y'
        (pinnedByBishops',pinnedByRooks')
            = findPinners y'
        defendMap'  = calcDefendMap y'

play :: String -> Position -> Maybe Position
play x y = (`makeMove` y) <$> move
    where
        lowerMove = map toLower x
        move = find ((== lowerMove) . toLAN) $ legalMoves y

legalPositions :: Position -> [Position]
legalPositions x  = map (`makeMove` x) $ legalMoves x

moves :: Position -> ([Move],[Move],[Move])
moves x =   (   filter (notSuicidal x) $ pawnPush ++ doublePush ++ pawnCapture ++ epMove ++ promotion ++ promotionCapture ++ pieceMoves ++ castlingMoves
            ,   sortBy (flip compare `on` see x) . filter noisyEnough . filter (notSuicidal x) $ capture ++ pawnCapture
            ,   filter (notSuicidal x) (promotionCapture ++ promotion) ++ winningCaptures x)
    where
        noisyEnough move = see x move > 0
        ownPawns    = x<-!->Pawn
        ownKing     = x<-!->King
        normalPawns = backward2 x . forward2 x $ ownPawns
        cherryPawns = normalPawns .&. forward x backrank
        promoting   = ownPawns .&. backward x backrank
        targets     = x^.defenders
        ocp         = occupancy x
        kCastle     = if isWhite x
            then 0x0000000000000006
            else 0x0600000000000000
        qCastle     = if isWhite x
            then 0x0000000000000070
            else 0x7000000000000000
        color       = x^.activeColor
        cRight      = activeCastleRight x
        kingside    =   if  notInCheck
                        &&  includeKingsideCastle cRight
                        &&  kCastle `disjoint` ocp
            then Just $ castlingMove ownKing (ownKing`shiftR`2)
            else Nothing
        queenside   =   if  notInCheck
                        &&  includeQueensideCastle cRight
                        &&  qCastle `disjoint` ocp
            then Just $ castlingMove ownKing (ownKing`shiftL`2)
            else Nothing
        notInCheck  = not (isInCheck x)
        pawnPush    = do
            from    <-  collapse
                    .   backward x
                    .   (`without` ocp)
                    .   forward x
                    $   normalPawns
            return  $ normalMove from (forward x from)
        doublePush  = do
            from    <-  collapse
                    .   backward2 x
                    .   (`without` ocp)
                    .   forward x
                    .   (`without` ocp)
                    .   forward x
                    $   cherryPawns
            return  $ normalMove from (forward2 x from)
        pawnCapture = do
            from    <- collapse normalPawns
            to      <-  collapse
                    .   (.&. targets)
                    $   pawnAttack color from
            return  $ normalMove from to
        epMove      = if epSquare == 0
            then []
            else do
                from    <-  collapse
                        .   (.&. ownPawns)
                        $   pawnAttack (opposite color) epPlace
                return $ enPassantMove from epPlace
            where
                epSquare    = x^.enPassantSquare
                epPlace     = bit epSquare
        promotion   = do
            piece   <-  [Queen, Rook .. Knight]
            from    <-  collapse
                    .   backward x
                    .   (`without` ocp)
                    .   forward x
                    $   promoting
            return  $   promotionMove from (forward x from) piece
        promotionCapture = do
            piece   <-  [Queen, Rook .. Knight]
            from    <-  collapse promoting
            to      <-  collapse
                    .   (.&. targets)
                    $   pawnAttack color from
            return  $   promotionMove from to piece
        pieceMoves = do
            piece   <-  [Knight .. King]
            from    <-  collapse (x<-!->piece)
            to      <-  collapse
                    $   (`without`x^.attackers)
                    $   T.pieceAttack piece ocp (countTrailingZeros from)
            return  $   normalMove from to
        capture = do
            piece   <-  [Knight .. King]
            from    <-  collapse (x<-!->piece)
            to      <-  collapse
                    $   (.&.x^.defenders)
                    $   T.pieceAttack piece ocp (countTrailingZeros from)
            return  $   normalMove from to
        castlingMoves = catMaybes [kingside,queenside]

legalMoves,winningCaptures,noisyMoves :: Position -> [Move]
legalMoves x        = moves x ^. _1
winningCaptures x   = moves x ^. _2
noisyMoves x        = moves x ^. _3

notSuicidal :: Position -> Move -> Bool
notSuicidal x@Position{_attackers = as,_defenders = ds,_checkers = cs,_defendMap = dMap,_pinnedByBishops = pb,_pinnedByRooks = pr} y
    = case popCount cs of
    0   ->  case getMoveType y of
            EnPassant   ->  pinTest && epPinTest
            Castling    ->  dMap `disjoint` kingPath
            _           ->  kingFlees || (not kingMove && pinTest)
    1   ->  kingFlees
        ||  (   not kingMove
            &&  to == cs
            &&  notPinned)
        ||  (   not kingMove
            &&  (cs`joint`x<!>Bishop || cs`joint`x<!>Rook || cs`joint`x<!>Queen)
            &&  notPinned
            &&  ownKing `disjoint` T.pieceAttack (fromMaybe (error "No piece is attacking the king.") (x`pieceAt`cs)) (ocp.|.to) (countTrailingZeros cs))
        ||  (   getMoveType y == EnPassant
            &&  epPawn == cs
            &&  notPinned)
    2   ->  kingFlees
    where
        fromSqr     = getFromSquare y
        toSqr       = getToSquare y
        ownKingSqr  = countTrailingZeros ownKing
        from        = bit fromSqr
        to          = bit toSqr
        ocp         = occupancy x
        kingPath    = to .|. bit ((fromSqr + toSqr) `div` 2)
        ownKing     = x<-!->King
        epPawn      = backward x to
        pinnedMen   = pb .|. pr
        kingMove    = from == ownKing
        kingFlees   = kingMove && dMap `disjoint` to
        pinTest     = notPinned || alongBishop || alongRook
        notPinned   = from`disjoint`pinnedMen
        alongBishop = from`joint`pb && to`joint`pinnedRay
            where pinnedRay =   T.bishopAttack (ocp`xor`from) ownKingSqr
                            .&. T.bishopAttack ocp fromSqr
        alongRook   = from`joint`pr && to`joint`pinnedRay
            where pinnedRay =   T.rookAttack (ocp`xor`from) ownKingSqr
                            .&. T.rookAttack ocp fromSqr
        epPinTest   =   epPinners `disjoint` attack East epEmptyMap ownKing
                    &&  epPinners `disjoint` attack West epEmptyMap ownKing
        epPinners   = (x^.rooks.|.x^.queens).&.ds
        epEmptyMap  = complement ocp `xor` (from.|.epPawn)

see :: Position -> Move -> PieceValue
see x y = maybe 0 nextSEE attacker
    where
        toSqr = getToSquare y
        to = bit toSqr
        ocp = occupancy x
        pawnCandidate
            = ls1b $ T.pawnAttack (x ^. activeColor & opposite) toSqr.&.x<-!->Pawn
        knightCandidate
            = ls1b $ T.knightAttack toSqr .&. x<-!->Knight
        bishopCandidate
            = ls1b $ T.bishopAttack ocp toSqr .&. x<-!->Bishop
        rookCandidate
            = ls1b $ T.rookAttack ocp toSqr .&. x<-!->Rook
        queenCandidate
            = ls1b $ T.queenAttack ocp toSqr .&. x<-!->Queen
        kingCandidate
            = ls1b $ T.kingAttack toSqr .&. x<-!->King
        attacker        | populated pawnCandidate   = Just (Pawn,pawnCandidate)
                        | populated knightCandidate = Just (Knight,knightCandidate)
                        | populated bishopCandidate = Just (Bishop,bishopCandidate)
                        | populated rookCandidate   = Just (Rook,rookCandidate)
                        | populated queenCandidate  = Just (Queen,queenCandidate)
                        | populated kingCandidate   = Just (King,kingCandidate)
                        | otherwise                 = Nothing
        target      = if getMoveType y == EnPassant then backward x to else to
        targetPiece = fromMaybe (error "No piece on the target square.") (x`pieceAt`target)
        nextSEE (piece,from) = max 0 (material targetPiece - see nextPos y)
            where
                fromTo = from.|.to
                nextPos =   x
                        &   changeColor
                        .   (xor fromTo`overAttacker`piece)
                        .   (xor to`overDefender`targetPiece)
