module Position where

import Alias
import qualified AttackTable as T
import BasicData
import BasicConstant
import BitBoard
import Bitwise
import Data.Bits
import Data.Tuple
import Data.Maybe
import Data.Char
import Data.List.Split
import qualified Data.Vector.Unboxed as U
import Control.Lens hiding(without)
import System.Random

showPos :: Position -> String
showPos x  =   placement ++ "\n"
                ++  "COLOR TO MOVE:         " ++ show (x^.activeColor) ++ "\n"
                ++  "CASTLING AVAILABILITY: " ++ show (x^.castleRights) ++ "\n"
                ++  "ENPASSANT SQUARE:      " ++ (if epSqr == 0 then "NONE" else squareNameFrom (bit epSqr)) ++ "\n"
                ++  "HALF MOVE CLOCK:       " ++ show (x^.halfMoveClock) ++ "\n"
                ++  "FULL MOVE NUMBER:      " ++ show ((x^.plyCount) `div` 2 + 1) ++ "\n"
    where
        epSqr           = x^.enPassantSquare
        placement       = unlines . chunksOf 8 . map squareChar $ [63, 62 .. 0]
        squareChar sqr  = case x `atSquare` sqr of
            Just (White,piece)  -> toUpper (pieceChar piece)
            Just (Black,piece)  -> pieceChar piece
            Nothing             -> '.'

pieceChar :: PieceType -> Char
pieceChar Pawn      = 'p'
pieceChar Knight    = 'n'
pieceChar Bishop    = 'b'
pieceChar Rook      = 'r'
pieceChar Queen     = 'q'
pieceChar King      = 'k'

isEmpty,isOccupied :: Position -> BitBoard -> Bool
isEmpty x y = occupancy x `disjoint` y
isOccupied x y = occupancy x `joint` y

pieceAt :: Position -> BitBoard ->  Maybe PieceType
pieceAt p b | b `joint` p^.pawns    = Just Pawn
            | b `joint` p^.knights  = Just Knight
            | b `joint` p^.bishops  = Just Bishop
            | b `joint` p^.rooks    = Just Rook
            | b `joint` p^.queens   = Just Queen
            | b `joint` p^.kings    = Just King
            | otherwise             = Nothing

atSquare :: Position -> Square -> Maybe (Color,PieceType)
atSquare x y =  (,) color <$> x `pieceAt` bit y
    where
        color = if x ^. attackers `joint` bit y
            then x ^. activeColor
            else x ^. activeColor & opposite

occupancy,emptySquares :: Position -> BitBoard
occupancy p = p^.attackers.|.p^.defenders
emptySquares = complement.occupancy

pieces :: Position -> [(Color,PieceType,Square)]
pieces p =  [   (color,piece,square)
            |   color   <- [White,Black]
            ,   piece   <- [Pawn .. King]
            ,   square  <- map countTrailingZeros.collapse $ lookupPiece color piece]
    where
        lookupPiece White = if isWhite p then (p<-!->) else (p<~!~>)
        lookupPiece Black = if isBlack p then (p<-!->) else (p<~!~>)

infixl 9 <!>,<-!->,<~!~>
(<!>),(<-!->),(<~!~>):: Position -> PieceType -> BitBoard
p <!> Pawn      = _pawns p
p <!> Knight    = _knights p
p <!> Bishop    = _bishops p
p <!> Rook      = _rooks p
p <!> Queen     = _queens p
p <!> King      = _kings p
p@Position{_attackers = a} <-!-> piece  = a.&.p<!>piece
p@Position{_defenders = d} <~!~> piece  = d.&.p<!>piece

infixr 4 %~~
(%~~) :: PieceType -> (BitBoard -> BitBoard) -> Position -> Position
Pawn    %~~ f   = pawns%~f
Knight  %~~ f   = knights%~f
Bishop  %~~ f   = bishops%~f
Rook    %~~ f   = rooks%~f
Queen   %~~ f   = queens%~f
King    %~~ f   = kings%~f

overAttacker :: (BitBoard -> BitBoard) -> PieceType -> Position -> Position
overAttacker f piece = (attackers%~f).(piece%~~f)

overDefender :: (BitBoard -> BitBoard) -> PieceType -> Position -> Position
overDefender f piece = (defenders%~f).(piece%~~f)

move,enPassant,castle :: FromSquare -> ToSquare -> Position -> Position
move x y z  =   (materialBalance%~negate.(+ gain))
            .   (zobristKey%~xor key)
            .   (xor fromTo `overAttacker`piece)
            .   remove
            $   z
    where
        from    = bit x
        to      = bit y
        fromTo  = from .|. to
        piece   = fromMaybe (error "No piece on the from-square.") (z`pieceAt`from)
        target  = z`pieceAt`to
        color   = z^.activeColor
        remove  = maybe id (xor to `overDefender`) target
        gain    = maybe 0 material target
        key     = pieceHashKey (color,piece,x)
            `xor` pieceHashKey (color,piece,y)
            `xor` maybe 0 (\p -> pieceHashKey(opposite color,p,y)) target
enPassant x y z =   (materialBalance%~negate.(+ material Pawn))
                .   (zobristKey%~xor key)
                .   (xor fromTo `overAttacker`Pawn)
                .   remove
                $   z
    where
        from    = bit x
        to      = bit y
        fromTo  = from .|. to
        epPawn  = backward z to
        epSqr   = if isWhite z then y - 8 else y + 8
        color   = z^.activeColor
        remove  = xor epPawn `overDefender`Pawn
        key     = pieceHashKey (color,Pawn,x)
            `xor` pieceHashKey (color,Pawn,y)
            `xor` pieceHashKey (opposite color,Pawn,epSqr)
castle x y z    =   (materialBalance%~negate)
                .   (zobristKey%~xor key)
                .   (xor fromTo `overAttacker`King)
                .   (xor rFromTo `overAttacker`Rook)
                $   z
    where
        from    = bit x
        to      = bit y
        fromTo  = from .|. to
        (rFromTo,rFromSqr,rToSqr) = if to `joint` kingside
            then (to`shiftR`1.|.to`shiftL`1,y - 1,y + 1)
            else (to`shiftL`2.|.to`shiftR`1,y + 2,y - 1)
        color = z^.activeColor
        key     = pieceHashKey (color,King,x)
            `xor` pieceHashKey (color,King,y)
            `xor` pieceHashKey (color,Rook,rFromSqr)
            `xor` pieceHashKey (color,Rook,rToSqr)

promotion :: PieceType -> FromSquare -> ToSquare -> Position -> Position
promotion w x y z   =   (materialBalance%~ negate.(+ gain))
                    .   (zobristKey%~xor key)
                    .   (xor to`overAttacker`w)
                    .   (xor from `overAttacker`Pawn)
                    .   remove
                    $   z
    where
        from    = bit x
        to      = bit y
        fromTo  = from .|. to
        target  = z`pieceAt`to
        color   = z^.activeColor
        remove  = maybe id (xor to `overDefender`) target
        gain    = maybe 0 material target + material w - material Pawn
        key     = pieceHashKey (color,Pawn,x)
            `xor` pieceHashKey (color,w,y)
            `xor` maybe 0 (\p -> pieceHashKey(opposite color,p,y)) target

swapOccupancy :: Position -> Position
swapOccupancy p@Position{_attackers = a,_defenders = d} = p{_attackers = d,_defenders = a}

opposite :: Color -> Color
opposite White = Black
opposite Black = White

isWhite,isBlack :: Position -> Bool
isWhite = (== White)._activeColor
isBlack = (== Black)._activeColor

forward,forward2,backward,backward2 :: Position -> BitBoard -> BitBoard
forward x   = if isWhite x then (`shiftL`8) else (`shiftR`8)
forward2 x  = if isWhite x then (`shiftL`16) else (`shiftR`16)
backward x  = if isBlack x then (`shiftL`8) else (`shiftR`8)
backward2 x = if isBlack x then (`shiftL`16) else (`shiftR`16)

changeColor :: Position -> Position
changeColor = (zobristKey%~xor colorHashKey).(activeColor%~opposite).swapOccupancy

castleRightWithout :: CastleRight -> CastleRight -> CastleRight
x `castleRightWithout` y = toEnum $ fromEnum x `without` fromEnum y

combineCastleRight :: CastleRight -> CastleRight -> CastleRight
combineCastleRight x y = toEnum $ fromEnum x .|. fromEnum y

includeKingsideCastle,includeQueensideCastle :: CastleRight -> Bool
includeKingsideCastle x     = fromEnum x `joint` fromEnum WKBK
includeQueensideCastle x    = fromEnum x `joint` fromEnum WQBQ

activeCastleRight :: Position -> CastleRight
activeCastleRight x = case x^.activeColor of
    White   -> (x^.castleRights) `castleRightWithout` BKBQ
    Black   -> (x^.castleRights) `castleRightWithout` WKWQ

disableCastling :: FromSquare -> ToSquare -> Position -> Position
disableCastling x y z = z & (zobristKey%~xor key).(castleRights.~cRight')
    where
        cRight  = z^.castleRights
        cRight' | relevantBits == initWKing             = cRight`castleRightWithout`WKWQ
                | relevantBits == initBKing             = cRight`castleRightWithout`BKBQ
                | relevantBits == initWKingsideRook     = cRight`castleRightWithout`WK
                | relevantBits == initWQueensideRook    = cRight`castleRightWithout`WQ
                | relevantBits == initBKingsideRook     = cRight`castleRightWithout`BK
                | relevantBits == initBQueensideRook    = cRight`castleRightWithout`BQ
                | relevantBits == initKingsideRooks     = cRight`castleRightWithout`WKBK
                | relevantBits == initQueensideRooks    = cRight`castleRightWithout`WQBQ
                | relevantBits == initWKRBQR            = cRight`castleRightWithout`WKBQ
                | relevantBits == initWQRBKR            = cRight`castleRightWithout`WQBK
                | otherwise                             = cRight
            where   relevantBits = initKingsAndRooks .&. (bit x .|. bit y)
        key     = castleHashKey cRight
            `xor` castleHashKey cRight'

--This should be called BEFORE the piece placement is arranged.
arrangeEPSquare :: FromSquare -> ToSquare -> Position -> Position
arrangeEPSquare x y z = z & (zobristKey%~xor key).(enPassantSquare.~epSqr')
    where
        isDoublePush    = abs (x - y) == 16 && fromMaybe (error "No piece on the from-square.") (z`pieceAt`bit x) == Pawn
        epSqr           = z^.enPassantSquare
        epSqr'          = if isDoublePush then (x + y) `div` 2 else 0
        key             = (if epSqr == 0 then 0 else epHashKey epSqr)
                    `xor` (if isDoublePush then epHashKey epSqr' else 0)

disableEPSquare :: Position -> Position
disableEPSquare x = x & (zobristKey%~xor key).(enPassantSquare.~0)
    where
        epSqr           = x^.enPassantSquare
        key             = if epSqr == 0 then 0 else epHashKey epSqr


--This should be called BEFORE the piece placement is arranged.
arrangeHalfMoveClock :: FromSquare -> ToSquare -> Position -> Position
arrangeHalfMoveClock x y z = z & halfMoveClock .~ halfmove'
    where
        halfmove    = z^.halfMoveClock
        halfmove'   | fromMaybe (error "No piece on the from-square.") (z`pieceAt`bit x) == Pawn = 0
                    | bit y`joint`(z^.defenders)    = 0
                    | otherwise                     = halfmove + 1

isInCheck :: Position -> Bool
isInCheck = populated . _checkers

findCheckers :: Position -> BitBoard
findCheckers p =    (T.pawnAttack (p^.activeColor) ownKingSquare.&.p<~!~>Pawn)
                .|. (T.knightAttack ownKingSquare.&.p<~!~>Knight)
                .|. (T.bishopAttack ocp ownKingSquare.&.p<~!~>Bishop)
                .|. (T.rookAttack ocp ownKingSquare.&.p<~!~>Rook)
                .|. (T.queenAttack ocp ownKingSquare.&.p<~!~>Queen)
    where   ownKingSquare   = countTrailingZeros (p<-!->King)
            ocp             = occupancy p

findPinners :: Position -> (BitBoard,BitBoard)
findPinners p@Position{_attackers = a,_defenders = d,_bishops = b,_rooks = r,_queens = q}
    = (praysForBishops,praysForRooks)
    where   ocp     = occupancy p
            ownKingSquare = countTrailingZeros (p <-!-> King)
            praysForRooks   = possiblePrays.&.rookAttack ocp pinners
                where
                    pinners         =   T.rookAttack (ocp `xor` possiblePrays) ownKingSquare
                                    .&. ((r.|.q).&.d)
                    possiblePrays   = T.rookAttack ocp ownKingSquare .&. a
            praysForBishops = possiblePrays.&.bishopAttack ocp pinners
                where
                    pinners         =   T.bishopAttack (ocp `xor` possiblePrays) ownKingSquare
                                    .&. ((b.|.q).&.d)
                    possiblePrays   = T.bishopAttack ocp ownKingSquare .&. a

calcDefendMap :: Position -> AttackMap
calcDefendMap pos@Position{_attackers = a,_defenders = d,_queens = q}
    =   pawnAttack (pos^.activeColor&opposite) (pos<~!~>Pawn)
    .|. knightAttack (pos<~!~>Knight)
    .|. bishopAttack ocp (bq.&.d)
    .|. rookAttack ocp (rq.&.d)
    .|. kingAttack (pos<~!~>King)
    where   bq = pos<!>Bishop   .|. q
            rq = pos<!>Rook     .|. q
            ocp = occupancy pos `xor` pos<-!->King

material :: PieceType -> Int
material Pawn   = pawnMaterial
material Knight = knightMaterial
material Bishop = bishopMaterial
material Rook   = rookMaterial
material Queen  = queenMaterial
material King   = kingMaterial

pieceHashKey :: (Color,PieceType,Square) -> ZobristKey
pieceHashKey (x,y,z) =  keys U.! (fromEnum x + fromEnum y * 2 + z * 6)
    where   keys = U.fromList.take (64 * 12).randoms.mkStdGen $ 123643125

colorHashKey :: ZobristKey
colorHashKey = fst.random.mkStdGen $ 9864217986452

castleHashKey :: CastleRight -> ZobristKey
castleHashKey x = keys U.! fromEnum x
    where   keys = U.fromList.take 16.randoms.mkStdGen $ 654312345

epHashKey :: Square -> ZobristKey
epHashKey x = keys U.! (x `rem` 8)
    where   keys = U.fromList.take 8.randoms.mkStdGen $ 432432

calcHashKey :: Position -> ZobristKey
calcHashKey pos@Position{_castleRights = cRight,_enPassantSquare = epSquare}
    = placementHash `xor` colorHash `xor` castleHash `xor` epHash
    where
        placementHash   = foldl1 xor.map pieceHashKey $ pieces pos
        colorHash       = if isBlack pos then colorHashKey else 0
        castleHash      = castleHashKey cRight
        epHash          = if epSquare == 0 then 0 else epHashKey epSquare

calcMaterial :: Position -> PieceValue
calcMaterial x = sum [   attackerMaterial - defenderMaterial
                    |   piece <- [Pawn .. King]
                    ,   let value               = material piece
                            attackerMaterial    = value * popCount (x<-!->piece)
                            defenderMaterial    = value * popCount (x<~!~>piece)]

canDraw :: Position -> Bool
canDraw x = x^.halfMoveClock >= 100 || repetition
    where
        repetition = (>= 2) . length . filter (== x^.zobristKey) $ x^.history

minorPieces :: Position -> BitBoard
minorPieces x = x^.knights .|. x^.bishops

insufficientMaterial :: Position -> Bool
insufficientMaterial x  =   popCount ocp == 2
                        ||  popCount ocp == 3 && ocp `joint` minorPieces x
                        ||  popCount ocp == 4 && popCount (x<-!->Knight) == 2
                        ||  popCount ocp == 4 && popCount (x<~!~>Knight) == 2
                        ||  popCount ocp == 4 && popCount (x<-!->Bishop) == 1 && popCount (x<~!~>Bishop) == 1 && popCount (x<!>Bishop .&. lightSquares) /= 1
    where ocp = occupancy x


endGame :: Position -> Bool
endGame x = popCount (_attackers x `without` x<!>Pawn) <= 3
