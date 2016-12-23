module Constructor where

import Alias
import BasicData
import Position
import BitBoard
import Control.Lens
import Data.Bits
import Data.Char
import Data.Function
import Control.Monad
import Text.ParserCombinators.Parsec

startPos,nullPos :: Position
startPos = pos & zobristKey .~ calcHashKey pos
    where pos = Position{
        _attackers          = 0x000000000000ffff
    ,   _defenders          = 0xffff000000000000
    ,   _pawns              = 0x00ff00000000ff00
    ,   _knights            = 0x4200000000000042
    ,   _bishops            = 0x2400000000000024
    ,   _rooks              = 0x8100000000000081
    ,   _queens             = 0x1000000000000010
    ,   _kings              = 0x0800000000000008
    ,   _activeColor        = White
    ,   _castleRights       = WKWQBKBQ
    ,   _enPassantSquare    = 0
    ,   _halfMoveClock      = 0
    ,   _plyCount           = 0
    ,   _checkers           = 0
    ,   _pinnedByBishops    = 0
    ,   _pinnedByRooks      = 0
    ,   _defendMap          = 0x7effff0000000000
    ,   _materialBalance    = 0
    ,   _zobristKey         = 0
    ,   _history            = []}
nullPos = Position{
        _attackers          = 0
    ,   _defenders          = 0
    ,   _pawns              = 0
    ,   _knights            = 0
    ,   _bishops            = 0
    ,   _rooks              = 0
    ,   _queens             = 0
    ,   _kings              = 0
    ,   _activeColor        = White
    ,   _castleRights       = WKWQBKBQ
    ,   _enPassantSquare    = 0
    ,   _halfMoveClock      = 0
    ,   _plyCount           = 0
    ,   _checkers           = 0
    ,   _pinnedByBishops    = 0
    ,   _pinnedByRooks      = 0
    ,   _defendMap          = 0
    ,   _materialBalance    = 0
    ,   _zobristKey         = 0
    ,   _history            = []}

bitBoardOf :: String -> BitBoard
bitBoardOf = either (error.show) id.parse squareParser "Failed to parse square coordinates."

squareParser :: Parser BitBoard
squareParser = do
    file <- oneOf "abcdefgh"
    rank <- oneOf "12345678"
    let fileNum = 7 - (subtract`on`fromEnum) 'a' file
        rankNum = (subtract`on`fromEnum) '1' rank
    return.bit $ fileNum + rankNum * 8

fromFEN :: String -> Position
fromFEN = either (error.show) id.parse fenParser "Failed to parse FEN"

fenParser :: Parser Position
fenParser = do
    f           <- placementParser 63
    let tmp = f nullPos
    color       <- colorParser
    cRight      <- castlingRightParser
    epSqr       <- enPassantParser
    void space <|> eof
    halfmove    <- halfMoveParser <|> return 0
    void space <|> eof
    fullmove    <- fullMoveParser <|> return 1
    let ply = (fullmove - 1) * 2 + (if color == Black then 1 else 0)
        pos = tmp   &   activeColor .~ color
                    &   (if color == Black then swapOccupancy else id)
                    &   castleRights .~ cRight
                    &   enPassantSquare .~ epSqr
                    &   halfMoveClock .~ halfmove
                    &   plyCount .~ ply
        cs          = findCheckers pos
        (pb,pr)     = findPinners pos
        dMap        = calcDefendMap pos
        mBalance    = calcMaterial pos
        zKey        = calcHashKey pos

    pos &   checkers .~ cs
        &   pinnedByBishops .~ pb
        &   pinnedByRooks .~ pr
        &   defendMap .~ dMap
        &   materialBalance .~ mBalance
        &   zobristKey .~ zKey
        &   return

placementParser :: Int -> Parser (Position -> Position)
placementParser square = do
    x   <- oneOf "PNBRQKpnbrqk12345678/"
    let nextSquare  | isDigit x     = square - digitToInt x
                    | x == '/'      = square
                    | otherwise     = square - 1
        putPiece    = case x of
            'P' -> xor (bit square) `overAttacker` Pawn
            'N' -> xor (bit square) `overAttacker` Knight
            'B' -> xor (bit square) `overAttacker` Bishop
            'R' -> xor (bit square) `overAttacker` Rook
            'Q' -> xor (bit square) `overAttacker` Queen
            'K' -> xor (bit square) `overAttacker` King
            'p' -> xor (bit square) `overDefender` Pawn
            'n' -> xor (bit square) `overDefender` Knight
            'b' -> xor (bit square) `overDefender` Bishop
            'r' -> xor (bit square) `overDefender` Rook
            'q' -> xor (bit square) `overDefender` Queen
            'k' -> xor (bit square) `overDefender` King
            _   -> id
        parseTrailingSpace = do
            space
            when (nextSquare /= -1) $ unexpected ("number of squares(" ++ show numOfSquares ++ ")")
            return id
        numOfSquares = 64 - (nextSquare + 1)

    putRemainings   <-  placementParser nextSquare
                    <|> parseTrailingSpace
    (`label` "piece placement") . return $ putPiece.putRemainings

colorParser :: Parser Color
colorParser = do
    color <- oneOf "wb"
    space
    (`label` "active color") . return $ if color == 'w'
        then White
        else Black

castlingRightParser :: Parser CastleRight
castlingRightParser = do
    xs <- string "-" <|> many1 (oneOf "KQkq")
    space
    let castleRight '-' = None
        castleRight 'K' = WK
        castleRight 'Q' = WQ
        castleRight 'k' = BK
        castleRight 'q' = BQ
    (`label` "castling availability") . return . foldl1 combineCastleRight . map castleRight $ xs

enPassantParser :: Parser Square
enPassantParser = do
    xs <- string "-" <|> many1 (oneOf "abcdefgh12345678")
    let epSqr = countTrailingZeros $ bitBoardOf xs
    (`label` "en passant target square") . return $ if xs == "-"
        then 0
        else epSqr

halfMoveParser :: Parser Int
halfMoveParser = do
    xs <- many1 digit
    (`label` "half move clock") . return $ read xs

fullMoveParser :: Parser Int
fullMoveParser = do
    xs <- many1 digit
    (`label` "full move number") . return $ read xs
