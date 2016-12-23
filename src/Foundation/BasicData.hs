{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}
module BasicData where

import Alias
import Bitwise
import Data.Word
import Control.Lens

data Color  = White
            | Black
    deriving (Eq,Enum,Show)

data Direction  =   North
                |   NorthEast
                |   East
                |   SouthEast
                |   South
                |   SouthWest
                |   West
                |   NorthWest

data PieceType  = Pawn
                | Knight
                | Bishop
                | Rook
                | Queen
                | King
    deriving(Eq,Read,Ord,Enum,Show)

data CastleRight    = None
                    | BQ
                    | BK
                    | BKBQ
                    | WQ
                    | WQBQ
                    | WQBK
                    | WQBKBQ
                    | WK
                    | WKBQ
                    | WKBK
                    | WKBKBQ
                    | WKWQ
                    | WKWQBQ
                    | WQWQBK
                    | WKWQBKBQ
    deriving(Enum,Eq)

instance Show CastleRight where
    show x  =   let bits    = fromEnum x
                    none    = if bits == 0 then "-" else ""
                    wk      = if bits `joint` 0x8 then "K" else ""
                    wq      = if bits `joint` 0x4 then "Q" else ""
                    bk      = if bits `joint` 0x2 then "k" else ""
                    bq      = if bits `joint` 0x1 then "q" else ""
                in  none ++ wk ++ wq ++ bk ++ bq

data MoveType   = Normal
                | EnPassant
                | Castling
                | Promotion
    deriving(Enum,Eq,Show)

data Position = Position {
        _attackers,_defenders,_pawns,_knights,_bishops,_rooks,_queens,_kings :: BitBoard
    ,   _activeColor :: Color
    ,   _castleRights :: CastleRight
    ,   _enPassantSquare :: Square
    ,   _halfMoveClock,_plyCount :: Int
    ,   _checkers,_pinnedByRooks,_pinnedByBishops,_defendMap :: BitBoard
    ,   _materialBalance :: Int
    ,   _zobristKey :: ZobristKey
    ,   _history :: [ZobristKey]}
    deriving(Show,Eq)
makeLenses ''Position

data ScoreType  = Exact
                | LowerBound
                | UpperBound
    deriving (Eq)
