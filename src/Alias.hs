module Alias where

import Data.Word
import qualified Data.IntMap as M

type BitBoard           = Word64
type ZobristKey         = Word64
type From               = BitBoard
type To                 = BitBoard
type PieceMap           = BitBoard
type EmptyMap           = BitBoard
type AttackMap          = BitBoard
type Occupancy          = BitBoard
type Magic              = BitBoard
type Depth              = Int
type Counter            = Int
type Timeout            = Int
type Square             = Int
type TableSize          = Int
type FromSquare         = Square
type ToSquare           = Square
type Evaluation         = Int
type PieceValue         = Evaluation
type Alpha              = Evaluation
type Beta               = Evaluation
type Lower              = Evaluation
type Upper              = Evaluation
type Move               = Word16
type Variation          = [Move]
type Killers            = [Move]
type Sequence           = [Move]
type FEN                = String
