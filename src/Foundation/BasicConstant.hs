module BasicConstant where

import Alias
import BasicData
import Data.Bits

aFile,aAndBFile,hFile,gAndHFile,edge,backrank,eighthRank,firstRank,kingside,queenside   :: BitBoard
initWKingsideRook,initWQueensideRook,initBKingsideRook,initBQueensideRook,initRooks     :: BitBoard
initWKing,initBKing,initKingsAndRooks,initKingsideRooks,initQueensideRooks              :: BitBoard
initWKRBQR,initWQRBKR,lightSquares,darkSquares                                          :: BitBoard
aFile                   = 0x8080808080808080
aAndBFile               = 0xc0c0c0c0c0c0c0c0
hFile                   = 0x0101010101010101
gAndHFile               = 0x0303030303030303
edge                    = 0xff818181818181ff
backrank                = 0xff000000000000ff
eighthRank              = 0xff00000000000000
firstRank               = 0x00000000000000ff
kingside                = 0x0f0f0f0f0f0f0f0f
queenside               = 0xf0f0f0f0f0f0f0f0
initWKingsideRook       = 0x0000000000000001
initWQueensideRook      = 0x0000000000000080
initBKingsideRook       = 0x0100000000000000
initBQueensideRook      = 0x8000000000000000
initRooks               = 0x8100000000000081
initWKing               = 0x0000000000000008
initBKing               = 0x0800000000000000
initKingsAndRooks       = 0x8900000000000089
initKingsideRooks       = initWKingsideRook  .|. initBKingsideRook
initQueensideRooks      = initWQueensideRook .|. initBQueensideRook
initWKRBQR              = initWKingsideRook  .|. initBQueensideRook
initWQRBKR              = initWQueensideRook .|. initBKingsideRook
lightSquares            = 0x5555555555555555
darkSquares             = complement lightSquares

pawnMaterial,knightMaterial,bishopMaterial,rookMaterial,queenMaterial,kingMaterial :: PieceValue
pawnMaterial    = 100
knightMaterial  = 320
bishopMaterial  = 330
rookMaterial    = 500
queenMaterial   = 900
kingMaterial    = 20000
