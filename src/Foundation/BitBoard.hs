{-# LANGUAGE Strict #-}
module BitBoard where

import Alias
import BasicData
import BasicConstant
import Bitwise
import Data.Word
import Data.Bits
import Data.Bool
import Data.List.Split

showBoard :: BitBoard -> String
showBoard x =   unlines
            .   chunksOf 8
            .   map (bool '.' '1'.joint x.bit)
            $   [63,62..0]

squareNameFrom :: BitBoard -> String
squareNameFrom x    | oneBit x  =   let square  = countTrailingZeros x
                                        file    = "hgfedcba" !! (square `rem` 8)
                                        rank    = "12345678" !! (square `div` 8)
                                    in  [file,rank]
                    | otherwise =   error "More than one bit is populated."

pawnAttack :: Color -> PieceMap -> AttackMap
pawnAttack White x = step NorthEast x .|.step NorthWest x
pawnAttack Black x = step SouthEast x .|.step SouthWest x

knightAttack :: PieceMap -> AttackMap
knightAttack x
    =   (x `shiftL` 15`without`aFile)
    .|. (x `shiftL` 6`without`aAndBFile)
    .|. (x `shiftR` 10`without`aAndBFile)
    .|. (x `shiftR` 17`without`aFile)
    .|. (x `shiftR` 15`without`hFile)
    .|. (x `shiftR` 6`without`gAndHFile)
    .|. (x `shiftL` 10`without`gAndHFile)
    .|. (x `shiftL` 17`without`hFile)

kingAttack :: PieceMap -> AttackMap
kingAttack x
    =   step North x
    .|. step NorthEast x
    .|. step East x
    .|. step SouthEast x
    .|. step South x
    .|. step SouthWest x
    .|. step West x
    .|. step NorthWest x

step :: Direction -> PieceMap -> BitBoard
step North      x   = x`shiftL`8
step NorthEast  x   = x`shiftL`7`without`aFile
step East       x   = x`shiftR`1`without`aFile
step SouthEast  x   = x`shiftR`9`without`aFile
step South      x   = x`shiftR`8
step SouthWest  x   = x`shiftR`7`without`hFile
step West       x   = x`shiftL`1`without`hFile
step NorthWest  x   = x`shiftL`9`without`hFile

bishopAttack :: Occupancy -> PieceMap -> AttackMap
bishopAttack x y    =   attack NorthEast x' y
                    .|. attack SouthEast x' y
                    .|. attack SouthWest x' y
                    .|. attack NorthWest x' y
    where
        x' = complement x

rookAttack :: Occupancy -> PieceMap -> AttackMap
rookAttack x y      =   attack North x' y
                    .|. attack East x' y
                    .|. attack South x' y
                    .|. attack West x' y
    where
        x' = complement x

fillVertically :: PieceMap -> BitBoard
fillVertically x = fill North x .|. fill South x

fill :: Direction -> PieceMap -> BitBoard
fill North x = x''.|.x''`shiftL`32
    where
        x'  = x.|.x`shiftL`8
        x'' = x'.|.x'`shiftL`16
fill NorthEast x = x''.|. x''`shiftL`28.&.onBoard''
    where
        onBoard     = complement aFile
        x'          = x.|.x`shiftL`7.&.onBoard
        onBoard'    = onBoard.&.onBoard`shiftL`7
        x''         = x'.|.x'`shiftL`14.&.onBoard'
        onBoard''   = onBoard'.&.onBoard'`shiftL`14
fill East x = x''.|. x''`shiftR`4.&.onBoard''
    where
        onBoard     = complement aFile
        x'          = x.|.x`shiftR`1.&.onBoard
        onBoard'    = onBoard.&.onBoard`shiftR`1
        x''         = x'.|.x'`shiftR`2.&.onBoard'
        onBoard''   = onBoard'.&.onBoard'`shiftR`2
fill SouthEast x = x''.|.x''`shiftR`36.&.onBoard''
    where
        onBoard     = complement aFile
        x'          = x.|.x`shiftR`9.&.onBoard
        onBoard'    = onBoard.&.onBoard`shiftR`9
        x''         = x'.|.x'`shiftR`18.&.onBoard'
        onBoard''   = onBoard'.&.onBoard'`shiftR`18
fill South x = x''.|.x''`shiftR`32
    where
        x'  = x.|.x`shiftR`8
        x'' = x'.|.x'`shiftR`16
fill SouthWest x = x''.|.x''`shiftR`28.&.onBoard''
    where
        onBoard     = complement hFile
        x'          = x.|.x`shiftR`7.&.onBoard
        onBoard'    = onBoard.&.onBoard`shiftR`7
        x''         = x'.|.x'`shiftR`14.&.onBoard'
        onBoard''   = onBoard'.&.onBoard'`shiftR`14
fill West x = x''.|. x''`shiftL`4.&.onBoard''
    where
        onBoard     = complement hFile
        x'          = x.|.x`shiftL`1.&.onBoard
        onBoard'    = onBoard.&.onBoard`shiftL`1
        x''         = x'.|.x'`shiftL`2.&.onBoard'
        onBoard''   = onBoard'.&.onBoard'`shiftL`2
fill NorthWest x = x''.|.x''`shiftL`36.&.onBoard''
    where
        onBoard     = complement hFile
        x'          = x.|.x`shiftL`9.&.onBoard
        onBoard'    = onBoard.&.onBoard`shiftL`9
        x''         = x'.|.x'`shiftL`18.&.onBoard'
        onBoard''   = onBoard'.&.onBoard'`shiftL`18

attack :: Direction -> EmptyMap -> PieceMap -> BitBoard
attack North x y
    = (y''.|.y''`shiftL`32.&.x'')`shiftL`8
    where   y'  = y.|.y`shiftL`8.&.x
            x'  = x.&.x`shiftL`8
            y'' = y'.|.y'`shiftL`16.&.x'
            x'' = x'.&.x'`shiftL`16
attack NorthEast x y
    = (y''.|.y''`shiftL`28.&.x''.&.onBoard'')`shiftL`7.&.onBoard
    where   onBoard     = complement aFile
            y'          = y.|.y`shiftL`7.&.x.&.onBoard
            x'          = x.&.x`shiftL`7.&.onBoard
            onBoard'    = onBoard.&.onBoard`shiftL`7
            y''         = y'.|.y'`shiftL`14.&.x'.&.onBoard'
            x''         = x'.&.x'`shiftL`14.&.onBoard'
            onBoard''   = onBoard'.&.onBoard'`shiftL`14
attack East x y
    = (y''.|.y''`shiftR`4.&.x''.&.onBoard'')`shiftR`1.&.onBoard
    where   onBoard     = complement aFile
            y'          = y.|.y`shiftR`1.&.x.&.onBoard
            x'          = x.&.x`shiftR`1.&.onBoard
            onBoard'    = onBoard.&.onBoard`shiftR`1
            y''         = y'.|.y'`shiftR`2.&.x'.&.onBoard'
            x''         = x'.&.x'`shiftR`2.&.onBoard'
            onBoard''   = onBoard'.&.onBoard'`shiftR`2
attack SouthEast x y
    = (y''.|.y''`shiftR`36.&.x''.&.onBoard'')`shiftR`9.&.onBoard
    where   onBoard     = complement aFile
            y'          = y.|.y`shiftR`9.&.x.&.onBoard
            x'          = x.&.x`shiftR`9.&.onBoard
            onBoard'    = onBoard.&.onBoard`shiftR`9
            y''         = y'.|.y'`shiftR`18.&.x'.&.onBoard'
            x''         = x'.&.x'`shiftR`18.&.onBoard'
            onBoard''   = onBoard'.&.onBoard'`shiftR`18
attack South x y
    = (y''.|.y''`shiftR`32.&.x'')`shiftR`8
    where   y'  = y.|.y`shiftR`8.&.x
            x'  = x.&.x`shiftR`8
            y'' = y'.|.y'`shiftR`16.&.x'
            x'' = x'.&.x'`shiftR`16
attack SouthWest x y
    = (y''.|.y''`shiftR`28.&.x''.&.onBoard'')`shiftR`7.&.onBoard
    where   onBoard     = complement hFile
            y'          = y.|.y`shiftR`7.&.x.&.onBoard
            x'          = x.&.x`shiftR`7.&.onBoard
            onBoard'    = onBoard.&.onBoard`shiftR`7
            y''         = y'.|.y'`shiftR`14.&.x'.&.onBoard'
            x''         = x'.&.x'`shiftR`14.&.onBoard'
            onBoard''   = onBoard'.&.onBoard'`shiftR`14
attack West x y
    = (y''.|.y''`shiftL`4.&.x''.&.onBoard'')`shiftL`1.&.onBoard
    where   onBoard     = complement hFile
            y'          = y.|.y`shiftL`1.&.x.&.onBoard
            x'          = x.&.x`shiftL`1.&.onBoard
            onBoard'    = onBoard.&.onBoard`shiftL`1
            y''         = y'.|.y'`shiftL`2.&.x'.&.onBoard'
            x''         = x'.&.x'`shiftL`2.&.onBoard'
            onBoard''   = onBoard'.&.onBoard'`shiftL`2
attack NorthWest x y
    = (y''.|.y''`shiftL`36.&.x''.&.onBoard'')`shiftL`9.&.onBoard
    where   onBoard     = complement hFile
            y'          = y.|.y`shiftL`9.&.x.&.onBoard
            x'          = x.&.x`shiftL`9.&.onBoard
            onBoard'    = onBoard.&.onBoard`shiftL`9
            y''         = y'.|.y'`shiftL`18.&.x'.&.onBoard'
            x''         = x'.&.x'`shiftL`18.&.onBoard'
            onBoard''   = onBoard'.&.onBoard'`shiftL`18

{-# INLINE invert #-}
invert :: BitBoard -> BitBoard
invert = deltaSwap 0x00ff00ff00ff00ff 8 . deltaSwap 0x0000ffff0000ffff 16 . deltaSwap 0x00000000ffffffff 32
