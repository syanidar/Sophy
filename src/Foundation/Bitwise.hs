{-# LANGUAGE Strict #-}
module Bitwise where

import Data.Bits
import Data.List

{-# INLINE joint #-}
{-# INLINE disjoint #-}
infix 4 `joint` , `disjoint`
joint,disjoint :: (Bits a) => a -> a -> Bool
a `joint` b = (a.&.b) /= zeroBits
a `disjoint` b = (a.&.b) == zeroBits

{-# INLINE populated #-}
populated :: (Bits a) => a -> Bool
populated = (/= zeroBits)

{-# INLINE oneBit #-}
oneBit :: (Bits a, Num a) => a -> Bool
oneBit x = x /= zeroBits && x.&.(x - 1) == zeroBits

{-# INLINE ls1b #-}
ls1b :: (Bits a, Num a) => a -> a
ls1b x = x .&. negate x

{-# INLINE without #-}
infixl 7 `without`
without :: (Bits a) => a -> a -> a
a `without` b = a .&. complement b

{-# INLINE collapse #-}
collapse :: (Bits a, Num a) => a -> [a]
collapse = unfoldr step
    where   step xs = let x = ls1b xs in if xs == zeroBits then Nothing else Just (x, xs `xor` x)

{-# INLINE deltaSwap #-}
deltaSwap :: (Bits a) => a -> Int -> a -> a
deltaSwap mask distance source = source `xor` xored `xor` xored `shiftR` distance
    where   xored       = fromMask `shiftL` distance `xor` toMask
            fromMask    = mask .&. source
            toMask      = mask `shiftL` distance .&. source
