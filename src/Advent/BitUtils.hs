module Advent.BitUtils
  ( fromBits,
  )
where

import Data.Bits (Bits, setBit, shiftL, zeroBits)
import Data.Foldable (foldl')

fromBits :: (Foldable t, Bits b) => t Bool -> b
fromBits = foldl' f zeroBits
  where
    f acc True = setBit (shiftL acc 1) 0
    f acc False = shiftL acc 1
