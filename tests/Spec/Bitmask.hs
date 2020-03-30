module Spec.Bitmask (symmetric, bounded, singleton) where

import Data.Bits
import System.Random

symmetric :: (RandomGen g, FiniteBits a, Num a, Ord a, Random a) => g -> (a, a) -> Bool
symmetric g (l, r) = fst (bitmaskWithRejection (l, r) g) == fst (bitmaskWithRejection (r, l) g)

bounded :: (RandomGen g, FiniteBits a, Num a, Ord a, Random a) => g -> (a, a) -> Bool
bounded g (l, r) = bottom <= result && result <= top
  where
    bottom = min l r
    top = max l r
    result = fst (bitmaskWithRejection (l, r) g)

singleton :: (RandomGen g, FiniteBits a, Num a, Ord a, Random a) => g -> a -> Bool
singleton g x = result == x
  where
    result = fst (bitmaskWithRejection (x, x) g)
