module Spec.Range (symmetric, bounded, singleton) where

import Data.Bits
import System.Random

symmetric :: (RandomGen g, Random a, Eq a) => g -> (a, a) -> Bool
symmetric g (l, r) = fst (randomR (l, r) g) == fst (randomR (r, l) g)

bounded :: (RandomGen g, Random a, Ord a) => g -> (a, a) -> Bool
bounded g (l, r) = bottom <= result && result <= top
  where
    bottom = min l r
    top = max l r
    result = fst (randomR (l, r) g)

singleton :: (RandomGen g, Random a, Eq a) => g -> a -> Bool
singleton g x = result == x
  where
    result = fst (randomR (x, x) g)
