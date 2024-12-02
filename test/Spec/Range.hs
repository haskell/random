module Spec.Range
  ( symmetric
  , bounded
  , singleton
  , uniformRangeWithin
  , uniformRangeWithinExcludedF
  , uniformRangeWithinExcludedD
  ) where

import System.Random.Stateful
import Data.Proxy

(===) :: (Eq a, Show a) => a -> a -> Either String String
x === y
  | x == y = Right "OK"
  | otherwise = Left $ "Expected equal, got " ++ show x ++ " /= " ++ show y

symmetric :: (RandomGen g, UniformRange a, Eq a, Show a) => Proxy a -> g -> (a, a) -> Either String String
symmetric _ g (l, r) = fst (uniformR (l, r) g) === fst (uniformR (r, l) g)

bounded :: (RandomGen g, UniformRange a, Ord a) => Proxy a -> g -> (a, a) -> Bool
bounded _ g (l, r) = isInRange (l, r) (fst (uniformR (l, r) g))

singleton :: (RandomGen g, UniformRange a, Eq a, Show a) => Proxy a -> g -> a -> Either String String
singleton _ g x = result === x
  where
    result = fst (uniformR (x, x) g)

uniformRangeWithin :: (RandomGen g, UniformRange a, Ord a) => Proxy a -> g -> (a, a) -> Bool
uniformRangeWithin _ gen (l, r) =
  runStateGen_ gen $ \g ->
    isInRange (l, r) <$> uniformRM (l, r) g

uniformRangeWithinExcludedF :: RandomGen g => g -> Bool
uniformRangeWithinExcludedF gen =
  runStateGen_ gen $ \g ->
    (\result -> 0 < result && result <= 1) <$> uniformFloatPositive01M g

uniformRangeWithinExcludedD :: RandomGen g => g -> Bool
uniformRangeWithinExcludedD gen =
  runStateGen_ gen $ \g ->
    (\result -> 0 < result && result <= 1) <$> uniformDoublePositive01M g
