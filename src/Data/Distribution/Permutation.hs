module Data.Distribution.Permutation where

import Control.Monad.Primitive
{- | this permutation algorithm is due to knuth.

to construct a permutation of n symbols, @0... n-1@

initialize a size n array A with position @i@ having value @i@

(may choose to precompute the sequence of permutations before setting up the array)

forM [1 .. n-1] \j ->  pick a uniform sample s from the interval [j, n-1],
then swap the values at A[j-1] and A[s]

return the array A
-}
--samplePermutation :: Monad m =>
