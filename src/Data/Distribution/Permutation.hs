{-# LANGUAGE ScopedTypeVariables, GADTs#-}
module Data.Distribution.Permutation where

import Control.Monad.Primitive as Prim
--import Data.Primitive.Array as DPA
import Data.Word(Word32)
import Data.Int(Int32)
import Control.Monad.ST(runST)
import Control.Monad(forM,forM_)
import Data.Vector.Unboxed.Mutable as DVM
import qualified Data.Vector.Unboxed as DV

--import Data.Distribution.Integers
{- | this permutation algorithm is due to knuth.

to construct a permutation of n symbols, @0... n-1@

initialize a size n array A with position @i@ having value @i@

(may choose to precompute the sequence of permutations before setting up the array)

forM [1 .. n-1] \j ->  pick a uniform sample s from the interval [j, n-1],
then swap the values at A[j-1] and A[s]

return the array A

or to quote the fisher-yates shuffle entry on wikipedia

-- To shuffle an array a of n elements (indices 0..n-1):
for i from 0 to n−2 do
     j ← random integer such that i ≤ j < n
     exchange a[i] and a[j]

@`samplePermutation` integerSampler size@ for now is limited to allowing permutations over
no more than 2^32 elements, mostly because if you're wanting larger permutations theres likely better
algorithms available
-}
samplePermutation :: forall m  .  (Monad m) => ((Word32,Word32)->m Word32) -> Word32 -> m (DV.Vector  Int32)
samplePermutation intervalSample wSize
  | wSize == 0 || wSize > 2^(31 ::  Int) = error "i'm not letting you do 0 or > 2^31  element permutations"
  | otherwise = do
    swapList :: [(Int,Int)] <- forM [0 ..  wSize - 2 ]
                                    (\i -> do  jay <- intervalSample (i,wSize - 1) ;
                                               return (fromIntegral i,fromIntegral jay ) )

    return $ runST $
       do vecM  <- DVM.unsafeNew (fromIntegral wSize)
          forM_  [0 :: Int .. fromIntegral wSize - 1 ]
                      (\ i -> DVM.write vecM i  (fromIntegral i :: Int32))
          executeArraySwaps swapList vecM

executeArraySwaps :: forall s m . (s~PrimState m,PrimMonad m) => [(Int,Int)]
     -> DVM.MVector s Int32 -> m (DV.Vector Int32)
executeArraySwaps [] _marr = error "you really shouldn't be invoking executeArraySwaps on empty things"
executeArraySwaps  ls@((a,_):_) marr
      | a /= 0 = error "the swap sequence list for executeArraySwaps doesn't start with a swap with zero"
      | otherwise = do swapSpots 0 ls ; DV.unsafeFreeze marr
    where
      arrayLength :: Int
      arrayLength = DVM.length marr
      swapSpots :: Int -> [(Int,Int)] -> m ()
      swapSpots ix []
          | ix >= (arrayLength - 2) = return ()
          | otherwise = error "the swap list for executeArraySwaps is shorter than the array length"
      swapSpots ix _
              | ix >=  (arrayLength - 1 ) = error "can't swap permutations beyond the array size in executeArraySwaps"
      swapSpots ix ((from,to):rest)
              | ix /=  from = error "bad coordinate mismatch "
              | otherwise =  do DVM.swap marr from to
                                swapSpots (ix +1) rest


