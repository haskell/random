{-# LANGUAGE ScopedTypeVariables #-}
module Data.Distribution.Integers where

import Data.Word(Word64)
import Data.Bits

--sampleWordRange :: Monad m => (m Word64) -> (Word64,Word64) -> Either

{- | @'sampleWordRangeSimplified' hi@ samples  the closed finite interval @[0,hi]@ -}
sampleWordRangeSimplified :: forall m . Monad m => m Word64 -> Word64 -> m Word64
sampleWordRangeSimplified mwd upper
        | upper + 1 == 0 = mwd
        -- full 0 ... 2^64-1 range
        | popCount (upper + 1) == 1 = do  wd <- mwd ; return (wd `mod` (upper + 1))
        -- power of two range of the form 0 ... 2^k -1, for  0 < k < 64
        | otherwise = rejectionSampler
        -- we need to do rejection sampling now!
    where
      rejectionSampler :: m Word64
      rejectionSampler = do awd <- adjustSampleValue
                            if awd <= upper then return awd
                              else rejectionSampler
      nextPower2Log :: Int
      nextPower2Log =  (64 - countLeadingZeros upper  )
      adjustSampleValue :: m Word64
      adjustSampleValue = if nextPower2Log == 64
                            then mwd
                            else do wd <- mwd ; return (wd `mod` (bit nextPower2Log))


