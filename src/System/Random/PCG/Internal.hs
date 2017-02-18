{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
-- |
--
-- Standard PCG64 Random Number Generator with chosen streams, written in
-- pure haskell. See <http://www.pcg-random.org> for details.
--
module System.Random.PCG.Internal
  ( -- * PCG 64
    PCG64 (..)
  , seed
  , newPCG64

    -- * Generating random numbers
  , next32
  , next64
  , bounded

    -- * Generator utilities
  , advancePCG64
  , split
  ) where

import Data.Bits
import Data.Data
import Foreign
import GHC.Generics

-- | The multiple sequence varient of the pcg random number generator.
data PCG64 = PCG64
  {-# UNPACK #-} !Word64 -- state
  {-# UNPACK #-} !Word64 -- inc
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

-- | Create a new generator from two words.
newPCG64 :: Word64 -> Word64 -> PCG64
newPCG64 = \a b ->
  let s = state (PCG64 (a + i) i)
      i = (b `shiftL` 1) .|. 1
  in  PCG64 s i
{-# INLINE newPCG64 #-}

-- | Fixed seed.
seed :: PCG64
seed = PCG64 9600629759793949339 15726070495360670683
{-# INLINE seed #-}

-- Generating random numbers -------------------------------------------

-- All operations are done via Pair to ensure everything's strict. GHC
-- is good at inlining this so Pair doesn't exist in the generated core.
data Pair = Pair
  {-# UNPACK #-} !Word64 -- new state
  {-# UNPACK #-} !Word32 -- output

multiplier :: Word64
multiplier = 6364136223846793005
{-# INLINE multiplier #-}

-- A single step in the generator state
state :: PCG64 -> Word64
state (PCG64 s inc) = s * multiplier + inc
{-# INLINE state #-}

-- The random number output
output :: Word64 -> Word32
output s =
  (shifted `unsafeShiftR` rot) .|. (shifted `unsafeShiftL` (negate rot .&. 31))
  where
    rot     = fromIntegral $ s `shiftR` 59 :: Int
    shifted = fromIntegral $ ((s `shiftR` 18) `xor` s) `shiftR` 27 :: Word32
{-# INLINE output #-}

-- a new generator state and random number
pair :: PCG64 -> Pair
pair g@(PCG64 s _) = Pair (state g) (output s)
{-# INLINE pair #-}

-- | Return a random 'Word32' with a generator incremented by one step.
next32 :: PCG64 -> (Word32, PCG64)
next32 = \g@(PCG64 _ inc) ->
  let Pair s' r = pair g
  in  (r, PCG64 s' inc)
{-# INLINE next32 #-}

-- | Return a random 'Word64' with the generator incremented by two steps.
next64 :: PCG64 -> (Word64, PCG64)
next64 = \(PCG64 s inc) ->
  let Pair s'  w1 = pair (PCG64 s inc)
      Pair s'' w2 = pair (PCG64 s' inc)
  in  (wordsTo64Bit w1 w2, PCG64 s'' inc)
{-# INLINE next64 #-}

bounded' :: Word32 -> PCG64 -> Pair
bounded' b (PCG64 s0 inc) = go s0
  where
   t = negate b `mod` b
   go !s | r >= t    = Pair s' (r `mod` b)
         | otherwise = go s'
     where Pair s' r = pair (PCG64 s inc)
{-# INLINE bounded' #-}

-- | Generate a uniform 'Word32' less than the bound. If the bound is
--   zero, this throws a divide by zero exception.
bounded :: Word32 -> PCG64 -> (Word32, PCG64)
bounded = \b g@(PCG64 _ inc) ->
  let !(Pair s1 w) = bounded' b g
  in  (w, PCG64 s1 inc)
{-# INLINE bounded #-}

-- Utilities -----------------------------------------------------------

-- | Split the generator @g@ into @(g', g2)@ where @g'@ is @g@
--   incremented by 4 steps and @g2@ is a new generator with a
--   difference sequence to @g@.
split :: PCG64 -> (PCG64, PCG64)
split (PCG64 s inc) = (PCG64 s4 inc, mk w1 w2 w3 w4)
  where
    -- This is just something I made up. It passed big crunch and
    -- dieharder (by splitting every step) but there's probably a better
    -- way.
    mk a b c d = newPCG64 (wordsTo64Bit a b) (wordsTo64Bit c d)
    Pair s1 w1 = pair (PCG64 s  inc)
    Pair s2 w2 = pair (PCG64 s1 inc)
    Pair s3 w3 = pair (PCG64 s2 inc)
    Pair s4 w4 = pair (PCG64 s3 inc)
{-# INLINE split #-}

advancing
  :: Word64 -- amount to advance by
  -> Word64 -- state
  -> Word64 -- multiplier
  -> Word64 -- increment
  -> Word64 -- new state
advancing d0 s m0 p0 = go d0 m0 p0 1 0
  where
    go !d !cm !cp !am !ap
      | d <= 0    = am * s + ap
      | odd d     = go d' cm' cp' (am * cm) (ap * cm + cp)
      | otherwise = go d' cm' cp' am        ap
      where
        cm' = cm * cm
        cp' = (cm + 1) * cp
        d'  = d `div` 2
{-# INLINE advancing #-}

-- | Advance a pcg generator @n@ steps. You can give this @-n@ to take
--   the generator back @n@ steps.
advancePCG64 :: Word64 -> PCG64 -> PCG64
advancePCG64 = \d (PCG64 s inc) -> PCG64 (advancing d s multiplier inc) inc
{-# INLINE advancePCG64 #-}

-- Misc ----------------------------------------------------------------

wordsTo64Bit :: Integral a => Word32 -> Word32 -> a
wordsTo64Bit x y =
    fromIntegral ((fromIntegral x `shiftL` 32) .|. fromIntegral y :: Word64)
{-# INLINE wordsTo64Bit #-}

