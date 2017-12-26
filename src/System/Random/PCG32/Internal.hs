{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
-- |
--
-- Standard PCG32 Random Number Generator with chosen streams, written in
-- pure haskell. See <http://www.pcg-random.org> for details.
--
module System.Random.PCG32.Internal
  ( -- * PCG 32
    PCG32 (..)
  , seed
  , newPCG32

    -- * Generating random numbers
  , next32
  , next64
  , bounded

    -- * Generator utilities
  , advancePCG32
  , split
  ) where

import Data.Bits
import Data.Data
import Foreign
import GHC.Generics

-- | The multiple sequence varient of the pcg random number generator.
data PCG32 = PCG32
  {-# UNPACK #-} !Word64 -- state
  {-# UNPACK #-} !Word64 -- inc
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

-- | Create a new generator from two words.
newPCG32 :: Word64 -> Word64 -> PCG32
newPCG32 = \a b ->
  let s = state (PCG32 (a + i) i)
      i = (b `shiftL` 1) .|. 1
  in  PCG32 s i
{-# INLINE newPCG32 #-}

-- | Fixed seed.
seed :: PCG32
seed = PCG32 9600629759793949339 15726070495360670683
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
state :: PCG32 -> Word64
state (PCG32 s inc) = s * multiplier + inc
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
pair :: PCG32 -> Pair
pair g@(PCG32 s _) = Pair (state g) (output s)
{-# INLINE pair #-}

-- | Return a random 'Word32' with a generator incremented by one step.
next32 :: PCG32 -> (Word32, PCG32)
next32 = \g@(PCG32 _ inc) ->
  let Pair s' r = pair g
  in  (r, PCG32 s' inc)
{-# INLINE next32 #-}

-- | Return a random 'Word64' with the generator incremented by two steps.
next64 :: PCG32 -> (Word64, PCG32)
next64 = \(PCG32 s inc) ->
  let Pair s'  w1 = pair (PCG32 s inc)
      Pair s'' w2 = pair (PCG32 s' inc)
  in  (wordsTo64Bit w1 w2, PCG32 s'' inc)
{-# INLINE next64 #-}

bounded' :: Word32 -> PCG32 -> Pair
bounded' b (PCG32 s0 inc) = go s0
  where
   t = negate b `mod` b
   go !s | r >= t    = Pair s' (r `mod` b)
         | otherwise = go s'
     where Pair s' r = pair (PCG32 s inc)
{-# INLINE bounded' #-}

-- | Generate a uniform 'Word32' less than the bound. If the bound is
--   zero, this throws a divide by zero exception.
bounded :: Word32 -> PCG32 -> (Word32, PCG32)
bounded = \b g@(PCG32 _ inc) ->
  let !(Pair s1 w) = bounded' b g
  in  (w, PCG32 s1 inc)
{-# INLINE bounded #-}

-- Utilities -----------------------------------------------------------

-- | Split the generator @g@ into @(g', g2)@ where @g'@ is @g@
--   incremented by 4 steps and @g2@ is a new generator with a
--   difference sequence to @g@.
split :: PCG32 -> (PCG32, PCG32)
split (PCG32 s inc) = (PCG32 s4 inc, mk w1 w2 w3 w4)
  where
    -- This is just something I made up. It passed big crunch and
    -- dieharder (by splitting every step) but there's probably a better
    -- way.
    mk a b c d = newPCG32 (wordsTo64Bit a b) (wordsTo64Bit c d)
    Pair s1 w1 = pair (PCG32 s  inc)
    Pair s2 w2 = pair (PCG32 s1 inc)
    Pair s3 w3 = pair (PCG32 s2 inc)
    Pair s4 w4 = pair (PCG32 s3 inc)
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
advancePCG32 :: Word64 -> PCG32 -> PCG32
advancePCG32 = \d (PCG32 s inc) -> PCG32 (advancing d s multiplier inc) inc
{-# INLINE advancePCG32 #-}

-- Misc ----------------------------------------------------------------

wordsTo64Bit :: Integral a => Word32 -> Word32 -> a
wordsTo64Bit x y =
    fromIntegral ((fromIntegral x `shiftL` 32) .|. fromIntegral y :: Word64)
{-# INLINE wordsTo64Bit #-}

