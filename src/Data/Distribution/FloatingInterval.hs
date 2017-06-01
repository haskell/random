
{- | This module provides both complete and fast (sloppy)
random samples for the unit interval [+0,1)


complete in this context means all representable normalized floats are reachable

fast means
-}

{-# LANGUAGE ScopedTypeVariables #-}
module Data.Distribution.FloatingInterval where

import Data.Bits
import Data.Word
import Data.Random.Utils

{-
for more info, read the IEEE 2008 Floating point spec or wikipedia.
single precision float is also called binary32,
double precision is also called binary64

the greatest negative exponent for double precision normalized floats is -1023, and
    53 bits after the implicit MSB of the significand
for single precision its -126,
  and

in both cases, for normalized form (exponent != 0, theres an implicit leading 1)


likewise, the exponent component of a floating point number is "biased"
in the sense of being an unsigned integer, with an additive positive correction factor
to map the actual exponent to its representation. To map from representation to value,
subtract the bias constant

binary32 bias == 127
binary64 bias == 1023
-}


{- |  sampleUnitIntervalDoubleM uniformly samples over the [+0,1) interval  of
  representable floating point numbers

  TODO: so for now the
-}
sampleUnitIntervalDoubleM :: forall m . Monad m => (m Word64) -> m Double
sampleUnitIntervalDoubleM mword = error "finish this" $ mword  {-computeMantissa
  where
    computeMantissa :: m Double
    computeMantissa = do
        wd <- mword
        leading <- return $ countLeadingZeros wd
        if leading == 64
          then computeMoreMantissa 64
        else
          computeNormalizedSignificandWith leading wd

    computeNormalizedSignificandWith:: Int -> Word64 -> m Double
    computeNormalizedSignificandWith leadingZeros rawWord =
        error "finish me"  mkUnitDouble  leadingZeros rawWord
    computeMoreMantissa :: Int  -> m Double
    computeMoreMantissa = error "finish this too"
    --- mkDouble takes the positive version of the negative exponent
    --- and the normalized significand (which ellides the leading 1 digit)
    mkUnitDouble :: Word64 -> Word64 -> Double
    mkUnitDouble negUnBiasedExponent normalSignificand = toIEEE $ undefined (negUnBiasedExponent )
-}

{- | sampleUnitIntervalDoubleReallySloppyM, using the same algorithm as in
http://xoroshiro.di.unimi.it/#remarks, which is also used by the rand package
in rust. It has issues, but its super fast. Generates all the representable floats
the correspond to dyadic (binary) rationals of the form k/2^{−53}. Note that
the lowest order bit will 0. Which is why the lowest order bit  of the random word
is then xor'd against the corrected unit interval number in this specific


extracted docs from the original site:
"""
   #include <stdint.h>

    (x >> 11) * (1. / (UINT64_C(1) << 53))
This conversion guarantees that all dyadic rationals of the form k / 2^53 will be equally likely. Note that this conversion prefers the high bits of x, but you can alternatively use the lowest bits.

An alternative, faster multiplication-free operation is

    #include <stdint.h>

    static inline double to_double(uint64_t x) {
       const union { uint64_t i; double d; } u = { .i = UINT64_C(0x3FF) << 52 | x >> 12 };
       return u.d - 1.0;
    }
The code above cooks up by bit manipulation a real number in the interval [1..2), and then subtracts one to obtain a real number in the interval [0..1). If x is chosen uniformly among 64-bit integers, d is chosen uniformly among dyadic rationals of the form k / 2−52.
"""


 -}
sampleUnitIntervalDoubleReallySloppyM ::  forall m . Monad m => (m Word64) -> m Double
sampleUnitIntervalDoubleReallySloppyM mword = do
        word <- mword
        return $ toIEEE $ (\x -> (  x `xor` (1 .&. word) )) $ fromIEEE $
          toIEEE (0x3FF `unsafeShiftL` 52 .|. (word `unsafeShiftR` 12)) - 1
