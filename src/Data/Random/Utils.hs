{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP
           , GHCForeignImportPrim
           , MagicHash
           , UnboxedTuples
           , UnliftedFFITypes
  #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

#include "MachDeps.h"
module Data.Random.Utils(
    castWord64ToDouble
    ,castDoubleToWord64
    ,castWord32ToFloat
    ,castFloatToWord32
    ,RandomCastIEEE(..)) where
import GHC.Word(Word32(..),Word64(..))
import GHC.Prim (Word#,Float#,Double#)
import GHC.Types
{-
from commit
aa206346e6f12c9f88fdf051185741761ea88fbb
of the ghc git repo, due for inclusion in ghc 8.4

this should be move out of random into its own micro package for pre ghc 8.4 compat
with conversion facilities  in ghc >= 8.4

this copy has name mangling at the CMM layer for happy linking
plus Random prefixing the class so it should be low headache
-}




class RandomCastIEEE word ieee | word -> ieee , ieee -> word where
    toIEEE :: word -> ieee
    fromIEEE :: ieee -> word

instance RandomCastIEEE Word32 Float where
  {-# INLINE toIEEE #-}
  {-# INLINE fromIEEE #-}
  toIEEE = castWord32ToFloat
  fromIEEE = castFloatToWord32
instance RandomCastIEEE Word64 Double where
  {-# INLINE toIEEE #-}
  {-# INLINE fromIEEE #-}
  toIEEE = castWord64ToDouble
  fromIEEE = castDoubleToWord64




{-
Note [Casting from integral to floating point types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To implement something like `reinterpret_cast` from C++ to go from a
floating-point type to an integral type one might niavely think that the
following should work:
      cast :: Float -> Word32
      cast (F# f#) = W32# (unsafeCoerce# f#)
Unfortunately that is not the case, because all the `unsafeCoerce#` does is tell
the compiler that the types have changed. When one does the above cast and
tries to operate on the resulting `Word32` the code generator will generate code
that performs an integer/word operation on a floating-point register, which
results in a compile error.
The correct way of implementing `reinterpret_cast` to implement a primpop, but
that requires a unique implementation for all supported archetectures. The next
best solution is to write the value from the source register to memory and then
read it from memory into the destination register and the best way to do that
is using CMM.
-}






-- | @'castWord32ToFloat' w@ does a bit-for-bit copy from an integral value
-- to a floating-point value.
--
-- @since 4.10.0.0

{-# INLINE castWord32ToFloat #-}
castWord32ToFloat :: Word32 -> Float
castWord32ToFloat (W32# w#) = F# (stgWord32ToFloat w#)

foreign import prim "cts_random_stg_word32ToFloatzhPrivate"
    stgWord32ToFloat :: Word# -> Float#


-- | @'castFloatToWord32' f@ does a bit-for-bit copy from a floating-point value
-- to an integral value.
--
-- @since 4.10.0.0

{-# INLINE castFloatToWord32 #-}
castFloatToWord32 :: Float -> Word32
castFloatToWord32 (F# f#) = W32# (stgFloatToWord32 f#)

foreign import prim "cts_random_stg_floatToWord32zhPrivate"
    stgFloatToWord32 :: Float# -> Word#



-- | @'castWord64ToDouble' w@ does a bit-for-bit copy from an integral value
-- to a floating-point value.
--
-- @since 4.10.0.0

{-# INLINE castWord64ToDouble #-}
castWord64ToDouble :: Word64 -> Double
castWord64ToDouble (W64# w) = D# (stgWord64ToDouble w)

foreign import prim "cts_random_stg_word64ToDoublezhPrivate"
#if WORD_SIZE_IN_BITS == 64
    stgWord64ToDouble :: Word# -> Double#
#else
    stgWord64ToDouble :: Word64# -> Double#
#endif


-- | @'castFloatToWord32' f@ does a bit-for-bit copy from a floating-point value
-- to an integral value.
--
-- @since 4.10.0.0

{-# INLINE castDoubleToWord64 #-}
castDoubleToWord64 :: Double -> Word64
castDoubleToWord64 (D# d#) = W64# (stgDoubleToWord64 d#)

foreign import prim "cts_random_stg_doubleToWord64zhPrivate"
#if WORD_SIZE_IN_BITS == 64
    stgDoubleToWord64 :: Double# -> Word#
#else
    stgDoubleToWord64 :: Double# -> Word64#
#endif
