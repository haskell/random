{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# OPTIONS_HADDOCK hide, not-home #-}
#include "MachDeps.h"

-- |
-- Module      :  System.Random.Internal
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE in the 'random' repository)
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
--
-- This library deals with the common task of pseudo-random number generation.
module System.Random.Internal
  (-- * Pure and monadic pseudo-random number generator interfaces
    RandomGen(..)
  , MonadRandom(..)

  -- ** Standard pseudo-random number generator
  , StdGen(..)
  , mkStdGen

  -- * Monadic adapters for pure pseudo-random number generators
  -- ** Pure adapter
  , StateGen(..)
  , StateGenM(..)
  , splitGen
  , runStateGen
  , runStateGen_
  , runStateGenT
  , runStateGenT_
  , runStateGenST

  -- * Pseudo-random values of various types
  , Uniform(..)
  , UniformRange(..)
  , uniformByteString

  -- * Generators for sequences of pseudo-random bytes
  , genShortByteStringIO
  , genShortByteStringST
  ) where

import Control.Arrow
import Control.DeepSeq (NFData)
import Control.Monad.IO.Class
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Control.Monad.State.Strict
import Data.Bits
import Data.ByteString.Builder.Prim (word64LE)
import Data.ByteString.Builder.Prim.Internal (runF)
import Data.ByteString.Internal (ByteString(PS))
import Data.ByteString.Short.Internal (ShortByteString(SBS), fromShort)
import Data.Int
import Data.Kind (Type)
import Data.Word
import Foreign.C.Types
import Foreign.Ptr (plusPtr)
import Foreign.Storable (pokeByteOff)
import GHC.Exts
import GHC.ForeignPtr
import GHC.IO (IO(..))
import GHC.Word
import Numeric.Natural (Natural)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Random.SplitMix as SM
import qualified System.Random.SplitMix32 as SM32

-- | 'RandomGen' is an interface to pure pseudo-random number generators.
--
-- 'StdGen' is the standard 'RandomGen' instance provided by this library.
{-# DEPRECATED next "No longer used" #-}
{-# DEPRECATED genRange "No longer used" #-}
class RandomGen g where
  {-# MINIMAL split,(genWord32|genWord64|(next,genRange)) #-}
  -- | Returns an 'Int' that is uniformly distributed over the range returned by
  -- 'genRange' (including both end points), and a new generator. Using 'next'
  -- is inefficient as all operations go via 'Integer'. See
  -- [here](https://alexey.kuleshevi.ch/blog/2019/12/21/random-benchmarks) for
  -- more details. It is thus deprecated.
  next :: g -> (Int, g)
  next g = runStateGen g (uniformRM (genRange g))

  -- | Returns a 'Word8' that is uniformly distributed over the entire 'Word8'
  -- range.
  --
  -- @since 1.2
  genWord8 :: g -> (Word8, g)
  genWord8 = first fromIntegral . genWord32

  -- | Returns a 'Word16' that is uniformly distributed over the entire 'Word16'
  -- range.
  --
  -- @since 1.2
  genWord16 :: g -> (Word16, g)
  genWord16 = first fromIntegral . genWord32

  -- | Returns a 'Word32' that is uniformly distributed over the entire 'Word32'
  -- range.
  --
  -- @since 1.2
  genWord32 :: g -> (Word32, g)
  genWord32 = randomIvalIntegral (minBound, maxBound)
  -- Once `next` is removed, this implementation should be used instead:
  -- first fromIntegral . genWord64

  -- | Returns a 'Word64' that is uniformly distributed over the entire 'Word64'
  -- range.
  --
  -- @since 1.2
  genWord64 :: g -> (Word64, g)
  genWord64 g =
    case genWord32 g of
      (l32, g') ->
        case genWord32 g' of
          (h32, g'') ->
            ((fromIntegral h32 `unsafeShiftL` 32) .|. fromIntegral l32, g'')

  -- | @genWord32R upperBound g@ returns a 'Word32' that is uniformly
  -- distributed over the range @[0, upperBound]@.
  --
  -- @since 1.2
  genWord32R :: Word32 -> g -> (Word32, g)
  genWord32R m g = runStateGen g (unbiasedWordMult32 m)

  -- | @genWord64R upperBound g@ returns a 'Word64' that is uniformly
  -- distributed over the range @[0, upperBound]@.
  --
  -- @since 1.2
  genWord64R :: Word64 -> g -> (Word64, g)
  genWord64R m g = runStateGen g (unsignedBitmaskWithRejectionM uniformWord64 m)

  -- | @genShortByteString n g@ returns a 'ShortByteString' of length @n@
  -- filled with pseudo-random bytes.
  --
  -- @since 1.2
  genShortByteString :: Int -> g -> (ShortByteString, g)
  genShortByteString n g =
    unsafePerformIO $ runStateGenT g (genShortByteStringIO n . uniformWord64)
  {-# INLINE genShortByteString #-}

  -- | Yields the range of values returned by 'next'.
  --
  -- It is required that:
  --
  -- *   If @(a, b) = 'genRange' g@, then @a < b@.
  -- *   'genRange' must not examine its argument so the value it returns is
  --     determined only by the instance of 'RandomGen'.
  --
  -- The default definition spans the full range of 'Int'.
  genRange :: g -> (Int, Int)
  genRange _ = (minBound, maxBound)

  -- | Returns two distinct pseudo-random number generators.
  --
  -- Implementations should take care to ensure that the resulting generators
  -- are not correlated. Some pseudo-random number generators are not
  -- splittable. In that case, the 'split' implementation should fail with a
  -- descriptive 'error' message.
  split :: g -> (g, g)


-- | 'MonadRandom' is an interface to monadic pseudo-random number generators.
class Monad m => MonadRandom g s m | g m -> s where
  -- | Represents the state of the pseudo-random number generator for use with
  -- 'thawGen' and 'freezeGen'.
  --
  -- @since 1.2
  type Frozen g = (f :: Type) | f -> g
  {-# MINIMAL freezeGen,thawGen,(uniformWord32|uniformWord64) #-}

  -- | Restores the pseudo-random number generator from its 'Frozen'
  -- representation.
  --
  -- @since 1.2
  thawGen :: Frozen g -> m (g s)

  -- | Saves the state of the pseudo-random number generator to its 'Frozen'
  -- representation.
  --
  -- @since 1.2
  freezeGen :: g s -> m (Frozen g)

  -- | @uniformWord32R upperBound g@ generates a 'Word32' that is uniformly
  -- distributed over the range @[0, upperBound]@.
  --
  -- @since 1.2
  uniformWord32R :: Word32 -> g s -> m Word32
  uniformWord32R = unsignedBitmaskWithRejectionM uniformWord32

  -- | @uniformWord64R upperBound g@ generates a 'Word64' that is uniformly
  -- distributed over the range @[0, upperBound]@.
  --
  -- @since 1.2
  uniformWord64R :: Word64 -> g s -> m Word64
  uniformWord64R = unsignedBitmaskWithRejectionM uniformWord64

  -- | Generates a 'Word8' that is uniformly distributed over the entire 'Word8'
  -- range.
  --
  -- The default implementation extracts a 'Word8' from 'uniformWord32'.
  --
  -- @since 1.2
  uniformWord8 :: g s -> m Word8
  uniformWord8 = fmap fromIntegral . uniformWord32

  -- | Generates a 'Word16' that is uniformly distributed over the entire
  -- 'Word16' range.
  --
  -- The default implementation extracts a 'Word16' from 'uniformWord32'.
  --
  -- @since 1.2
  uniformWord16 :: g s -> m Word16
  uniformWord16 = fmap fromIntegral . uniformWord32

  -- | Generates a 'Word32' that is uniformly distributed over the entire
  -- 'Word32' range.
  --
  -- The default implementation extracts a 'Word32' from 'uniformWord64'.
  --
  -- @since 1.2
  uniformWord32 :: g s -> m Word32
  uniformWord32 = fmap fromIntegral . uniformWord64

  -- | Generates a 'Word64' that is uniformly distributed over the entire
  -- 'Word64' range.
  --
  -- The default implementation combines two 'Word32' from 'uniformWord32' into
  -- one 'Word64'.
  --
  -- @since 1.2
  uniformWord64 :: g s -> m Word64
  uniformWord64 g = do
    l32 <- uniformWord32 g
    h32 <- uniformWord32 g
    pure (unsafeShiftL (fromIntegral h32) 32 .|. fromIntegral l32)

  -- | @uniformShortByteString n g@ generates a 'ShortByteString' of length @n@
  -- filled with pseudo-random bytes.
  --
  -- @since 1.2
  uniformShortByteString :: Int -> g s -> m ShortByteString
  default uniformShortByteString :: MonadIO m => Int -> g s -> m ShortByteString
  uniformShortByteString n = genShortByteStringIO n . uniformWord64
  {-# INLINE uniformShortByteString #-}



data MBA s = MBA (MutableByteArray# s)


-- | Efficiently generates a sequence of pseudo-random bytes in a platform
-- independent manner. The allocated memory is be pinned, so it is safe to use
-- with FFI calls.
--
-- @since 1.2
genShortByteStringIO :: MonadIO m => Int -> m Word64 -> m ShortByteString
genShortByteStringIO n0 gen64 = do
  let !n@(I# n#) = max 0 n0
      (n64, nrem64) = n `quotRem` 8
  MBA mba# <-
    liftIO $
    IO $ \s# ->
      case newPinnedByteArray# n# s# of
        (# s'#, mba# #) -> (# s'#, MBA mba# #)
  let go i ptr
        | i < n64 = do
          w64 <- gen64
          -- Writing 8 bytes at a time in a Little-endian order gives us
          -- platform portability
          liftIO $ runF word64LE w64 ptr
          go (i + 1) (ptr `plusPtr` 8)
        | otherwise = return ptr
  ptr <- go 0 (Ptr (byteArrayContents# (unsafeCoerce# mba#)))
  when (nrem64 > 0) $ do
    w64 <- gen64
    -- In order to not mess up the byte order we write generated Word64 into a
    -- temporary pointer and then copy only the missing bytes over to the array.
    -- It is tempting to simply generate as many bytes as we still need using
    -- smaller generators (eg. uniformWord8), but that would result in
    -- inconsistent tail when total length is slightly varied.
    liftIO $ do
      let goRem64 z i =
            when (i < nrem64) $ do
              pokeByteOff ptr i (fromIntegral z :: Word8)
              goRem64 (z `unsafeShiftR` 8) (i + 1)
      goRem64 w64 0
  liftIO $
    IO $ \s# ->
      case unsafeFreezeByteArray# mba# s# of
        (# s'#, ba# #) -> (# s'#, SBS ba# #)
{-# INLINE genShortByteStringIO #-}

-- | Same as 'genShortByteStringIO', but runs in 'ST'.
--
-- @since 1.2
genShortByteStringST :: Int -> ST s Word64 -> ST s ShortByteString
genShortByteStringST n action =
  unsafeIOToST (genShortByteStringIO n (unsafeSTToIO action))

pinnedByteArrayToByteString :: ByteArray# -> ByteString
pinnedByteArrayToByteString ba# =
  PS (pinnedByteArrayToForeignPtr ba#) 0 (I# (sizeofByteArray# ba#))
{-# INLINE pinnedByteArrayToByteString #-}

pinnedByteArrayToForeignPtr :: ByteArray# -> ForeignPtr a
pinnedByteArrayToForeignPtr ba# =
  ForeignPtr (byteArrayContents# ba#) (PlainPtr (unsafeCoerce# ba#))
{-# INLINE pinnedByteArrayToForeignPtr #-}


-- | Generates a pseudo-random 'ByteString' of the specified size.
--
-- @since 1.2
uniformByteString :: MonadRandom g s m => Int -> g s -> m ByteString
uniformByteString n g = do
  ba@(SBS ba#) <- uniformShortByteString n g
  pure $
    if isTrue# (isByteArrayPinned# ba#)
      then pinnedByteArrayToByteString ba#
      else fromShort ba
{-# INLINE uniformByteString #-}


-- | Opaque data type that carries the type of a pure pseudo-random number
-- generator.
--
-- @since 1.2
data StateGenM g s = StateGenM
newtype StateGen g = StateGen g

instance (RandomGen g, MonadState g m) => MonadRandom (StateGenM g) g m where
  type Frozen (StateGenM g) = StateGen g
  thawGen (StateGen g) = StateGenM <$ put g
  freezeGen _ = fmap StateGen get
  uniformWord32R r _ = state (genWord32R r)
  uniformWord64R r _ = state (genWord64R r)
  uniformWord8 _ = state genWord8
  uniformWord16 _ = state genWord16
  uniformWord32 _ = state genWord32
  uniformWord64 _ = state genWord64
  uniformShortByteString n _ = state (genShortByteString n)



-- | Splits a pseudo-random number generator into two. Updates the state with
-- one of the resulting generators and returns the other.
--
-- @since 1.2
splitGen :: (MonadState g m, RandomGen g) => m g
splitGen = state split

-- | Runs a monadic generating action in the `State` monad using a pure
-- pseudo-random number generator.
--
-- @since 1.2
runStateGen :: RandomGen g => g -> (StateGenM g g -> State g a) -> (a, g)
runStateGen g f = runState (f StateGenM) g

-- | Runs a monadic generating action in the `State` monad using a pure
-- pseudo-random number generator. Returns only the resulting pseudo-random
-- value.
--
-- @since 1.2
runStateGen_ :: RandomGen g => g -> (StateGenM g g -> State g a) -> a
runStateGen_ g = fst . runStateGen g

-- | Runs a monadic generating action in the `StateT` monad using a pure
-- pseudo-random number generator.
--
-- @since 1.2
runStateGenT :: RandomGen g => g -> (StateGenM g g -> StateT g m a) -> m (a, g)
runStateGenT g f = runStateT (f StateGenM) g

-- | Runs a monadic generating action in the `StateT` monad using a pure
-- pseudo-random number generator. Returns only the resulting pseudo-random
-- value.
--
-- @since 1.2
runStateGenT_ :: (RandomGen g, Functor f) => g -> (StateGenM g g -> StateT g f a) -> f a
runStateGenT_ g = fmap fst . runStateGenT g

-- | Runs a monadic generating action in the `ST` monad using a pure
-- pseudo-random number generator.
--
-- @since 1.2
runStateGenST :: RandomGen g => g -> (forall s . StateGenM g g -> StateT g (ST s) a) -> (a, g)
runStateGenST g action = runST $ runStateGenT g action
{-# INLINE runStateGenST #-}


-- | The standard pseudo-random number generator.
newtype StdGen = StdGen { unStdGen :: SM.SMGen }
  deriving newtype (RandomGen, NFData)
  deriving stock   (Show)

instance Eq StdGen where
  StdGen x1 == StdGen x2 = SM.unseedSMGen x1 == SM.unseedSMGen x2

instance RandomGen SM.SMGen where
  next = SM.nextInt
  genWord32 = SM.nextWord32
  genWord64 = SM.nextWord64
  split = SM.splitSMGen

instance RandomGen SM32.SMGen where
  next = SM32.nextInt
  genWord32 = SM32.nextWord32
  genWord64 = SM32.nextWord64
  split = SM32.splitSMGen

-- | Constructs a 'StdGen' deterministically.
mkStdGen :: Int -> StdGen
mkStdGen = StdGen . SM.mkSMGen . fromIntegral

-- | The class of types for which a uniformly distributed value can be drawn
-- from all possible values of the type.
--
-- @since 1.2
class Uniform a where
  -- | Generates a value uniformly distributed over all possible values of that
  -- type.
  --
  -- @since 1.2
  uniformM :: MonadRandom g s m => g s -> m a

-- | The class of types for which a uniformly distributed value can be drawn
-- from a range.
--
-- @since 1.2
class UniformRange a where
  -- | Generates a value uniformly distributed over the provided range, which
  -- is interpreted as inclusive in the lower and upper bound.
  --
  -- *   @uniformRM (1 :: Int, 4 :: Int)@ generates values uniformly from the
  --     set \(\{1,2,3,4\}\)
  --
  -- *   @uniformRM (1 :: Float, 4 :: Float)@ generates values uniformly from
  --     the set \(\{x\;|\;1 \le x \le 4\}\)
  --
  -- The following law should hold to make the function always defined:
  --
  -- > uniformRM (a, b) = uniformRM (b, a)
  --
  -- @since 1.2
  uniformRM :: MonadRandom g s m => (a, a) -> g s -> m a

instance UniformRange Integer where
  uniformRM = uniformIntegralM

instance UniformRange Natural where
  uniformRM = uniformIntegralM

instance Uniform Int8 where
  uniformM = fmap (fromIntegral :: Word8 -> Int8) . uniformWord8
instance UniformRange Int8 where
  uniformRM = signedBitmaskWithRejectionRM (fromIntegral :: Int8 -> Word8) fromIntegral

instance Uniform Int16 where
  uniformM = fmap (fromIntegral :: Word16 -> Int16) . uniformWord16
instance UniformRange Int16 where
  uniformRM = signedBitmaskWithRejectionRM (fromIntegral :: Int16 -> Word16) fromIntegral
  {-# INLINE uniformRM #-}

instance Uniform Int32 where
  uniformM = fmap (fromIntegral :: Word32 -> Int32) . uniformWord32
instance UniformRange Int32 where
  uniformRM = signedBitmaskWithRejectionRM (fromIntegral :: Int32 -> Word32) fromIntegral
  {-# INLINE uniformRM #-}

instance Uniform Int64 where
  uniformM = fmap (fromIntegral :: Word64 -> Int64) . uniformWord64
instance UniformRange Int64 where
  uniformRM = signedBitmaskWithRejectionRM (fromIntegral :: Int64 -> Word64) fromIntegral
  {-# INLINE uniformRM #-}

instance Uniform Int where
#if WORD_SIZE_IN_BITS < 64
  uniformM = fmap (fromIntegral :: Word32 -> Int) . uniformWord32
#else
  uniformM = fmap (fromIntegral :: Word64 -> Int) . uniformWord64
#endif
  {-# INLINE uniformM #-}
instance UniformRange Int where
  uniformRM = signedBitmaskWithRejectionRM (fromIntegral :: Int -> Word) fromIntegral
  {-# INLINE uniformRM #-}

instance Uniform Word where
#if WORD_SIZE_IN_BITS < 64
  uniformM = fmap (fromIntegral :: Word32 -> Word) . uniformWord32
#else
  uniformM = fmap (fromIntegral :: Word64 -> Word) . uniformWord64
#endif
instance UniformRange Word where
  {-# INLINE uniformRM #-}
  uniformRM = unsignedBitmaskWithRejectionRM

instance Uniform Word8 where
  {-# INLINE uniformM #-}
  uniformM = uniformWord8
instance UniformRange Word8 where
  {-# INLINE uniformRM #-}
  uniformRM = unbiasedWordMult32RM

instance Uniform Word16 where
  {-# INLINE uniformM #-}
  uniformM = uniformWord16
instance UniformRange Word16 where
  {-# INLINE uniformRM #-}
  uniformRM = unbiasedWordMult32RM

instance Uniform Word32 where
  {-# INLINE uniformM #-}
  uniformM  = uniformWord32
instance UniformRange Word32 where
  {-# INLINE uniformRM #-}
  uniformRM = unbiasedWordMult32RM

instance Uniform Word64 where
  {-# INLINE uniformM #-}
  uniformM  = uniformWord64
instance UniformRange Word64 where
  {-# INLINE uniformRM #-}
  uniformRM = unsignedBitmaskWithRejectionRM

instance Uniform CBool where
  uniformM = fmap CBool . uniformM
instance UniformRange CBool where
  uniformRM (CBool b, CBool t) = fmap CBool . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CChar where
  uniformM = fmap CChar . uniformM
instance UniformRange CChar where
  uniformRM (CChar b, CChar t) = fmap CChar . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CSChar where
  uniformM = fmap CSChar . uniformM
instance UniformRange CSChar where
  uniformRM (CSChar b, CSChar t) = fmap CSChar . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CUChar where
  uniformM = fmap CUChar . uniformM
instance UniformRange CUChar where
  uniformRM (CUChar b, CUChar t) = fmap CUChar . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CShort where
  uniformM = fmap CShort . uniformM
instance UniformRange CShort where
  uniformRM (CShort b, CShort t) = fmap CShort . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CUShort where
  uniformM = fmap CUShort . uniformM
instance UniformRange CUShort where
  uniformRM (CUShort b, CUShort t) = fmap CUShort . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CInt where
  uniformM = fmap CInt . uniformM
instance UniformRange CInt where
  uniformRM (CInt b, CInt t) = fmap CInt . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CUInt where
  uniformM = fmap CUInt . uniformM
instance UniformRange CUInt where
  uniformRM (CUInt b, CUInt t) = fmap CUInt . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CLong where
  uniformM = fmap CLong . uniformM
instance UniformRange CLong where
  uniformRM (CLong b, CLong t) = fmap CLong . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CULong where
  uniformM = fmap CULong . uniformM
instance UniformRange CULong where
  uniformRM (CULong b, CULong t) = fmap CULong . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CPtrdiff where
  uniformM = fmap CPtrdiff . uniformM
instance UniformRange CPtrdiff where
  uniformRM (CPtrdiff b, CPtrdiff t) = fmap CPtrdiff . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CSize where
  uniformM = fmap CSize . uniformM
instance UniformRange CSize where
  uniformRM (CSize b, CSize t) = fmap CSize . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CWchar where
  uniformM = fmap CWchar . uniformM
instance UniformRange CWchar where
  uniformRM (CWchar b, CWchar t) = fmap CWchar . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CSigAtomic where
  uniformM = fmap CSigAtomic . uniformM
instance UniformRange CSigAtomic where
  uniformRM (CSigAtomic b, CSigAtomic t) = fmap CSigAtomic . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CLLong where
  uniformM = fmap CLLong . uniformM
instance UniformRange CLLong where
  uniformRM (CLLong b, CLLong t) = fmap CLLong . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CULLong where
  uniformM = fmap CULLong . uniformM
instance UniformRange CULLong where
  uniformRM (CULLong b, CULLong t) = fmap CULLong . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CIntPtr where
  uniformM                         = fmap CIntPtr . uniformM
instance UniformRange CIntPtr where
  uniformRM (CIntPtr b, CIntPtr t) = fmap CIntPtr . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CUIntPtr where
  uniformM = fmap CUIntPtr . uniformM
instance UniformRange CUIntPtr where
  uniformRM (CUIntPtr b, CUIntPtr t) = fmap CUIntPtr . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CIntMax where
  uniformM = fmap CIntMax . uniformM
instance UniformRange CIntMax where
  uniformRM (CIntMax b, CIntMax t) = fmap CIntMax . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CUIntMax where
  uniformM = fmap CUIntMax . uniformM
instance UniformRange CUIntMax where
  uniformRM (CUIntMax b, CUIntMax t) = fmap CUIntMax . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance UniformRange CFloat where
  uniformRM (CFloat l, CFloat h) = fmap CFloat . uniformRM (l, h)
  {-# INLINE uniformRM #-}

instance UniformRange CDouble where
  uniformRM (CDouble l, CDouble h) = fmap CDouble . uniformRM (l, h)
  {-# INLINE uniformRM #-}


-- The `chr#` and `ord#` are the prim functions that will be called, regardless of which
-- way you gonna do the `Char` conversion, so it is better to call them directly and
-- bypass all the hoops. Also because `intToChar` and `charToInt` are internal functions
-- and are called on valid character ranges it is impossible to generate an invalid
-- `Char`, therefore it is totally fine to omit all the unnecessary checks involved in
-- other paths of conversion.
word32ToChar :: Word32 -> Char
word32ToChar (W32# w#) = C# (chr# (word2Int# w#))
{-# INLINE word32ToChar #-}

charToWord32 :: Char -> Word32
charToWord32 (C# c#) = W32# (int2Word# (ord# c#))
{-# INLINE charToWord32 #-}

instance Uniform Char where
  uniformM g = word32ToChar <$> unbiasedWordMult32 (charToWord32 maxBound) g
  {-# INLINE uniformM #-}
instance UniformRange Char where
  uniformRM (l, h) g =
    word32ToChar <$> unbiasedWordMult32RM (charToWord32 l, charToWord32 h) g
  {-# INLINE uniformRM #-}

instance Uniform Bool where
  uniformM = fmap wordToBool . uniformWord8
    where wordToBool w = (w .&. 1) /= 0
instance UniformRange Bool where
  uniformRM (False, False) _g = return False
  uniformRM (True, True)   _g = return True
  uniformRM _               g = uniformM g

-- | See /Floating point number caveats/ in "System.Random.Monad".
instance UniformRange Double where
  uniformRM (l, h) g = do
    w64 <- uniformWord64 g
    let x = word64ToDoubleInUnitInterval w64
    return $ (h - l) * x + l

-- | Turns a given uniformly distributed 'Word64' value into a uniformly
-- distributed 'Double' value in the range [0, 1].
word64ToDoubleInUnitInterval :: Word64 -> Double
word64ToDoubleInUnitInterval w64 = d / m
  where
    d = fromIntegral w64 :: Double
    m = fromIntegral (maxBound :: Word64) :: Double
{-# INLINE word64ToDoubleInUnitInterval #-}

-- | See /Floating point number caveats/ in "System.Random.Monad".
instance UniformRange Float where
  uniformRM (l, h) g = do
    w32 <- uniformWord32 g
    let x = word32ToFloatInUnitInterval w32
    return $ (h - l) * x + l

-- | Turns a given uniformly distributed 'Word32' value into a uniformly
-- distributed 'Float' value in the range [0,1].
word32ToFloatInUnitInterval :: Word32 -> Float
word32ToFloatInUnitInterval w32 = f / m
  where
    f = fromIntegral w32 :: Float
    m = fromIntegral (maxBound :: Word32) :: Float
{-# INLINE word32ToFloatInUnitInterval #-}

-- The two integer functions below take an [inclusive,inclusive] range.
randomIvalIntegral :: (RandomGen g, Integral a) => (a, a) -> g -> (a, g)
randomIvalIntegral (l,h) = randomIvalInteger (toInteger l, toInteger h)

{-# SPECIALIZE randomIvalInteger :: (Num a) =>
    (Integer, Integer) -> StdGen -> (a, StdGen) #-}

randomIvalInteger :: (RandomGen g, Num a) => (Integer, Integer) -> g -> (a, g)
randomIvalInteger (l,h) rng
 | l > h     = randomIvalInteger (h,l) rng
 | otherwise = case f 1 0 rng of (v, rng') -> (fromInteger (l + v `mod` k), rng')
     where
       (genlo, genhi) = genRange rng
       b = fromIntegral genhi - fromIntegral genlo + 1

       -- Probabilities of the most likely and least likely result
       -- will differ at most by a factor of (1 +- 1/q). Assuming the RandomGen
       -- is uniform, of course

       -- On average, log q / log b more pseudo-random values will be generated
       -- than the minimum
       q = 1000 :: Integer
       k = h - l + 1
       magtgt = k * q

       -- generate pseudo-random values until we exceed the target magnitude
       f mag v g | mag >= magtgt = (v, g)
                 | otherwise = v' `seq`f (mag*b) v' g' where
                        (x,g') = next g
                        v' = v * b + (fromIntegral x - fromIntegral genlo)

-- | Generate an integral in the range @[l, h]@ if @l <= h@ and @[h, l]@
-- otherwise.
uniformIntegralM :: (Bits a, Integral a, MonadRandom g s m) => (a, a) -> g s -> m a
uniformIntegralM (l, h) gen = case l `compare` h of
  LT -> do
    let limit = h - l
    let limitAsWord64 :: Word64 = fromIntegral limit
    bounded <-
      if fromIntegral limitAsWord64 == limit
        -- Optimisation: if 'limit' fits into 'Word64', generate a bounded
        -- 'Word64' and then convert to 'Integer'
        then fromIntegral <$> unsignedBitmaskWithRejectionM uniformWord64 limitAsWord64 gen
        else boundedExclusiveIntegralM (limit + 1) gen
    return $ l + bounded
  GT -> uniformIntegralM (h, l) gen
  EQ -> pure l
{-# INLINEABLE uniformIntegralM #-}

-- | Generate an integral in the range @[0, s)@ using a variant of Lemire's
-- multiplication method.
--
-- Daniel Lemire. 2019. Fast Random Integer Generation in an Interval. In ACM
-- Transactions on Modeling and Computer Simulation
-- https://doi.org/10.1145/3230636
--
-- PRECONDITION (unchecked): s > 0
boundedExclusiveIntegralM :: (Bits a, Integral a, MonadRandom g s m) => a -> g s -> m a
boundedExclusiveIntegralM (s :: a) gen = go
  where
    n = integralWordSize s
    -- We renamed 'L' from the paper to 'k' here because 'L' is not a valid
    -- variable name in Haskell and 'l' is already used in the algorithm.
    k = WORD_SIZE_IN_BITS * n
    twoToK = (1::a) `shiftL` k
    modTwoToKMask = twoToK - 1

    t = (twoToK - s) `mod` s
    go = do
      x <- uniformIntegralWords n gen
      let m = x * s
      -- m .&. modTwoToKMask == m `mod` twoToK
      let l = m .&. modTwoToKMask
      if l < t
        then go
        -- m `shiftR` k == m `quot` twoToK
        else return $ m `shiftR` k
{-# INLINE boundedExclusiveIntegralM #-}

-- | @integralWordSize i@ returns that least @w@ such that
-- @i <= WORD_SIZE_IN_BITS^w@.
integralWordSize :: (Bits a, Num a) => a -> Int
integralWordSize = go 0
  where
    go !acc i
      | i == 0 = acc
      | otherwise = go (acc + 1) (i `shiftR` WORD_SIZE_IN_BITS)
{-# INLINE integralWordSize #-}

-- | @uniformIntegralWords n@ is a uniformly pseudo-random integral in the range
-- @[0, WORD_SIZE_IN_BITS^n)@.
uniformIntegralWords :: (Bits a, Integral a, MonadRandom g s m) => Int -> g s -> m a
uniformIntegralWords n gen = go 0 n
  where
    go !acc i
      | i == 0 = return acc
      | otherwise = do
        (w :: Word) <- uniformM gen
        go ((acc `shiftL` WORD_SIZE_IN_BITS) .|. fromIntegral w) (i - 1)
{-# INLINE uniformIntegralWords #-}

-- | Uniformly generate an 'Integral' in an inclusive-inclusive range.
--
-- Only use for integrals size less than or equal to that of 'Word32'.
unbiasedWordMult32RM :: (MonadRandom g s m, Integral a) => (a, a) -> g s -> m a
unbiasedWordMult32RM (b, t) g
  | b <= t    = (+b) . fromIntegral <$> unbiasedWordMult32 (fromIntegral (t - b)) g
  | otherwise = (+t) . fromIntegral <$> unbiasedWordMult32 (fromIntegral (b - t)) g
{-# SPECIALIZE unbiasedWordMult32RM :: MonadRandom g s m => (Word8, Word8) -> g s -> m Word8 #-}

-- | Uniformly generate Word32 in @[0, s]@.
unbiasedWordMult32 :: MonadRandom g s m => Word32 -> g s -> m Word32
unbiasedWordMult32 s g
  | s == maxBound = uniformWord32 g
  | otherwise = unbiasedWordMult32Exclusive (s+1) g
{-# INLINE unbiasedWordMult32 #-}

-- | See [Lemire's paper](https://arxiv.org/pdf/1805.10941.pdf),
-- [O\'Neill's
-- blogpost](https://www.pcg-random.org/posts/bounded-rands.html) and
-- more directly [O\'Neill's github
-- repo](https://github.com/imneme/bounded-rands/blob/3d71f53c975b1e5b29f2f3b05a74e26dab9c3d84/bounded32.cpp#L234).
-- N.B. The range is [0,t) **not** [0,t].
unbiasedWordMult32Exclusive  :: MonadRandom g s m => Word32 -> g s -> m Word32
unbiasedWordMult32Exclusive r g = go
  where
    t :: Word32
    t = (-r) `mod` r -- Calculates 2^32 `mod` r!!!
    go = do
      x <- uniformWord32 g
      let m :: Word64
          m = fromIntegral x * fromIntegral r
          l :: Word32
          l = fromIntegral m
      if l >= t then return (fromIntegral $ m `shiftR` 32) else go

-- | This only works for unsigned integrals
unsignedBitmaskWithRejectionRM ::
     (MonadRandom g s m, FiniteBits a, Num a, Ord a, Uniform a)
  => (a, a)
  -> g s
  -> m a
unsignedBitmaskWithRejectionRM (bottom, top) gen
  | bottom == top = pure top
  | otherwise = (b +) <$> unsignedBitmaskWithRejectionM uniformM r gen
  where
    (b, r) = if bottom > top then (top, bottom - top) else (bottom, top - bottom)
{-# INLINE unsignedBitmaskWithRejectionRM #-}

-- | This works for signed integrals by explicit conversion to unsigned and abusing overflow
signedBitmaskWithRejectionRM ::
     (Num a, Num b, Ord b, Ord a, FiniteBits a, MonadRandom g s f, Uniform a)
  => (b -> a)
  -> (a -> b)
  -> (b, b)
  -> g s
  -> f b
signedBitmaskWithRejectionRM toUnsigned fromUnsigned (bottom, top) gen
  | bottom == top = pure top
  | otherwise =
    (b +) . fromUnsigned <$> unsignedBitmaskWithRejectionM uniformM r gen
    -- This works in all cases, see Appendix 1 at the end of the file.
  where
    (b, r) =
      if bottom > top
        then (top, toUnsigned bottom - toUnsigned top)
        else (bottom, toUnsigned top - toUnsigned bottom)
{-# INLINE signedBitmaskWithRejectionRM #-}

unsignedBitmaskWithRejectionM :: (Ord a, FiniteBits a, Num a, MonadRandom g s m) => (g s -> m a) -> a -> g s -> m a
unsignedBitmaskWithRejectionM genUniformM range gen = go
  where
    mask = complement zeroBits `shiftR` countLeadingZeros (range .|. 1)
    go = do
      x <- genUniformM gen
      let x' = x .&. mask
      if x' > range
        then go
        else pure x'
{-# INLINE unsignedBitmaskWithRejectionM #-}

-------------------------------------------------------------------------------
-- 'Uniform' instances for tuples
-------------------------------------------------------------------------------

instance (Uniform a, Uniform b) => Uniform (a, b) where
  uniformM g = (,) <$> uniformM g <*> uniformM g

instance (Uniform a, Uniform b, Uniform c) => Uniform (a, b, c) where
  uniformM g = (,,) <$> uniformM g <*> uniformM g <*> uniformM g

instance (Uniform a, Uniform b, Uniform c, Uniform d) => Uniform (a, b, c, d) where
  uniformM g = (,,,) <$> uniformM g <*> uniformM g <*> uniformM g <*> uniformM g

instance (Uniform a, Uniform b, Uniform c, Uniform d, Uniform e) => Uniform (a, b, c, d, e) where
  uniformM g = (,,,,) <$> uniformM g <*> uniformM g <*> uniformM g <*> uniformM g <*> uniformM g

instance (Uniform a, Uniform b, Uniform c, Uniform d, Uniform e, Uniform f) => Uniform (a, b, c, d, e, f) where
  uniformM g = (,,,,,) <$> uniformM g <*> uniformM g <*> uniformM g <*> uniformM g <*> uniformM g <*> uniformM g

instance (Uniform a, Uniform b, Uniform c, Uniform d, Uniform e, Uniform f, Uniform g) => Uniform (a, b, c, d, e, f, g) where
  uniformM g = (,,,,,,) <$> uniformM g <*> uniformM g <*> uniformM g <*> uniformM g <*> uniformM g <*> uniformM g <*> uniformM g

-- Appendix 1.
--
-- @top@ and @bottom@ are signed integers of bit width @n@. @toUnsigned@
-- converts a signed integer to an unsigned number of the same bit width @n@.
--
--     range = toUnsigned top - toUnsigned bottom
--
-- This works out correctly thanks to modular arithmetic. Conceptually,
--
--     toUnsigned x | x >= 0 = x
--     toUnsigned x | x <  0 = 2^n + x
--
-- The following combinations are possible:
--
-- 1. @bottom >= 0@ and @top >= 0@
-- 2. @bottom < 0@ and @top >= 0@
-- 3. @bottom < 0@ and @top < 0@
--
-- Note that @bottom >= 0@ and @top < 0@ is impossible because of the
-- invariant @bottom < top@.
--
-- For any signed integer @i@ of width @n@, we have:
--
--     -2^(n-1) <= i <= 2^(n-1) - 1
--
-- Considering each combination in turn, we have
--
-- 1. @bottom >= 0@ and @top >= 0@
--
--     range = (toUnsigned top - toUnsigned bottom) `mod` 2^n
--                 --^ top    >= 0, so toUnsigned top    == top
--                 --^ bottom >= 0, so toUnsigned bottom == bottom
--           = (top - bottom) `mod` 2^n
--                 --^ top <= 2^(n-1) - 1 and bottom >= 0
--                 --^ top - bottom <= 2^(n-1) - 1
--                 --^ 0 < top - bottom <= 2^(n-1) - 1
--           = top - bottom
--
-- 2. @bottom < 0@ and @top >= 0@
--
--     range = (toUnsigned top - toUnsigned bottom) `mod` 2^n
--                 --^ top    >= 0, so toUnsigned top    == top
--                 --^ bottom <  0, so toUnsigned bottom == 2^n + bottom
--           = (top - (2^n + bottom)) `mod` 2^n
--                 --^ summand -2^n cancels out in calculation modulo 2^n
--           = (top - bottom) `mod` 2^n
--                 --^ top <= 2^(n-1) - 1 and bottom >= -2^(n-1)
--                 --^ top - bottom <= (2^(n-1) - 1) - (-2^(n-1)) = 2^n - 1
--                 --^ 0 < top - bottom <= 2^n - 1
--           = top - bottom
--
-- 3. @bottom < 0@ and @top < 0@
--
--     range = (toUnsigned top - toUnsigned bottom) `mod` 2^n
--                 --^ top    < 0, so toUnsigned top    == 2^n + top
--                 --^ bottom < 0, so toUnsigned bottom == 2^n + bottom
--           = ((2^n + top) - (2^n + bottom)) `mod` 2^n
--                 --^ summand 2^n cancels out in calculation modulo 2^n
--           = (top - bottom) `mod` 2^n
--                 --^ top <= -1
--                 --^ bottom >= -2^(n-1)
--                 --^ top - bottom <= -1 - (-2^(n-1)) = 2^(n-1) - 1
--                 --^ 0 < top - bottom <= 2^(n-1) - 1
--           = top - bottom
