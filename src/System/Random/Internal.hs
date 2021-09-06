{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedFFITypes #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TypeFamilyDependencies #-}
#else
{-# LANGUAGE TypeFamilies #-}
#endif
{-# OPTIONS_HADDOCK hide, not-home #-}

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
  , StatefulGen(..)
  , FrozenGen(..)

  -- ** Standard pseudo-random number generator
  , StdGen(..)
  , mkStdGen
  , theStdGen

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
  , runStateGenST_

  -- * Pseudo-random values of various types
  , Uniform(..)
  , uniformViaFiniteM
  , UniformRange(..)
  , uniformByteStringM
  , uniformDouble01M
  , uniformDoublePositive01M
  , uniformFloat01M
  , uniformFloatPositive01M
  , uniformEnumM
  , uniformEnumRM

  -- * Generators for sequences of pseudo-random bytes
  , genShortByteStringIO
  , genShortByteStringST
  ) where

import Control.Arrow
import Control.DeepSeq (NFData)
import Control.Monad (when)
import Control.Monad.Cont (ContT, runContT)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Control.Monad.State.Strict (MonadState(..), State, StateT(..), runState)
import Control.Monad.Trans (lift)
import Data.Bits
import Data.ByteString.Short.Internal (ShortByteString(SBS), fromShort)
import Data.IORef (IORef, newIORef)
import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Storable (Storable)
import GHC.Exts
import GHC.Generics
import GHC.IO (IO(..))
import GHC.Word
import Numeric.Natural (Natural)
import System.IO.Unsafe (unsafePerformIO)
import System.Random.GFinite (Cardinality(..), GFinite(..))
import qualified System.Random.SplitMix as SM
import qualified System.Random.SplitMix32 as SM32
#if __GLASGOW_HASKELL__ >= 800
import Data.Kind
#endif
#if __GLASGOW_HASKELL__ >= 802
import Data.ByteString.Internal (ByteString(PS))
import GHC.ForeignPtr
#else
import Data.ByteString (ByteString)
#endif

-- Needed for WORDS_BIGENDIAN
#include "MachDeps.h"


-- | 'RandomGen' is an interface to pure pseudo-random number generators.
--
-- 'StdGen' is the standard 'RandomGen' instance provided by this library.
--
-- @since 1.0.0
{-# DEPRECATED next "No longer used" #-}
{-# DEPRECATED genRange "No longer used" #-}
class RandomGen g where
  {-# MINIMAL split,(genWord32|genWord64|(next,genRange)) #-}
  -- | Returns an 'Int' that is uniformly distributed over the range returned by
  -- 'genRange' (including both end points), and a new generator. Using 'next'
  -- is inefficient as all operations go via 'Integer'. See
  -- [here](https://alexey.kuleshevi.ch/blog/2019/12/21/random-benchmarks) for
  -- more details. It is thus deprecated.
  --
  -- @since 1.0.0
  next :: g -> (Int, g)
  next g = runStateGen g (uniformRM (genRange g))

  -- | Returns a 'Word8' that is uniformly distributed over the entire 'Word8'
  -- range.
  --
  -- @since 1.2.0
  genWord8 :: g -> (Word8, g)
  genWord8 = first fromIntegral . genWord32
  {-# INLINE genWord8 #-}

  -- | Returns a 'Word16' that is uniformly distributed over the entire 'Word16'
  -- range.
  --
  -- @since 1.2.0
  genWord16 :: g -> (Word16, g)
  genWord16 = first fromIntegral . genWord32
  {-# INLINE genWord16 #-}

  -- | Returns a 'Word32' that is uniformly distributed over the entire 'Word32'
  -- range.
  --
  -- @since 1.2.0
  genWord32 :: g -> (Word32, g)
  genWord32 = randomIvalIntegral (minBound, maxBound)
  -- Once `next` is removed, this implementation should be used instead:
  -- first fromIntegral . genWord64
  {-# INLINE genWord32 #-}

  -- | Returns a 'Word64' that is uniformly distributed over the entire 'Word64'
  -- range.
  --
  -- @since 1.2.0
  genWord64 :: g -> (Word64, g)
  genWord64 g =
    case genWord32 g of
      (l32, g') ->
        case genWord32 g' of
          (h32, g'') ->
            ((fromIntegral h32 `shiftL` 32) .|. fromIntegral l32, g'')
  {-# INLINE genWord64 #-}

  -- | @genWord32R upperBound g@ returns a 'Word32' that is uniformly
  -- distributed over the range @[0, upperBound]@.
  --
  -- @since 1.2.0
  genWord32R :: Word32 -> g -> (Word32, g)
  genWord32R m g = runStateGen g (unbiasedWordMult32 m)
  {-# INLINE genWord32R #-}

  -- | @genWord64R upperBound g@ returns a 'Word64' that is uniformly
  -- distributed over the range @[0, upperBound]@.
  --
  -- @since 1.2.0
  genWord64R :: Word64 -> g -> (Word64, g)
  genWord64R m g = runStateGen g (unsignedBitmaskWithRejectionM uniformWord64 m)
  {-# INLINE genWord64R #-}

  -- | @genShortByteString n g@ returns a 'ShortByteString' of length @n@
  -- filled with pseudo-random bytes.
  --
  -- @since 1.2.0
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
  --
  -- @since 1.0.0
  genRange :: g -> (Int, Int)
  genRange _ = (minBound, maxBound)

  -- | Returns two distinct pseudo-random number generators.
  --
  -- Implementations should take care to ensure that the resulting generators
  -- are not correlated. Some pseudo-random number generators are not
  -- splittable. In that case, the 'split' implementation should fail with a
  -- descriptive 'error' message.
  --
  -- @since 1.0.0
  split :: g -> (g, g)


-- | 'StatefulGen' is an interface to monadic pseudo-random number generators.
--
-- @since 1.2.0
class Monad m => StatefulGen g m where
  {-# MINIMAL (uniformWord32|uniformWord64) #-}
  -- | @uniformWord32R upperBound g@ generates a 'Word32' that is uniformly
  -- distributed over the range @[0, upperBound]@.
  --
  -- @since 1.2.0
  uniformWord32R :: Word32 -> g -> m Word32
  uniformWord32R = unsignedBitmaskWithRejectionM uniformWord32
  {-# INLINE uniformWord32R #-}

  -- | @uniformWord64R upperBound g@ generates a 'Word64' that is uniformly
  -- distributed over the range @[0, upperBound]@.
  --
  -- @since 1.2.0
  uniformWord64R :: Word64 -> g -> m Word64
  uniformWord64R = unsignedBitmaskWithRejectionM uniformWord64
  {-# INLINE uniformWord64R #-}

  -- | Generates a 'Word8' that is uniformly distributed over the entire 'Word8'
  -- range.
  --
  -- The default implementation extracts a 'Word8' from 'uniformWord32'.
  --
  -- @since 1.2.0
  uniformWord8 :: g -> m Word8
  uniformWord8 = fmap fromIntegral . uniformWord32
  {-# INLINE uniformWord8 #-}

  -- | Generates a 'Word16' that is uniformly distributed over the entire
  -- 'Word16' range.
  --
  -- The default implementation extracts a 'Word16' from 'uniformWord32'.
  --
  -- @since 1.2.0
  uniformWord16 :: g -> m Word16
  uniformWord16 = fmap fromIntegral . uniformWord32
  {-# INLINE uniformWord16 #-}

  -- | Generates a 'Word32' that is uniformly distributed over the entire
  -- 'Word32' range.
  --
  -- The default implementation extracts a 'Word32' from 'uniformWord64'.
  --
  -- @since 1.2.0
  uniformWord32 :: g -> m Word32
  uniformWord32 = fmap fromIntegral . uniformWord64
  {-# INLINE uniformWord32 #-}

  -- | Generates a 'Word64' that is uniformly distributed over the entire
  -- 'Word64' range.
  --
  -- The default implementation combines two 'Word32' from 'uniformWord32' into
  -- one 'Word64'.
  --
  -- @since 1.2.0
  uniformWord64 :: g -> m Word64
  uniformWord64 g = do
    l32 <- uniformWord32 g
    h32 <- uniformWord32 g
    pure (shiftL (fromIntegral h32) 32 .|. fromIntegral l32)
  {-# INLINE uniformWord64 #-}

  -- | @uniformShortByteString n g@ generates a 'ShortByteString' of length @n@
  -- filled with pseudo-random bytes.
  --
  -- @since 1.2.0
  uniformShortByteString :: Int -> g -> m ShortByteString
  default uniformShortByteString :: MonadIO m => Int -> g -> m ShortByteString
  uniformShortByteString n = genShortByteStringIO n . uniformWord64
  {-# INLINE uniformShortByteString #-}



-- | This class is designed for stateful pseudo-random number generators that
-- can be saved as and restored from an immutable data type.
--
-- @since 1.2.0
class StatefulGen (MutableGen f m) m => FrozenGen f m where
  -- | Represents the state of the pseudo-random number generator for use with
  -- 'thawGen' and 'freezeGen'.
  --
  -- @since 1.2.0
#if __GLASGOW_HASKELL__ >= 800
  type MutableGen f m = (g :: Type) | g -> f
#else
  type MutableGen f m :: *
#endif
  -- | Saves the state of the pseudo-random number generator as a frozen seed.
  --
  -- @since 1.2.0
  freezeGen :: MutableGen f m -> m f
  -- | Restores the pseudo-random number generator from its frozen seed.
  --
  -- @since 1.2.0
  thawGen :: f -> m (MutableGen f m)


data MBA = MBA (MutableByteArray# RealWorld)


-- | Efficiently generates a sequence of pseudo-random bytes in a platform
-- independent manner.
--
-- @since 1.2.0
genShortByteStringIO ::
     MonadIO m
  => Int -- ^ Number of bytes to generate
  -> m Word64 -- ^ IO action that can generate 8 random bytes at a time
  -> m ShortByteString
genShortByteStringIO n0 gen64 = do
  let !n@(I# n#) = max 0 n0
      !n64 = n `quot` 8
      !nrem = n `rem` 8
  mba@(MBA mba#) <-
    liftIO $ IO $ \s# ->
      case newByteArray# n# s# of
        (# s'#, mba# #) -> (# s'#, MBA mba# #)
  let go i =
        when (i < n64) $ do
          w64 <- gen64
          -- Writing 8 bytes at a time in a Little-endian order gives us
          -- platform portability
          liftIO $ writeWord64LE mba i w64
          go (i + 1)
  go 0
  when (nrem > 0) $ do
    w64 <- gen64
    -- In order to not mess up the byte order we write 1 byte at a time in
    -- Little endian order. It is tempting to simply generate as many bytes as we
    -- still need using smaller generators (eg. uniformWord8), but that would
    -- result in inconsistent tail when total length is slightly varied.
    liftIO $ writeByteSliceWord64LE mba (n - nrem) n w64
  liftIO $ IO $ \s# ->
    case unsafeFreezeByteArray# mba# s# of
      (# s'#, ba# #) -> (# s'#, SBS ba# #)
{-# INLINE genShortByteStringIO #-}

-- Architecture independent helpers:
io_ :: (State# RealWorld -> State# RealWorld) -> IO ()
io_ m# = IO $ \s# -> (# m# s#, () #)
{-# INLINE io_ #-}

writeWord8 :: MBA -> Int -> Word8 -> IO ()
writeWord8 (MBA mba#) (I# i#) (W8# w#) = io_ (writeWord8Array# mba# i# w#)
{-# INLINE writeWord8 #-}

writeByteSliceWord64LE :: MBA -> Int -> Int -> Word64 -> IO ()
writeByteSliceWord64LE mba fromByteIx toByteIx = go fromByteIx
  where
    go !i !z =
      when (i < toByteIx) $ do
        writeWord8 mba i (fromIntegral z :: Word8)
        go (i + 1) (z `shiftR` 8)
{-# INLINE writeByteSliceWord64LE #-}

writeWord64LE :: MBA -> Int -> Word64 -> IO ()
#ifdef WORDS_BIGENDIAN
writeWord64LE mba i w64 = do
  let !i8 = i * 8
  writeByteSliceWord64LE mba i8 (i8 + 8) w64
#else
writeWord64LE (MBA mba#) (I# i#) w64@(W64# w64#)
  | wordSizeInBits == 64 = io_ (writeWord64Array# mba# i# w64#)
  | otherwise = do
    let !i32# = i# *# 2#
        !(W32# w32l#) = fromIntegral w64
        !(W32# w32u#) = fromIntegral (w64 `shiftR` 32)
    io_ (writeWord32Array# mba# i32# w32l#)
    io_ (writeWord32Array# mba# (i32# +# 1#) w32u#)
#endif
{-# INLINE writeWord64LE #-}


-- | Same as 'genShortByteStringIO', but runs in 'ST'.
--
-- @since 1.2.0
genShortByteStringST :: Int -> ST s Word64 -> ST s ShortByteString
genShortByteStringST n action =
  unsafeIOToST (genShortByteStringIO n (unsafeSTToIO action))
{-# INLINE genShortByteStringST #-}


-- | Generates a pseudo-random 'ByteString' of the specified size.
--
-- @since 1.2.0
uniformByteStringM :: StatefulGen g m => Int -> g -> m ByteString
uniformByteStringM n g = do
  ba <- uniformShortByteString n g
  pure $
#if __GLASGOW_HASKELL__ < 802
       fromShort ba
#else
    let !(SBS ba#) = ba in
    if isTrue# (isByteArrayPinned# ba#)
      then pinnedByteArrayToByteString ba#
      else fromShort ba
{-# INLINE uniformByteStringM #-}

pinnedByteArrayToByteString :: ByteArray# -> ByteString
pinnedByteArrayToByteString ba# =
  PS (pinnedByteArrayToForeignPtr ba#) 0 (I# (sizeofByteArray# ba#))
{-# INLINE pinnedByteArrayToByteString #-}

pinnedByteArrayToForeignPtr :: ByteArray# -> ForeignPtr a
pinnedByteArrayToForeignPtr ba# =
  ForeignPtr (byteArrayContents# ba#) (PlainPtr (unsafeCoerce# ba#))
{-# INLINE pinnedByteArrayToForeignPtr #-}
#endif


-- | Opaque data type that carries the type of a pure pseudo-random number
-- generator.
--
-- @since 1.2.0
data StateGenM g = StateGenM

-- | Wrapper for pure state gen, which acts as an immutable seed for the corresponding
-- stateful generator `StateGenM`
--
-- @since 1.2.0
newtype StateGen g = StateGen { unStateGen :: g }
  deriving (Eq, Ord, Show, RandomGen, Storable, NFData)

instance (RandomGen g, MonadState g m) => StatefulGen (StateGenM g) m where
  uniformWord32R r _ = state (genWord32R r)
  {-# INLINE uniformWord32R #-}
  uniformWord64R r _ = state (genWord64R r)
  {-# INLINE uniformWord64R #-}
  uniformWord8 _ = state genWord8
  {-# INLINE uniformWord8 #-}
  uniformWord16 _ = state genWord16
  {-# INLINE uniformWord16 #-}
  uniformWord32 _ = state genWord32
  {-# INLINE uniformWord32 #-}
  uniformWord64 _ = state genWord64
  {-# INLINE uniformWord64 #-}
  uniformShortByteString n _ = state (genShortByteString n)
  {-# INLINE uniformShortByteString #-}

instance (RandomGen g, MonadState g m) => FrozenGen (StateGen g) m where
  type MutableGen (StateGen g) m = StateGenM g
  freezeGen _ = fmap StateGen get
  thawGen (StateGen g) = StateGenM <$ put g

-- | Splits a pseudo-random number generator into two. Updates the state with
-- one of the resulting generators and returns the other.
--
-- @since 1.2.0
splitGen :: (MonadState g m, RandomGen g) => m g
splitGen = state split
{-# INLINE splitGen #-}

-- | Runs a monadic generating action in the `State` monad using a pure
-- pseudo-random number generator.
--
-- ====__Examples__
--
-- >>> import System.Random.Stateful
-- >>> let pureGen = mkStdGen 137
-- >>> runStateGen pureGen randomM :: (Int, StdGen)
-- (7879794327570578227,StdGen {unStdGen = SMGen 11285859549637045894 7641485672361121627})
--
-- @since 1.2.0
runStateGen :: RandomGen g => g -> (StateGenM g -> State g a) -> (a, g)
runStateGen g f = runState (f StateGenM) g
{-# INLINE runStateGen #-}

-- | Runs a monadic generating action in the `State` monad using a pure
-- pseudo-random number generator. Returns only the resulting pseudo-random
-- value.
--
-- ====__Examples__
--
-- >>> import System.Random.Stateful
-- >>> let pureGen = mkStdGen 137
-- >>> runStateGen_ pureGen randomM :: Int
-- 7879794327570578227
--
-- @since 1.2.0
runStateGen_ :: RandomGen g => g -> (StateGenM g -> State g a) -> a
runStateGen_ g = fst . runStateGen g
{-# INLINE runStateGen_ #-}

-- | Runs a monadic generating action in the `StateT` monad using a pure
-- pseudo-random number generator.
--
-- ====__Examples__
--
-- >>> import System.Random.Stateful
-- >>> let pureGen = mkStdGen 137
-- >>> runStateGenT pureGen randomM :: IO (Int, StdGen)
-- (7879794327570578227,StdGen {unStdGen = SMGen 11285859549637045894 7641485672361121627})
--
-- @since 1.2.0
runStateGenT :: RandomGen g => g -> (StateGenM g -> StateT g m a) -> m (a, g)
runStateGenT g f = runStateT (f StateGenM) g
{-# INLINE runStateGenT #-}

-- | Runs a monadic generating action in the `StateT` monad using a pure
-- pseudo-random number generator. Returns only the resulting pseudo-random
-- value.
--
-- ====__Examples__
--
-- >>> import System.Random.Stateful
-- >>> let pureGen = mkStdGen 137
-- >>> runStateGenT_ pureGen randomM :: IO Int
-- 7879794327570578227
--
-- @since 1.2.1
runStateGenT_ :: (RandomGen g, Functor f) => g -> (StateGenM g -> StateT g f a) -> f a
runStateGenT_ g = fmap fst . runStateGenT g
{-# INLINE runStateGenT_ #-}

-- | Runs a monadic generating action in the `ST` monad using a pure
-- pseudo-random number generator.
--
-- @since 1.2.0
runStateGenST :: RandomGen g => g -> (forall s . StateGenM g -> StateT g (ST s) a) -> (a, g)
runStateGenST g action = runST $ runStateGenT g action
{-# INLINE runStateGenST #-}

-- | Runs a monadic generating action in the `ST` monad using a pure
-- pseudo-random number generator. Same as `runStateGenST`, but discards the
-- resulting generator.
--
-- @since 1.2.1
runStateGenST_ :: RandomGen g => g -> (forall s . StateGenM g -> StateT g (ST s) a) -> a
runStateGenST_ g action = runST $ runStateGenT_ g action
{-# INLINE runStateGenST_ #-}


-- | The standard pseudo-random number generator.
newtype StdGen = StdGen { unStdGen :: SM.SMGen }
  deriving (Show, RandomGen, NFData)

instance Eq StdGen where
  StdGen x1 == StdGen x2 = SM.unseedSMGen x1 == SM.unseedSMGen x2

instance RandomGen SM.SMGen where
  next = SM.nextInt
  {-# INLINE next #-}
  genWord32 = SM.nextWord32
  {-# INLINE genWord32 #-}
  genWord64 = SM.nextWord64
  {-# INLINE genWord64 #-}
  split = SM.splitSMGen
  {-# INLINE split #-}

instance RandomGen SM32.SMGen where
  next = SM32.nextInt
  {-# INLINE next #-}
  genWord32 = SM32.nextWord32
  {-# INLINE genWord32 #-}
  genWord64 = SM32.nextWord64
  {-# INLINE genWord64 #-}
  split = SM32.splitSMGen
  {-# INLINE split #-}

-- | Constructs a 'StdGen' deterministically.
mkStdGen :: Int -> StdGen
mkStdGen = StdGen . SM.mkSMGen . fromIntegral

-- | Global mutable veriable with `StdGen`
theStdGen :: IORef StdGen
theStdGen = unsafePerformIO $ SM.initSMGen >>= newIORef . StdGen
{-# NOINLINE theStdGen #-}


-- | The class of types for which a uniformly distributed value can be drawn
-- from all possible values of the type.
--
-- @since 1.2.0
class Uniform a where
  -- | Generates a value uniformly distributed over all possible values of that
  -- type.
  --
  -- There is a default implementation via 'Generic':
  --
  -- >>> :set -XDeriveGeneric -XDeriveAnyClass
  -- >>> import GHC.Generics (Generic)
  -- >>> import System.Random.Stateful
  -- >>> data MyBool = MyTrue | MyFalse deriving (Show, Generic, Finite, Uniform)
  -- >>> data Action = Code MyBool | Eat (Maybe Bool) | Sleep deriving (Show, Generic, Finite, Uniform)
  -- >>> gen <- newIOGenM (mkStdGen 42)
  -- >>> uniformListM 10 gen :: IO [Action]
  -- [Code MyTrue,Code MyTrue,Eat Nothing,Code MyFalse,Eat (Just False),Eat (Just True),Eat Nothing,Eat (Just False),Sleep,Code MyFalse]
  --
  -- @since 1.2.0
  uniformM :: StatefulGen g m => g -> m a

  default uniformM :: (StatefulGen g m, Generic a, GUniform (Rep a)) => g -> m a
  uniformM = fmap to . (`runContT` pure) . guniformM
  {-# INLINE uniformM #-}

-- | Default implementation of 'Uniform' type class for 'Generic' data.
-- It's important to use 'ContT', because without it 'fmap' and '>>=' remain
-- polymorphic too long and GHC fails to inline or specialize it, ending up
-- building full 'Rep' a structure in memory. 'ContT'
-- makes 'fmap' and '>>=' used in 'guniformM' monomorphic, so GHC is able to
-- specialize 'Generic' instance reasonably close to a handwritten one.
class GUniform f where
  guniformM :: StatefulGen g m => g -> ContT r m (f a)

instance GUniform f => GUniform (M1 i c f) where
  guniformM = fmap M1 . guniformM
  {-# INLINE guniformM #-}

instance Uniform a => GUniform (K1 i a) where
  guniformM = fmap K1 . lift . uniformM
  {-# INLINE guniformM #-}

instance GUniform U1 where
  guniformM = const $ return U1
  {-# INLINE guniformM #-}

instance (GUniform f, GUniform g) => GUniform (f :*: g) where
  guniformM g = (:*:) <$> guniformM g <*> guniformM g
  {-# INLINE guniformM #-}

instance (GFinite f, GFinite g) => GUniform (f :+: g) where
  guniformM = lift . finiteUniformM
  {-# INLINE guniformM #-}

finiteUniformM :: forall g m f a. (StatefulGen g m, GFinite f) => g -> m (f a)
finiteUniformM = fmap toGFinite . case gcardinality (proxy# :: Proxy# f) of
  Shift n
    | n <= 64 -> fmap toInteger . unsignedBitmaskWithRejectionM uniformWord64 (bit n - 1)
    | otherwise -> boundedByPowerOf2ExclusiveIntegralM n
  Card n
    | n <= bit 64 -> fmap toInteger . unsignedBitmaskWithRejectionM uniformWord64 (fromInteger n - 1)
    | otherwise -> boundedExclusiveIntegralM n
{-# INLINE finiteUniformM #-}

-- | A definition of 'Uniform' for 'System.Random.Finite' types.
-- If your data has several fields of sub-'Word' cardinality,
-- this instance may be more efficient than one, derived via 'Generic' and 'GUniform'.
--
-- >>> :set -XDeriveGeneric -XDeriveAnyClass
-- >>> import GHC.Generics (Generic)
-- >>> import System.Random.Stateful
-- >>> data Triple = Triple Word8 Word8 Word8 deriving (Show, Generic, Finite)
-- >>> instance Uniform Triple where uniformM = uniformViaFiniteM
-- >>> gen <- newIOGenM (mkStdGen 42)
-- >>> uniformListM 5 gen :: IO [Triple]
-- [Triple 60 226 48,Triple 234 194 151,Triple 112 96 95,Triple 51 251 15,Triple 6 0 208]
--
uniformViaFiniteM :: (StatefulGen g m, Generic a, GFinite (Rep a)) => g -> m a
uniformViaFiniteM = fmap to . finiteUniformM
{-# INLINE uniformViaFiniteM #-}

-- | The class of types for which a uniformly distributed value can be drawn
-- from a range.
--
-- @since 1.2.0
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
  -- @since 1.2.0
  uniformRM :: StatefulGen g m => (a, a) -> g -> m a

instance UniformRange Integer where
  uniformRM = uniformIntegralM
  {-# INLINE uniformRM #-}

instance UniformRange Natural where
  uniformRM = uniformIntegralM
  {-# INLINE uniformRM #-}

instance Uniform Int8 where
  uniformM = fmap (fromIntegral :: Word8 -> Int8) . uniformWord8
  {-# INLINE uniformM #-}
instance UniformRange Int8 where
  uniformRM = signedBitmaskWithRejectionRM (fromIntegral :: Int8 -> Word8) fromIntegral
  {-# INLINE uniformRM #-}

instance Uniform Int16 where
  uniformM = fmap (fromIntegral :: Word16 -> Int16) . uniformWord16
  {-# INLINE uniformM #-}
instance UniformRange Int16 where
  uniformRM = signedBitmaskWithRejectionRM (fromIntegral :: Int16 -> Word16) fromIntegral
  {-# INLINE uniformRM #-}

instance Uniform Int32 where
  uniformM = fmap (fromIntegral :: Word32 -> Int32) . uniformWord32
  {-# INLINE uniformM #-}
instance UniformRange Int32 where
  uniformRM = signedBitmaskWithRejectionRM (fromIntegral :: Int32 -> Word32) fromIntegral
  {-# INLINE uniformRM #-}

instance Uniform Int64 where
  uniformM = fmap (fromIntegral :: Word64 -> Int64) . uniformWord64
  {-# INLINE uniformM #-}
instance UniformRange Int64 where
  uniformRM = signedBitmaskWithRejectionRM (fromIntegral :: Int64 -> Word64) fromIntegral
  {-# INLINE uniformRM #-}

wordSizeInBits :: Int
wordSizeInBits = finiteBitSize (0 :: Word)

instance Uniform Int where
  uniformM
    | wordSizeInBits == 64 =
      fmap (fromIntegral :: Word64 -> Int) . uniformWord64
    | otherwise =
      fmap (fromIntegral :: Word32 -> Int) . uniformWord32
  {-# INLINE uniformM #-}

instance UniformRange Int where
  uniformRM = signedBitmaskWithRejectionRM (fromIntegral :: Int -> Word) fromIntegral
  {-# INLINE uniformRM #-}

instance Uniform Word where
  uniformM
    | wordSizeInBits == 64 =
      fmap (fromIntegral :: Word64 -> Word) . uniformWord64
    | otherwise =
      fmap (fromIntegral :: Word32 -> Word) . uniformWord32
  {-# INLINE uniformM #-}

instance UniformRange Word where
  uniformRM = unsignedBitmaskWithRejectionRM
  {-# INLINE uniformRM #-}

instance Uniform Word8 where
  uniformM = uniformWord8
  {-# INLINE uniformM #-}
instance UniformRange Word8 where
  uniformRM = unbiasedWordMult32RM
  {-# INLINE uniformRM #-}

instance Uniform Word16 where
  uniformM = uniformWord16
  {-# INLINE uniformM #-}
instance UniformRange Word16 where
  uniformRM = unbiasedWordMult32RM
  {-# INLINE uniformRM #-}

instance Uniform Word32 where
  uniformM  = uniformWord32
  {-# INLINE uniformM #-}
instance UniformRange Word32 where
  uniformRM = unbiasedWordMult32RM
  {-# INLINE uniformRM #-}

instance Uniform Word64 where
  uniformM  = uniformWord64
  {-# INLINE uniformM #-}
instance UniformRange Word64 where
  uniformRM = unsignedBitmaskWithRejectionRM
  {-# INLINE uniformRM #-}

#if __GLASGOW_HASKELL__ >= 802
instance Uniform CBool where
  uniformM = fmap CBool . uniformM
  {-# INLINE uniformM #-}
instance UniformRange CBool where
  uniformRM (CBool b, CBool t) = fmap CBool . uniformRM (b, t)
  {-# INLINE uniformRM #-}
#endif

instance Uniform CChar where
  uniformM = fmap CChar . uniformM
  {-# INLINE uniformM #-}
instance UniformRange CChar where
  uniformRM (CChar b, CChar t) = fmap CChar . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CSChar where
  uniformM = fmap CSChar . uniformM
  {-# INLINE uniformM #-}
instance UniformRange CSChar where
  uniformRM (CSChar b, CSChar t) = fmap CSChar . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CUChar where
  uniformM = fmap CUChar . uniformM
  {-# INLINE uniformM #-}
instance UniformRange CUChar where
  uniformRM (CUChar b, CUChar t) = fmap CUChar . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CShort where
  uniformM = fmap CShort . uniformM
  {-# INLINE uniformM #-}
instance UniformRange CShort where
  uniformRM (CShort b, CShort t) = fmap CShort . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CUShort where
  uniformM = fmap CUShort . uniformM
  {-# INLINE uniformM #-}
instance UniformRange CUShort where
  uniformRM (CUShort b, CUShort t) = fmap CUShort . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CInt where
  uniformM = fmap CInt . uniformM
  {-# INLINE uniformM #-}
instance UniformRange CInt where
  uniformRM (CInt b, CInt t) = fmap CInt . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CUInt where
  uniformM = fmap CUInt . uniformM
  {-# INLINE uniformM #-}
instance UniformRange CUInt where
  uniformRM (CUInt b, CUInt t) = fmap CUInt . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CLong where
  uniformM = fmap CLong . uniformM
  {-# INLINE uniformM #-}
instance UniformRange CLong where
  uniformRM (CLong b, CLong t) = fmap CLong . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CULong where
  uniformM = fmap CULong . uniformM
  {-# INLINE uniformM #-}
instance UniformRange CULong where
  uniformRM (CULong b, CULong t) = fmap CULong . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CPtrdiff where
  uniformM = fmap CPtrdiff . uniformM
  {-# INLINE uniformM #-}
instance UniformRange CPtrdiff where
  uniformRM (CPtrdiff b, CPtrdiff t) = fmap CPtrdiff . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CSize where
  uniformM = fmap CSize . uniformM
  {-# INLINE uniformM #-}
instance UniformRange CSize where
  uniformRM (CSize b, CSize t) = fmap CSize . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CWchar where
  uniformM = fmap CWchar . uniformM
  {-# INLINE uniformM #-}
instance UniformRange CWchar where
  uniformRM (CWchar b, CWchar t) = fmap CWchar . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CSigAtomic where
  uniformM = fmap CSigAtomic . uniformM
  {-# INLINE uniformM #-}
instance UniformRange CSigAtomic where
  uniformRM (CSigAtomic b, CSigAtomic t) = fmap CSigAtomic . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CLLong where
  uniformM = fmap CLLong . uniformM
  {-# INLINE uniformM #-}
instance UniformRange CLLong where
  uniformRM (CLLong b, CLLong t) = fmap CLLong . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CULLong where
  uniformM = fmap CULLong . uniformM
  {-# INLINE uniformM #-}
instance UniformRange CULLong where
  uniformRM (CULLong b, CULLong t) = fmap CULLong . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CIntPtr where
  uniformM = fmap CIntPtr . uniformM
  {-# INLINE uniformM #-}
instance UniformRange CIntPtr where
  uniformRM (CIntPtr b, CIntPtr t) = fmap CIntPtr . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CUIntPtr where
  uniformM = fmap CUIntPtr . uniformM
  {-# INLINE uniformM #-}
instance UniformRange CUIntPtr where
  uniformRM (CUIntPtr b, CUIntPtr t) = fmap CUIntPtr . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CIntMax where
  uniformM = fmap CIntMax . uniformM
  {-# INLINE uniformM #-}
instance UniformRange CIntMax where
  uniformRM (CIntMax b, CIntMax t) = fmap CIntMax . uniformRM (b, t)
  {-# INLINE uniformRM #-}

instance Uniform CUIntMax where
  uniformM = fmap CUIntMax . uniformM
  {-# INLINE uniformM #-}
instance UniformRange CUIntMax where
  uniformRM (CUIntMax b, CUIntMax t) = fmap CUIntMax . uniformRM (b, t)
  {-# INLINE uniformRM #-}

-- | See [Floating point number caveats](System-Random-Stateful.html#fpcaveats).
instance UniformRange CFloat where
  uniformRM (CFloat l, CFloat h) = fmap CFloat . uniformRM (l, h)
  {-# INLINE uniformRM #-}

-- | See [Floating point number caveats](System-Random-Stateful.html#fpcaveats).
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
#if __GLASGOW_HASKELL__ < 902
word32ToChar (W32# w#) = C# (chr# (word2Int# w#))
#else
word32ToChar (W32# w#) = C# (chr# (word2Int# (word32ToWord# w#)))
#endif
{-# INLINE word32ToChar #-}

charToWord32 :: Char -> Word32
#if __GLASGOW_HASKELL__ < 902
charToWord32 (C# c#) = W32# (int2Word# (ord# c#))
#else
charToWord32 (C# c#) = W32# (wordToWord32# (int2Word# (ord# c#)))
#endif
{-# INLINE charToWord32 #-}

instance Uniform Char where
  uniformM g = word32ToChar <$> unbiasedWordMult32 (charToWord32 maxBound) g
  {-# INLINE uniformM #-}
instance UniformRange Char where
  uniformRM (l, h) g =
    word32ToChar <$> unbiasedWordMult32RM (charToWord32 l, charToWord32 h) g
  {-# INLINE uniformRM #-}

instance Uniform () where
  uniformM = const $ pure ()
  {-# INLINE uniformM #-}
instance UniformRange () where
  uniformRM = const $ const $ pure ()
  {-# INLINE uniformRM #-}

instance Uniform Bool where
  uniformM = fmap wordToBool . uniformWord8
    where wordToBool w = (w .&. 1) /= 0
          {-# INLINE wordToBool #-}
  {-# INLINE uniformM #-}
instance UniformRange Bool where
  uniformRM (False, False) _g = return False
  uniformRM (True, True)   _g = return True
  uniformRM _               g = uniformM g
  {-# INLINE uniformRM #-}

-- | See [Floating point number caveats](System-Random-Stateful.html#fpcaveats).
instance UniformRange Double where
  uniformRM (l, h) g
    | l == h = return l
    | isInfinite l || isInfinite h =
      -- Optimisation exploiting absorption:
      --   (-Infinity) + (anything but +Infinity) = -Infinity
      --   (anything but -Infinity) + (+Infinity) = +Infinity
      --                (-Infinity) + (+Infinity) = NaN
      return $! h + l
    | otherwise = do
      x <- uniformDouble01M g
      return $ x * l + (1 -x) * h
  {-# INLINE uniformRM #-}

-- | Generates uniformly distributed 'Double' in the range \([0, 1]\).
--   Numbers are generated by generating uniform 'Word64' and dividing
--   it by \(2^{64}\). It's used to implement 'UniformRange' instance for
--   'Double'.
--
-- @since 1.2.0
uniformDouble01M :: forall g m. StatefulGen g m => g -> m Double
uniformDouble01M g = do
  w64 <- uniformWord64 g
  return $ fromIntegral w64 / m
  where
    m = fromIntegral (maxBound :: Word64) :: Double
{-# INLINE uniformDouble01M #-}

-- | Generates uniformly distributed 'Double' in the range
--   \((0, 1]\). Number is generated as \(2^{-64}/2+\operatorname{uniformDouble01M}\).
--   Constant is 1\/2 of smallest nonzero value which could be generated
--   by 'uniformDouble01M'.
--
-- @since 1.2.0
uniformDoublePositive01M :: forall g m. StatefulGen g m => g -> m Double
uniformDoublePositive01M g = (+ d) <$> uniformDouble01M g
  where
    -- We add small constant to shift generated value from zero. It's
    -- selected as 1/2 of smallest possible nonzero value
    d = 2.710505431213761e-20 -- 2**(-65)
{-# INLINE uniformDoublePositive01M #-}

-- | See [Floating point number caveats](System-Random-Stateful.html#fpcaveats).
instance UniformRange Float where
  uniformRM (l, h) g
    | l == h = return l
    | isInfinite l || isInfinite h =
      -- Optimisation exploiting absorption:
      --   (-Infinity) + (anything but +Infinity) = -Infinity
      --   (anything but -Infinity) + (+Infinity) = +Infinity
      --                (-Infinity) + (+Infinity) = NaN
      return $! h + l
    | otherwise = do
      x <- uniformFloat01M g
      return $ x * l + (1 - x) * h
  {-# INLINE uniformRM #-}

-- | Generates uniformly distributed 'Float' in the range \([0, 1]\).
--   Numbers are generated by generating uniform 'Word32' and dividing
--   it by \(2^{32}\). It's used to implement 'UniformRange' instance for 'Float'.
--
-- @since 1.2.0
uniformFloat01M :: forall g m. StatefulGen g m => g -> m Float
uniformFloat01M g = do
  w32 <- uniformWord32 g
  return $ fromIntegral w32 / m
  where
    m = fromIntegral (maxBound :: Word32) :: Float
{-# INLINE uniformFloat01M #-}

-- | Generates uniformly distributed 'Float' in the range
--   \((0, 1]\). Number is generated as \(2^{-32}/2+\operatorname{uniformFloat01M}\).
--   Constant is 1\/2 of smallest nonzero value which could be generated
--   by 'uniformFloat01M'.
--
-- @since 1.2.0
uniformFloatPositive01M :: forall g m. StatefulGen g m => g -> m Float
uniformFloatPositive01M g = (+ d) <$> uniformFloat01M g
  where
    -- See uniformDoublePositive01M
    d = 1.1641532182693481e-10 -- 2**(-33)
{-# INLINE uniformFloatPositive01M #-}

-- | Generates uniformly distributed 'Enum'.
-- One can use it to define a 'Uniform' instance:
--
-- > data Colors = Red | Green | Blue deriving (Enum, Bounded)
-- > instance Uniform Colors where uniformM = uniformEnumM
--
-- @since 1.2.1
uniformEnumM :: forall a g m. (Enum a, Bounded a, StatefulGen g m) => g -> m a
uniformEnumM g = toEnum <$> uniformRM (fromEnum (minBound :: a), fromEnum (maxBound :: a)) g
{-# INLINE uniformEnumM #-}

-- | Generates uniformly distributed 'Enum' in the given range.
-- One can use it to define a 'UniformRange' instance:
--
-- > data Colors = Red | Green | Blue deriving (Enum)
-- > instance UniformRange Colors where
-- >   uniformRM = uniformEnumRM
-- >   inInRange (lo, hi) x = isInRange (fromEnum lo, fromEnum hi) (fromEnum x)
--
-- @since 1.2.1
uniformEnumRM :: forall a g m. (Enum a, StatefulGen g m) => (a, a) -> g -> m a
uniformEnumRM (l, h) g = toEnum <$> uniformRM (fromEnum l, fromEnum h) g
{-# INLINE uniformEnumRM #-}

-- The two integer functions below take an [inclusive,inclusive] range.
randomIvalIntegral :: (RandomGen g, Integral a) => (a, a) -> g -> (a, g)
randomIvalIntegral (l, h) = randomIvalInteger (toInteger l, toInteger h)

{-# SPECIALIZE randomIvalInteger :: (Num a) =>
    (Integer, Integer) -> StdGen -> (a, StdGen) #-}

randomIvalInteger :: (RandomGen g, Num a) => (Integer, Integer) -> g -> (a, g)
randomIvalInteger (l, h) rng
 | l > h     = randomIvalInteger (h,l) rng
 | otherwise = case f 1 0 rng of (v, rng') -> (fromInteger (l + v `mod` k), rng')
     where
       (genlo, genhi) = genRange rng
       b = fromIntegral genhi - fromIntegral genlo + 1 :: Integer

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
uniformIntegralM :: forall a g m. (Bits a, Integral a, StatefulGen g m) => (a, a) -> g -> m a
uniformIntegralM (l, h) gen = case l `compare` h of
  LT -> do
    let limit = h - l
    bounded <- case toIntegralSized limit :: Maybe Word64 of
      Just limitAsWord64 ->
        -- Optimisation: if 'limit' fits into 'Word64', generate a bounded
        -- 'Word64' and then convert to 'Integer'
        fromIntegral <$> unsignedBitmaskWithRejectionM uniformWord64 limitAsWord64 gen
      Nothing -> boundedExclusiveIntegralM (limit + 1) gen
    return $ l + bounded
  GT -> uniformIntegralM (h, l) gen
  EQ -> pure l
{-# INLINEABLE uniformIntegralM #-}
{-# SPECIALIZE uniformIntegralM :: StatefulGen g m => (Integer, Integer) -> g -> m Integer #-}
{-# SPECIALIZE uniformIntegralM :: StatefulGen g m => (Natural, Natural) -> g -> m Natural #-}

-- | Generate an integral in the range @[0, s)@ using a variant of Lemire's
-- multiplication method.
--
-- Daniel Lemire. 2019. Fast Random Integer Generation in an Interval. In ACM
-- Transactions on Modeling and Computer Simulation
-- https://doi.org/10.1145/3230636
--
-- PRECONDITION (unchecked): s > 0
boundedExclusiveIntegralM :: forall a g m . (Bits a, Integral a, StatefulGen g m) => a -> g -> m a
boundedExclusiveIntegralM s gen = go
  where
    n = integralWordSize s
    -- We renamed 'L' from the paper to 'k' here because 'L' is not a valid
    -- variable name in Haskell and 'l' is already used in the algorithm.
    k = wordSizeInBits * n
    twoToK = (1 :: a) `shiftL` k
    modTwoToKMask = twoToK - 1

    t = (twoToK - s) `rem` s -- `rem`, instead of `mod` because `twoToK >= s` is guaranteed
    go :: (Bits a, Integral a, StatefulGen g m) => m a
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

-- | boundedByPowerOf2ExclusiveIntegralM s ~ boundedExclusiveIntegralM (bit s)
boundedByPowerOf2ExclusiveIntegralM ::
  forall a g m. (Bits a, Integral a, StatefulGen g m) => Int -> g -> m a
boundedByPowerOf2ExclusiveIntegralM s gen = do
  let n = (s + wordSizeInBits - 1) `quot` wordSizeInBits
  x <- uniformIntegralWords n gen
  return $ x .&. (bit s - 1)
{-# INLINE boundedByPowerOf2ExclusiveIntegralM #-}

-- | @integralWordSize i@ returns that least @w@ such that
-- @i <= WORD_SIZE_IN_BITS^w@.
integralWordSize :: (Bits a, Num a) => a -> Int
integralWordSize = go 0
  where
    go !acc i
      | i == 0 = acc
      | otherwise = go (acc + 1) (i `shiftR` wordSizeInBits)
{-# INLINE integralWordSize #-}

-- | @uniformIntegralWords n@ is a uniformly pseudo-random integral in the range
-- @[0, WORD_SIZE_IN_BITS^n)@.
uniformIntegralWords :: forall a g m. (Bits a, Integral a, StatefulGen g m) => Int -> g -> m a
uniformIntegralWords n gen = go 0 n
  where
    go !acc i
      | i == 0 = return acc
      | otherwise = do
        (w :: Word) <- uniformM gen
        go ((acc `shiftL` wordSizeInBits) .|. fromIntegral w) (i - 1)
{-# INLINE uniformIntegralWords #-}

-- | Uniformly generate an 'Integral' in an inclusive-inclusive range.
--
-- Only use for integrals size less than or equal to that of 'Word32'.
unbiasedWordMult32RM :: forall a g m. (Integral a, StatefulGen g m) => (a, a) -> g -> m a
unbiasedWordMult32RM (b, t) g
  | b <= t    = (+b) . fromIntegral <$> unbiasedWordMult32 (fromIntegral (t - b)) g
  | otherwise = (+t) . fromIntegral <$> unbiasedWordMult32 (fromIntegral (b - t)) g
{-# INLINE unbiasedWordMult32RM #-}

-- | Uniformly generate Word32 in @[0, s]@.
unbiasedWordMult32 :: forall g m. StatefulGen g m => Word32 -> g -> m Word32
unbiasedWordMult32 s g
  | s == maxBound = uniformWord32 g
  | otherwise = unbiasedWordMult32Exclusive (s+1) g
{-# INLINE unbiasedWordMult32 #-}

-- | See [Lemire's paper](https://arxiv.org/pdf/1805.10941.pdf),
-- [O\'Neill's
-- blogpost](https://www.pcg-random.org/posts/bounded-rands.html) and
-- more directly [O\'Neill's github
-- repo](https://github.com/imneme/bounded-rands/blob/3d71f53c975b1e5b29f2f3b05a74e26dab9c3d84/bounded32.cpp#L234).
-- N.B. The range is [0,r) **not** [0,r].
unbiasedWordMult32Exclusive :: forall g m . StatefulGen g m => Word32 -> g -> m Word32
unbiasedWordMult32Exclusive r g = go
  where
    t :: Word32
    t = (-r) `mod` r -- Calculates 2^32 `mod` r!!!
    go :: StatefulGen g m => m Word32
    go = do
      x <- uniformWord32 g
      let m :: Word64
          m = fromIntegral x * fromIntegral r
          l :: Word32
          l = fromIntegral m
      if l >= t then return (fromIntegral $ m `shiftR` 32) else go
{-# INLINE unbiasedWordMult32Exclusive #-}

-- | This only works for unsigned integrals
unsignedBitmaskWithRejectionRM ::
     forall a g m . (FiniteBits a, Num a, Ord a, Uniform a, StatefulGen g m)
  => (a, a)
  -> g
  -> m a
unsignedBitmaskWithRejectionRM (bottom, top) gen
  | bottom == top = pure top
  | otherwise = (b +) <$> unsignedBitmaskWithRejectionM uniformM r gen
  where
    (b, r) = if bottom > top then (top, bottom - top) else (bottom, top - bottom)
{-# INLINE unsignedBitmaskWithRejectionRM #-}

-- | This works for signed integrals by explicit conversion to unsigned and abusing
-- overflow. It uses `unsignedBitmaskWithRejectionM`, therefore it requires functions that
-- take the value to unsigned and back.
signedBitmaskWithRejectionRM ::
     forall a b g m. (Num a, Num b, Ord b, Ord a, FiniteBits a, StatefulGen g m, Uniform a)
  => (b -> a) -- ^ Convert signed to unsigned. @a@ and @b@ must be of the same size.
  -> (a -> b) -- ^ Convert unsigned to signed. @a@ and @b@ must be of the same size.
  -> (b, b) -- ^ Range.
  -> g -- ^ Generator.
  -> m b
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


-- | Detailed explanation about the algorithm employed here can be found in this post:
-- http://web.archive.org/web/20200520071940/https://www.pcg-random.org/posts/bounded-rands.html
unsignedBitmaskWithRejectionM ::
  forall a g m. (Ord a, FiniteBits a, Num a, StatefulGen g m) => (g -> m a) -> a -> g -> m a
unsignedBitmaskWithRejectionM genUniformM range gen = go
  where
    mask :: a
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
  {-# INLINE uniformM #-}

instance (Uniform a, Uniform b, Uniform c) => Uniform (a, b, c) where
  uniformM g = (,,) <$> uniformM g <*> uniformM g <*> uniformM g
  {-# INLINE uniformM #-}

instance (Uniform a, Uniform b, Uniform c, Uniform d) => Uniform (a, b, c, d) where
  uniformM g = (,,,) <$> uniformM g <*> uniformM g <*> uniformM g <*> uniformM g
  {-# INLINE uniformM #-}

instance (Uniform a, Uniform b, Uniform c, Uniform d, Uniform e) => Uniform (a, b, c, d, e) where
  uniformM g = (,,,,) <$> uniformM g <*> uniformM g <*> uniformM g <*> uniformM g <*> uniformM g
  {-# INLINE uniformM #-}

instance (Uniform a, Uniform b, Uniform c, Uniform d, Uniform e, Uniform f) =>
  Uniform (a, b, c, d, e, f) where
  uniformM g = (,,,,,)
               <$> uniformM g
               <*> uniformM g
               <*> uniformM g
               <*> uniformM g
               <*> uniformM g
               <*> uniformM g
  {-# INLINE uniformM #-}

instance (Uniform a, Uniform b, Uniform c, Uniform d, Uniform e, Uniform f, Uniform g) =>
  Uniform (a, b, c, d, e, f, g) where
  uniformM g = (,,,,,,)
               <$> uniformM g
               <*> uniformM g
               <*> uniformM g
               <*> uniformM g
               <*> uniformM g
               <*> uniformM g
               <*> uniformM g
  {-# INLINE uniformM #-}

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
