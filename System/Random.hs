{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

#include "MachDeps.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Random
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE in the 'random' repository)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- This library deals with the common task of pseudo-random number
-- generation. The library makes it possible to generate repeatable
-- results, by starting with a specified initial random number generator,
-- or to get different results on each run by using the system-initialised
-- generator or by supplying a seed from some other source.
--
-- The library is split into two layers:
--
-- * A core /random number generator/ provides a supply of bits.
--   The class 'RandomGen' provides a common interface to such generators.
--   The library provides one instance of 'RandomGen', the abstract
--   data type 'StdGen'.  Programmers may, of course, supply their own
--   instances of 'RandomGen'.
--
-- * The class 'Random' provides a way to extract values of a particular
--   type from a random number generator.  For example, the 'Float'
--   instance of 'Random' allows one to generate random values of type
--   'Float'.
--
-- This implementation uses the SplitMix algorithm [1].
--
-- [/Example for RNG Implementors:/]
--
-- Suppose you want to use a [permuted congruential
-- generator](https://en.wikipedia.org/wiki/Permuted_congruential_generator)
-- as the source of entropy (FIXME: is that the correct
-- terminology). You can make it an instance of `RandomGen`:
--
-- >>> data PCGen = PCGen !Word64 !Word64
--
-- >>> :{
-- let stepGen :: PCGen -> (Word32, PCGen)
--     stepGen (PCGen state inc) = let
--       newState = state * 6364136223846793005 + (inc .|. 1)
--       xorShifted = fromIntegral (((state `shiftR` 18) `xor` state) `shiftR` 27) :: Word32
--       rot = fromIntegral (state `shiftR` 59) :: Word32
--       out = (xorShifted `shiftR` (fromIntegral rot)) .|. (xorShifted `shiftL` fromIntegral ((-rot) .&. 31))
--       in (out, PCGen newState inc)
-- :}
--
-- >>> fst $ stepGen $ snd $ stepGen (PCGen 17 29)
-- 3288430965
--
-- >>> :{
-- instance RandomGen PCGen where
--   next g = (fromIntegral y, h)
--     where
--       (y, h) = stepGen g
--   split _ = error "This PRNG is not splittable"
-- :}
--
-- Importantly, this implementation will not be as efficient as it
-- could be because the random values are converted to 'Integer' and
-- then to desired type.
--
-- Instead we should define (where e.g. @unBuildWord32 :: Word32 ->
-- (Word16, Word16)@ is a function to pull apart a 'Word32' into a
-- pair of 'Word16'):
--
-- >>> data PCGen' = PCGen' !Word64 !Word64
--
-- >>> :{
-- instance RandomGen PCGen' where
--   genWord8  (PCGen' s i) = (z, PCGen' s' i')
--     where
--       (x, PCGen s' i') = stepGen (PCGen s i)
--       y = fst $ unBuildWord32 x
--       z = fst $ unBuildWord16 y
--   genWord16 (PCGen' s i) = (y, PCGen' s' i')
--     where
--       (x, PCGen s' i') = stepGen (PCGen s i)
--       y = fst $ unBuildWord32 x
--   genWord32 (PCGen' s i) = (x, PCGen' s' i')
--     where
--       (x, PCGen s' i') = stepGen (PCGen s i)
--   genWord64 (PCGen' s i) = (undefined, PCGen' s i)
--     where
--       (x, g)           = stepGen (PCGen s i)
--       (y, PCGen s' i') = stepGen g
--   split _ = error "This PRNG is not splittable"
-- :}
--
-- [/Example for RNG Users:/]
--
-- Suppose you want to simulate rolls from a dice (yes I know it's a
-- plural form but it's now common to use it as a singular form):
--
-- >>> :{
-- let randomListM :: (MonadRandom g m, Num a, Uniform a) => g -> Int -> m [a]
--     randomListM gen n = replicateM n (uniform gen)
-- :}
--
-- >>> :{
-- let rolls :: [Word32]
--     rolls = runGenState_
--               (PCGen 17 29)
--               (randomListM PureGenI 10 >>= \xs -> return $ map ((+1) . (`mod` 6)) xs)
-- :}
--
-- >>> rolls
-- [1,4,2,4,2,2,3,1,5,1]
--
-- FIXME: What should we say about generating values from types other
-- than Word8 etc?
--
-----------------------------------------------------------------------------

module System.Random
  (

  -- $intro

  -- * Random number generators

    RandomGen(..)
  , MonadRandom(..)
  , withGenM
  -- ** Standard random number generators
  , StdGen
  , mkStdGen

  -- * Stateful interface for pure generators
  -- ** Based on StateT
  , PureGen
  , splitGen
  , genRandom
  , runGenState
  , runGenState_
  , runGenStateT
  , runGenStateT_
  , runPureGenST
  -- ** Based on PrimMonad
  -- *** PrimGen - boxed thread safe state
  , PrimGen
  , runPrimGenST
  , runPrimGenST_
  , runPrimGenIO
  , runPrimGenIO_
  , splitPrimGen
  , atomicPrimGen
  -- *** MutGen - unboxed mutable state
  , MutGen
  , runMutGenST
  , runMutGenST_
  , runMutGenIO
  , runMutGenIO_
  , splitMutGen
  , applyMutGen

  -- ** The global random number generator

  -- $globalrng

  , getStdRandom
  , getStdGen
  , setStdGen
  , newStdGen

  -- * Random values of various types
  -- $uniform
  , Uniform(..)
  , UniformRange(..)
  , Random(..)

  -- * Generators for sequences of bytes
  , uniformByteArrayPrim
  , uniformByteStringPrim
  , genByteString

  -- * References
  -- $references

  -- * Internals
  , bitmaskWithRejection -- FIXME Export this in a better way, e.g. in System.Random.Impl or something like that
  ) where

import Control.Arrow
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.State.Strict
import Data.Bits
import Data.ByteString.Builder.Prim (word64LE)
import Data.ByteString.Builder.Prim.Internal (runF)
import Data.ByteString.Internal (ByteString(PS))
import Data.ByteString.Short.Internal (ShortByteString(SBS), fromShort)
import Data.Int
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Primitive.ByteArray
import Data.Primitive.MutVar
import Data.Primitive.Types as Primitive (Prim, sizeOf)
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import GHC.Exts (Ptr(..), build)
import GHC.ForeignPtr
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Random.SplitMix as SM

#if !MIN_VERSION_primitive(0,7,0)
import Data.Primitive.Types (Addr(..))

mutableByteArrayContentsCompat mba =
  case mutableByteArrayContents mba of
    Addr addr# -> Ptr addr#
#else
mutableByteArrayContentsCompat = mutableByteArrayContents
#endif
mutableByteArrayContentsCompat :: MutableByteArray s -> Ptr Word8
{-# INLINE mutableByteArrayContentsCompat #-}

-- $setup
-- >>> import System.IO (IOMode(WriteMode), hPutStr, withBinaryFile)
-- >>> :set -XFlexibleContexts
-- >>> :set -fno-warn-missing-methods
-- >>> :{
-- unBuildWord32 :: Word32 -> (Word16, Word16)
-- unBuildWord32 w = (fromIntegral (shiftR w 16),
--                    fromIntegral (fromIntegral (maxBound :: Word16) .&. w))
-- :}
--
-- >>> :{
-- unBuildWord16 :: Word16 -> (Word8, Word8)
-- unBuildWord16 w = (fromIntegral (shiftR w 8),
--                    fromIntegral (fromIntegral (maxBound :: Word8) .&. w))
-- :}
--
-- >>> :{
-- buildWord64 :: Word32 -> Word32 -> Word64
-- buildWord64 w0 w1 = ((fromIntegral w1) `shiftL` 32) .|. (fromIntegral w0)
-- :}

-- | The class 'RandomGen' provides a common interface to random number
-- generators.
{-# DEPRECATED next "Use genWord32[R] or genWord64[R]" #-}
{-# DEPRECATED genRange "Use genWord32[R] or genWord64[R]" #-}
class RandomGen g where
  {-# MINIMAL (next,genRange)|((genWord32|genWord32R),(genWord64|genWord64R)) #-}
  -- |The 'next' operation returns an 'Int' that is uniformly
  -- distributed in the range returned by 'genRange' (including both
  -- end points), and a new generator. Using 'next' is inefficient as
  -- all operations go via 'Integer'. See
  -- [here](https://alexey.kuleshevi.ch/blog/2019/12/21/random-benchmarks)
  -- for more details. It is thus deprecated. If you need random
  -- values from other types you will need to construct a suitable
  -- conversion function.
  next :: g -> (Int, g)
  next g = (minR + fromIntegral w, g') where
    (minR, maxR) = genRange g
    range = fromIntegral $ maxR - minR
#if WORD_SIZE_IN_BITS == 32
    (w, g') = genWord32R range g
#elif WORD_SIZE_IN_BITS == 64
    (w, g') = genWord64R range g
#else
-- https://hackage.haskell.org/package/ghc-prim-0.5.3/docs/GHC-Prim.html#g:1
-- GHC always implements Int using the primitive type Int#, whose size equals
-- the MachDeps.h constant WORD_SIZE_IN_BITS. [...] Currently GHC itself has
-- only 32-bit and 64-bit variants [...].
# error unsupported WORD_SIZE_IN_BITS
#endif

  genWord8 :: g -> (Word8, g)
  genWord8 = first fromIntegral . genWord32R (fromIntegral (maxBound :: Word8))

  genWord16 :: g -> (Word16, g)
  genWord16 = first fromIntegral . genWord32R (fromIntegral (maxBound :: Word16))

  genWord32 :: g -> (Word32, g)
  genWord32 = genWord32R maxBound

  genWord64 :: g -> (Word64, g)
  genWord64 = genWord64R maxBound

  genWord32R :: Word32 -> g -> (Word32, g)
  genWord32R m = randomIvalIntegral (minBound, m)

  genWord64R :: Word64 -> g -> (Word64, g)
  genWord64R m = randomIvalIntegral (minBound, m)

  genByteArray :: Int -> g -> (ByteArray, g)
  genByteArray n g = runPureGenST g $ uniformByteArrayPrim n
  {-# INLINE genByteArray #-}

  -- |The 'genRange' operation yields the range of values returned by
  -- the generator.
  --
  -- It is required that:
  --
  -- * If @(a, b) = 'genRange' g@, then @a <= b@.
  --
  -- * 'genRange' always returns a pair of defined 'Int's.
  --
  -- The second condition ensures that 'genRange' cannot examine its
  -- argument, and hence the value it returns can be determined only by the
  -- instance of 'RandomGen'.  That in turn allows an implementation to make
  -- a single call to 'genRange' to establish a generator's range, without
  -- being concerned that the generator returned by (say) 'next' might have
  -- a different range to the generator passed to 'next'.
  --
  -- The default definition spans the full range of 'Int'.
  genRange :: g -> (Int, Int)
  genRange _ = (minBound, maxBound)

  -- | The 'split' operation allows one to obtain two distinct random number
  -- generators.
  split :: g -> (g, g)

class Monad m => MonadRandom g m where
  data Frozen g :: *
  {-# MINIMAL freezeGen,thawGen,(uniformWord32R|uniformWord32),(uniformWord64R|uniformWord64) #-}

  thawGen :: Frozen g -> m g
  freezeGen :: g -> m (Frozen g)
  -- | Generate `Word32` up to and including the supplied max value
  uniformWord32R :: Word32 -> g -> m Word32
  uniformWord32R = bitmaskWithRejection32M
  -- | Generate `Word64` up to and including the supplied max value
  uniformWord64R :: Word64 -> g -> m Word64
  uniformWord64R = bitmaskWithRejection64M

  uniformWord8 :: g -> m Word8
  uniformWord8 = fmap fromIntegral . uniformWord32R (fromIntegral (maxBound :: Word8))
  uniformWord16 :: g -> m Word16
  uniformWord16 = fmap fromIntegral . uniformWord32R (fromIntegral (maxBound :: Word16))
  uniformWord32 :: g -> m Word32
  uniformWord32 = uniformWord32R maxBound
  uniformWord64 :: g -> m Word64
  uniformWord64 = uniformWord64R maxBound
  uniformByteArray :: Int -> g -> m ByteArray
  default uniformByteArray :: PrimMonad m => Int -> g -> m ByteArray
  uniformByteArray = uniformByteArrayPrim
  {-# INLINE uniformByteArray #-}


withGenM :: MonadRandom g m => Frozen g -> (g -> m a) -> m (a, Frozen g)
withGenM fg action = do
  g <- thawGen fg
  res <- action g
  fg' <- freezeGen g
  pure (res, fg')


-- | This function will efficiently generate a sequence of random bytes in a platform
-- independent manner. Memory allocated will be pinned, so it is safe to use for FFI
-- calls.
uniformByteArrayPrim :: (MonadRandom g m, PrimMonad m) => Int -> g -> m ByteArray
uniformByteArrayPrim n0 gen = do
  let n = max 0 n0
      (n64, nrem64) = n `quotRem` 8
  ma <- newPinnedByteArray n
  let go i ptr
        | i < n64 = do
          w64 <- uniformWord64 gen
          -- Writing 8 bytes at a time in a Little-endian order gives us platform
          -- portability
          unsafeIOToPrim $ runF word64LE w64 ptr
          go (i + 1) (ptr `plusPtr` 8)
        | otherwise = return ptr
  ptr <- go 0 (mutableByteArrayContentsCompat ma)
  when (nrem64 > 0) $ do
    w64 <- uniformWord64 gen
    -- In order to not mess up the byte order we write generated Word64 into a temporary
    -- pointer and then copy only the missing bytes over to the array. It is tempting to
    -- simply generate as many bytes as we still need using smaller generators
    -- (eg. uniformWord8), but that would result in inconsistent tail when total length is
    -- slightly varied.
    unsafeIOToPrim $
      alloca $ \w64ptr -> do
        runF word64LE w64 w64ptr
        forM_ [0 .. nrem64 - 1] $ \i -> do
          w8 :: Word8 <- peekByteOff w64ptr i
          pokeByteOff ptr i w8
  unsafeFreezeByteArray ma
{-# INLINE uniformByteArrayPrim #-}


pinnedMutableByteArrayToByteString :: MutableByteArray RealWorld -> ByteString
pinnedMutableByteArrayToByteString mba =
  PS (pinnedMutableByteArrayToForeignPtr mba) 0 (sizeofMutableByteArray mba)
{-# INLINE pinnedMutableByteArrayToByteString #-}

pinnedMutableByteArrayToForeignPtr :: MutableByteArray RealWorld -> ForeignPtr a
pinnedMutableByteArrayToForeignPtr mba@(MutableByteArray mba#) =
  case mutableByteArrayContentsCompat mba of
    Ptr addr# -> ForeignPtr addr# (PlainPtr mba#)
{-# INLINE pinnedMutableByteArrayToForeignPtr #-}

-- | Generate a ByteString using a pure generator. For monadic counterpart see
-- `uniformByteStringPrim`.
--
-- @since 1.2
uniformByteStringPrim ::
     (MonadRandom g m, PrimMonad m) => Int -> g -> m ByteString
uniformByteStringPrim n g = do
  ba@(ByteArray ba#) <- uniformByteArray n g
  if isByteArrayPinned ba
    then unsafeIOToPrim $
         pinnedMutableByteArrayToByteString <$> unsafeThawByteArray ba
    else return $ fromShort (SBS ba#)
{-# INLINE uniformByteStringPrim #-}

-- | Generate a ByteString using a pure generator. For monadic counterpart see
-- `uniformByteStringPrim`.
--
-- @since 1.2
genByteString :: RandomGen g => Int -> g -> (ByteString, g)
genByteString n g = runPureGenST g (uniformByteStringPrim n)
{-# INLINE genByteString #-}

-- | Run an effectful generating action in `ST` monad using a pure generator.
--
-- @since 1.2
runPureGenST :: RandomGen g => g -> (forall s . PureGen g -> StateT g (ST s) a) -> (a, g)
runPureGenST g action = runST $ runGenStateT g $ action PureGenI
{-# INLINE runPureGenST #-}


-- | An opaque data type that carries the type of a pure generator
data PureGen g = PureGenI

instance (MonadState g m, RandomGen g) => MonadRandom (PureGen g) m where
  newtype Frozen (PureGen g) = PureGen g
  thawGen (PureGen g) = PureGenI <$ put g
  freezeGen _ =fmap PureGen get
  uniformWord32R r _ = state (genWord32R r)
  uniformWord64R r _ = state (genWord64R r)
  uniformWord8 _ = state genWord8
  uniformWord16 _ = state genWord16
  uniformWord32 _ = state genWord32
  uniformWord64 _ = state genWord64
  uniformByteArray n _ = state (genByteArray n)

-- | Generate a random value in a state monad
--
-- @since 1.2
genRandom :: (RandomGen g, Random a, MonadState g m) => m a
genRandom = randomM PureGenI

-- | Split current generator and update the state with one part, while returning the other.
--
-- @since 1.2
splitGen :: (MonadState g m, RandomGen g) => m g
splitGen = state split

runGenState :: RandomGen g => g -> State g a -> (a, g)
runGenState = flip runState

runGenState_ :: RandomGen g => g -> State g a -> a
runGenState_ g = fst . flip runState g

runGenStateT :: RandomGen g => g -> StateT g m a -> m (a, g)
runGenStateT = flip runStateT

runGenStateT_ :: (RandomGen g, Functor f) => g -> StateT g f a -> f a
runGenStateT_ g = fmap fst . flip runStateT g

-- | This is a wrapper wround pure generator that can be used in an effectful environment.
-- It is safe in presence of concurrency since all operations are performed atomically.
--
-- @since 1.2
newtype PrimGen s g = PrimGenI (MutVar s g)

instance (s ~ PrimState m, PrimMonad m, RandomGen g) =>
         MonadRandom (PrimGen s g) m where
  newtype Frozen (PrimGen s g) = PrimGen g
  thawGen (PrimGen g) = fmap PrimGenI (newMutVar g)
  freezeGen (PrimGenI gVar) = fmap PrimGen (readMutVar gVar)
  uniformWord32R r = atomicPrimGen (genWord32R r)
  uniformWord64R r = atomicPrimGen (genWord64R r)
  uniformWord8 = atomicPrimGen genWord8
  uniformWord16 = atomicPrimGen genWord16
  uniformWord32 = atomicPrimGen genWord32
  uniformWord64 = atomicPrimGen genWord64
  {-# INLINE uniformWord64 #-}
  uniformByteArray n = atomicPrimGen (genByteArray n)

-- | Apply a pure operation to generator atomically.
atomicPrimGen :: PrimMonad m => (g -> (a, g)) -> PrimGen (PrimState m) g -> m a
atomicPrimGen op (PrimGenI gVar) =
  atomicModifyMutVar' gVar $ \g ->
    case op g of
      (a, g') -> (g', a)
{-# INLINE atomicPrimGen #-}


-- | Split `PrimGen` into atomically updated current generator and a newly created that is
-- returned.
--
-- @since 1.2
splitPrimGen ::
     (RandomGen g, PrimMonad m)
  => PrimGen (PrimState m) g
  -> m (PrimGen (PrimState m) g)
splitPrimGen = atomicPrimGen split >=> thawGen . PrimGen

runPrimGenST :: RandomGen g => g -> (forall s . PrimGen s g -> ST s a) -> (a, g)
runPrimGenST g action = runST $ do
  primGen <- thawGen $ PrimGen g
  res <- action primGen
  PrimGen g' <- freezeGen primGen
  pure (res, g')

-- | Same as `runPrimGenST`, but discard the resulting generator.
runPrimGenST_ :: RandomGen g => g -> (forall s . PrimGen s g -> ST s a) -> a
runPrimGenST_ g action = fst $ runPrimGenST g action

-- | Functions like 'runPrimGenIO' are necessary for example if you
-- wish to write a function like
--
-- >>> let ioGen gen = withBinaryFile "foo.txt" WriteMode $ \h -> ((randomM gen) :: IO Word32) >>= (hPutStr h . show)
--
-- and then run it
--
-- >>> runPrimGenIO_ (mkStdGen 1729) ioGen
--
runPrimGenIO :: (RandomGen g, MonadIO m) => g -> (PrimGen RealWorld g -> m a) -> m (a, g)
runPrimGenIO g action = do
  primGen <- liftIO $ thawGen $ PrimGen g
  res <- action primGen
  PrimGen g' <- liftIO $ freezeGen primGen
  pure (res, g')
{-# INLINE runPrimGenIO #-}

-- | Same as `runPrimGenIO`, but discard the resulting generator.
runPrimGenIO_ :: (RandomGen g, MonadIO m) => g -> (PrimGen RealWorld g -> m a) -> m a
runPrimGenIO_ g action = fst <$> runPrimGenIO g action
{-# INLINE runPrimGenIO_ #-}


newtype MutGen s g = MutGenI (MutableByteArray s)

instance (s ~ PrimState m, PrimMonad m, RandomGen g, Prim g) =>
         MonadRandom (MutGen s g) m where
  newtype Frozen (MutGen s g) = MutGen g
  thawGen (MutGen g) = do
    ma <- newByteArray (Primitive.sizeOf g)
    writeByteArray ma 0 g
    pure $ MutGenI ma
  freezeGen (MutGenI ma) = MutGen <$> readByteArray ma 0
  uniformWord32R r = applyMutGen (genWord32R r)
  uniformWord64R r = applyMutGen (genWord64R r)
  uniformWord8 = applyMutGen genWord8
  uniformWord16 = applyMutGen genWord16
  uniformWord32 = applyMutGen genWord32
  uniformWord64 = applyMutGen genWord64
  uniformByteArray n = applyMutGen (genByteArray n)

applyMutGen :: (Prim g, PrimMonad m) => (g -> (a, g)) -> MutGen (PrimState m) g -> m a
applyMutGen f (MutGenI ma) = do
  g <- readByteArray ma 0
  case f g of
    (res, g') -> res <$ writeByteArray ma 0 g'

-- | Split `MutGen` into atomically updated current generator and a newly created that is
-- returned.
--
-- @since 1.2
splitMutGen ::
     (Prim g, RandomGen g, PrimMonad m)
  => MutGen (PrimState m) g
  -> m (MutGen (PrimState m) g)
splitMutGen = applyMutGen split >=> thawGen . MutGen

runMutGenST :: (Prim g, RandomGen g) => g -> (forall s . MutGen s g -> ST s a) -> (a, g)
runMutGenST g action = runST $ do
  mutGen <- thawGen $ MutGen g
  res <- action mutGen
  MutGen g' <- freezeGen mutGen
  pure (res, g')

-- | Same as `runMutGenST`, but discard the resulting generator.
runMutGenST_ :: (Prim g, RandomGen g) => g -> (forall s . MutGen s g -> ST s a) -> a
runMutGenST_ g action = fst $ runMutGenST g action

runMutGenIO :: (Prim g, RandomGen g, MonadIO m) => g -> (MutGen RealWorld g -> m a) -> m (a, g)
runMutGenIO g action = do
  mutGen <- liftIO $ thawGen $ MutGen g
  res <- action mutGen
  MutGen g' <- liftIO $ freezeGen mutGen
  pure (res, g')

-- | Same as `runMutGenIO`, but discard the resulting generator.
runMutGenIO_ :: (Prim g, RandomGen g, MonadIO m) => g -> (MutGen RealWorld g -> m a) -> m a
runMutGenIO_ g action = fst <$> runMutGenIO g action

type StdGen = SM.SMGen

instance RandomGen StdGen where
  next = SM.nextInt
  genWord32 = SM.nextWord32
  genWord64 = SM.nextWord64
  split = SM.splitSMGen


{- |
The function 'mkStdGen' provides an alternative way of producing an initial
generator, by mapping an 'Int' into a generator. Again, distinct arguments
should be likely to produce distinct generators.
-}
mkStdGen :: Int -> StdGen
mkStdGen s = SM.mkSMGen $ fromIntegral s


-- $uniform
--
-- @random@ has two type classes for generation of random numbers:
-- 'Uniform' and 'UniformRange'. One for generating every possible
-- value and another for generating every value in range. In other
-- libraries this functionality frequently bundled into single type
-- class but here we have two type classes because there're types
-- which could have instance for one type class but not the other.
--
-- For example: 'Integer', 'Float', 'Double' have instance for
-- @UniformRange@ but there's no way to define @Uniform@.
--
-- Conversely there're types where @Uniform@ instance is possible
-- while @UniformRange@ is not. One example is tuples: @(a,b)@. While
-- @Uniform@ instance is straightforward it's not clear how to define
-- @UniformRange@. We could try to generate values that @a <= x <= b@
-- But to do that we need to know number of elements of tuple's second
-- type parameter @b@ which we don't have.
-- 
-- Or type could have no order at all. Take for example
-- angle. Defining @Uniform@ instance is again straghtforward: just
-- generate value in @[0,2π)@ range. But for any two pair of angles
-- there're two ranges: clockwise and counterclockwise.


-- | Generate every possible value for data type with equal probability.
class Uniform a where
  uniform :: MonadRandom g m => g -> m a

-- | Generate every value in provided inclusive range with equal
--   probability. So @uniformR (1,4)@ should generate values from set
--   @[1,2,3,4]@. Inclusive range is used to allow to express any
--   interval for fixed-size ints, enumerations etc.
--
--   Additionally in order to make function always defined order of
--   elements in range shouldn't matter and following law should hold:
--
-- > uniformR (a,b) = uniform (b,a)
class UniformRange a where
  uniformR :: MonadRandom g m => (a, a) -> g -> m a


{- |
With a source of random number supply in hand, the 'Random' class allows the
programmer to extract random values of a variety of types.

Minimal complete definition: 'randomR' and 'random'.

-}
{-# DEPRECATED randomR "In favor of `uniformR`" #-}
{-# DEPRECATED randomRIO "In favor of `uniformR`" #-}
{-# DEPRECATED randomIO "In favor of `uniformR`" #-}
class Random a where

  -- | Takes a range /(lo,hi)/ and a random number generator
  -- /g/, and returns a random value uniformly distributed in the closed
  -- interval /[lo,hi]/, together with a new generator. It is unspecified
  -- what happens if /lo>hi/. For continuous types there is no requirement
  -- that the values /lo/ and /hi/ are ever produced, but they may be,
  -- depending on the implementation and the interval.
  {-# INLINE randomR #-}
  randomR :: RandomGen g => (a, a) -> g -> (a, g)
  default randomR :: (RandomGen g, UniformRange a) => (a, a) -> g -> (a, g)
  randomR r g = runGenState g (uniformR r PureGenI)

  -- | The same as 'randomR', but using a default range determined by the type:
  --
  -- * For bounded types (instances of 'Bounded', such as 'Char'),
  --   the range is normally the whole type.
  --
  -- * For fractional types, the range is normally the semi-closed interval
  -- @[0,1)@.
  --
  -- * For 'Integer', the range is (arbitrarily) the range of 'Int'.
  {-# INLINE random #-}
  random  :: RandomGen g => g -> (a, g)
  random g = runGenState g genRandom

  --{-# INLINE randomM #-}
  randomM :: MonadRandom g m => g -> m a
  -- default randomM :: (MonadRandom g m, Uniform a) => g -> m a
  -- randomM = uniform

  -- | Plural variant of 'randomR', producing an infinite list of
  -- random values instead of returning a new generator.
  {-# INLINE randomRs #-}
  randomRs :: RandomGen g => (a,a) -> g -> [a]
  randomRs ival g = build (\cons _nil -> buildRandoms cons (randomR ival) g)

  -- | Plural variant of 'random', producing an infinite list of
  -- random values instead of returning a new generator.
  {-# INLINE randoms #-}
  randoms  :: RandomGen g => g -> [a]
  randoms  g      = build (\cons _nil -> buildRandoms cons random g)

  -- | A variant of 'randomR' that uses the global random number generator
  -- (see "System.Random#globalrng").
  randomRIO :: (a,a) -> IO a
  randomRIO range  = getStdRandom (randomR range)

  -- | A variant of 'random' that uses the global random number generator
  -- (see "System.Random#globalrng").
  randomIO  :: IO a
  randomIO   = getStdRandom random

-- | Produce an infinite list-equivalent of random values.
{-# INLINE buildRandoms #-}
buildRandoms :: RandomGen g
             => (a -> as -> as)  -- ^ E.g. '(:)' but subject to fusion
             -> (g -> (a,g))     -- ^ E.g. 'random'
             -> g                -- ^ A 'RandomGen' instance
             -> as
buildRandoms cons rand = go
  where
    -- The seq fixes part of #4218 and also makes fused Core simpler.
    go g = x `seq` (x `cons` go g') where (x,g') = rand g


instance Random Integer where
  random g = randomR (toInteger (minBound::Int), toInteger (maxBound::Int)) g
  randomM g = uniformR (toInteger (minBound::Int), toInteger (maxBound::Int)) g

instance UniformRange Integer where
  --uniformR ival g = randomIvalInteger ival g -- FIXME

instance Random Int8 where
  randomM = uniform
instance Uniform Int8 where
  uniform = fmap (fromIntegral :: Word8 -> Int8) . uniformWord8
instance UniformRange Int8 where

instance Random Int16 where
  randomM = uniform
instance Uniform Int16 where
  uniform = fmap (fromIntegral :: Word16 -> Int16) . uniformWord16
instance UniformRange Int16 where

instance Random Int32 where
  randomM = uniform
instance Uniform Int32 where
  uniform = fmap (fromIntegral :: Word32 -> Int32) . uniformWord32
instance UniformRange Int32 where

instance Random Int64 where
  randomM = uniform
instance Uniform Int64 where
  uniform = fmap (fromIntegral :: Word64 -> Int64) . uniformWord64
instance UniformRange Int64 where

instance Random Int where
  randomM = uniform
instance Uniform Int where
#if WORD_SIZE_IN_BITS < 64
  uniform = fmap (fromIntegral :: Word32 -> Int) . uniformWord32
#else
  uniform = fmap (fromIntegral :: Word64 -> Int) . uniformWord64
#endif
instance UniformRange Int where

instance Random Word where
  randomM = uniform
instance Uniform Word where
#if WORD_SIZE_IN_BITS < 64
  uniform = fmap (fromIntegral :: Word32 -> Word) . uniformWord32
#else
  uniform = fmap (fromIntegral :: Word64 -> Word) . uniformWord64
#endif
instance UniformRange Word where
  {-# INLINE uniformR #-}
  uniformR    = bitmaskWithRejectionRM

instance Random Word8 where
  randomM = uniform
instance Uniform Word8 where
  {-# INLINE uniform #-}
  uniform     = uniformWord8
instance UniformRange Word8 where
  {-# INLINE uniformR #-}
  uniformR    = bitmaskWithRejectionRM

instance Random Word16 where
  randomM = uniform
instance Uniform Word16 where
  {-# INLINE uniform #-}
  uniform     = uniformWord16
instance UniformRange Word16 where
  {-# INLINE uniformR #-}
  uniformR    = bitmaskWithRejectionRM

instance Random Word32 where
  randomM = uniform
instance Uniform Word32 where
  {-# INLINE uniform #-}
  uniform  = uniformWord32
instance UniformRange Word32 where
  {-# INLINE uniformR #-}
  uniformR = bitmaskWithRejectionRM

instance Random Word64 where
  randomM = uniform
instance Uniform Word64 where
  {-# INLINE uniform #-}
  uniform  = uniformWord64
instance UniformRange Word64 where
  {-# INLINE uniformR #-}
  uniformR = bitmaskWithRejectionRM


instance Random CChar where
  randomM = uniform
instance Uniform CChar where
  uniform                     = fmap CChar . uniform
instance UniformRange CChar where
  uniformR (CChar b, CChar t) = fmap CChar . uniformR (b, t)

instance Random CSChar where
  randomM = uniform
instance Uniform CSChar where
  uniform                       = fmap CSChar . uniform
instance UniformRange CSChar where
  uniformR (CSChar b, CSChar t) = fmap CSChar . uniformR (b, t)

instance Random CUChar where
  randomM = uniform
instance Uniform CUChar where
  uniform                       = fmap CUChar . uniform
instance UniformRange CUChar where
  uniformR (CUChar b, CUChar t) = fmap CUChar . uniformR (b, t)

instance Random CShort where
  randomM = uniform
instance Uniform CShort where
  uniform                       = fmap CShort . uniform
instance UniformRange CShort where
  uniformR (CShort b, CShort t) = fmap CShort . uniformR (b, t)

instance Random CUShort where
  randomM = uniform
instance Uniform CUShort where
  uniform                         = fmap CUShort . uniform
instance UniformRange CUShort where
  uniformR (CUShort b, CUShort t) = fmap CUShort . uniformR (b, t)

instance Random CInt where
  randomM = uniform
instance Uniform CInt where
  uniform                   = fmap CInt . uniform
instance UniformRange CInt where
  uniformR (CInt b, CInt t) = fmap CInt . uniformR (b, t)

instance Random CUInt where
  randomM = uniform
instance Uniform CUInt where
  uniform                     = fmap CUInt . uniform
instance UniformRange CUInt where
  uniformR (CUInt b, CUInt t) = fmap CUInt . uniformR (b, t)

instance Random CLong where
  randomM = uniform
instance Uniform CLong where
  uniform                     = fmap CLong . uniform
instance UniformRange CLong where
  uniformR (CLong b, CLong t) = fmap CLong . uniformR (b, t)

instance Random CULong where
  randomM = uniform
instance Uniform CULong where
  uniform                       = fmap CULong . uniform
instance UniformRange CULong where
  uniformR (CULong b, CULong t) = fmap CULong . uniformR (b, t)

instance Random CPtrdiff where
  randomM = uniform
instance Uniform CPtrdiff where
  uniform                           = fmap CPtrdiff . uniform
instance UniformRange CPtrdiff where
  uniformR (CPtrdiff b, CPtrdiff t) = fmap CPtrdiff . uniformR (b, t)

instance Random CSize where
  randomM = uniform
instance Uniform CSize where
  uniform                     = fmap CSize . uniform
instance UniformRange CSize where
  uniformR (CSize b, CSize t) = fmap CSize . uniformR (b, t)

instance Random CWchar where
  randomM = uniform
instance Uniform CWchar where
  uniform                       = fmap CWchar . uniform
instance UniformRange CWchar where
  uniformR (CWchar b, CWchar t) = fmap CWchar . uniformR (b, t)

instance Random CSigAtomic where
  randomM = uniform
instance Uniform CSigAtomic where
  uniform                               = fmap CSigAtomic . uniform
instance UniformRange CSigAtomic where
  uniformR (CSigAtomic b, CSigAtomic t) = fmap CSigAtomic . uniformR (b, t)

instance Random CLLong where
  randomM = uniform
instance Uniform CLLong where
  uniform                       = fmap CLLong . uniform
instance UniformRange CLLong where
  uniformR (CLLong b, CLLong t) = fmap CLLong . uniformR (b, t)

instance Random CULLong where
  randomM = uniform
instance Uniform CULLong where
  uniform                         = fmap CULLong . uniform
instance UniformRange CULLong where
  uniformR (CULLong b, CULLong t) = fmap CULLong . uniformR (b, t)

instance Random CIntPtr where
  randomM = uniform
instance Uniform CIntPtr where
  uniform                         = fmap CIntPtr . uniform
instance UniformRange CIntPtr where
  uniformR (CIntPtr b, CIntPtr t) = fmap CIntPtr . uniformR (b, t)

instance Random CUIntPtr where
  randomM = uniform
instance Uniform CUIntPtr where
  uniform                           = fmap CUIntPtr . uniform
instance UniformRange CUIntPtr where
  uniformR (CUIntPtr b, CUIntPtr t) = fmap CUIntPtr . uniformR (b, t)

instance Random CIntMax where
  randomM = uniform
instance Uniform CIntMax where
  uniform                         = fmap CIntMax . uniform
instance UniformRange CIntMax where
  uniformR (CIntMax b, CIntMax t) = fmap CIntMax . uniformR (b, t)

instance Random CUIntMax where
  randomM = uniform
instance Uniform CUIntMax where
  uniform                           = fmap CUIntMax . uniform
instance UniformRange CUIntMax where
  uniformR (CUIntMax b, CUIntMax t) = fmap CUIntMax . uniformR (b, t)

instance Random Char where
  randomM = uniform
instance Uniform Char where
  uniform = uniformR (minBound, maxBound)
instance UniformRange Char where
  -- FIXME

instance Random Bool where
  randomM = uniform
instance Uniform Bool where
  uniform = uniformR (minBound, maxBound)
instance UniformRange Bool where
  uniformR (a, b) g = int2Bool <$> uniformR (bool2Int a, bool2Int b) g
    where
      bool2Int :: Bool -> Int
      bool2Int False = 0
      bool2Int True  = 1
      int2Bool :: Int -> Bool
      int2Bool 0 = False
      int2Bool _ = True

{-# INLINE randomRFloating #-}
randomRFloating :: (Fractional a, Num a, Ord a, Random a, RandomGen g) => (a, a) -> g -> (a, g)
randomRFloating (l,h) g
    | l>h       = randomRFloating (h,l) g
    | otherwise = let (coef,g') = random g in
                    (2.0 * (0.5*l + coef * (0.5*h - 0.5*l)), g')  -- avoid overflow

instance Random Double where
  randomR = randomRFloating
  random = randomDouble
  randomM = uniformR (0, 1)

instance UniformRange Double

randomDouble :: RandomGen b => b -> (Double, b)
randomDouble rng =
    case random rng of
      (x,rng') ->
          -- We use 53 bits of randomness corresponding to the 53 bit significand:
          ((fromIntegral (mask53 .&. (x::Int64)) :: Double)
           /  fromIntegral twoto53, rng')
   where
    twoto53 = (2::Int64) ^ (53::Int64)
    mask53 = twoto53 - 1


instance Random Float where
  randomR = randomRFloating
  random = randomFloat
  randomM = uniformR (0, 1)

instance UniformRange Float

randomFloat :: RandomGen b => b -> (Float, b)
randomFloat rng =
    -- TODO: Faster to just use 'next' IF it generates enough bits of randomness.
    case random rng of
      (x,rng') ->
          -- We use 24 bits of randomness corresponding to the 24 bit significand:
          ((fromIntegral (mask24 .&. (x::Int32)) :: Float)
           /  fromIntegral twoto24, rng')
          -- Note, encodeFloat is another option, but I'm not seeing slightly
          --  worse performance with the following [2011.06.25]:
--         (encodeFloat rand (-24), rng')
   where
     mask24 = twoto24 - 1
     twoto24 = (2::Int32) ^ (24::Int32)

-- CFloat/CDouble are basically the same as a Float/Double:
-- instance Random CFloat where
--   randomR = randomRFloating
  -- random rng = case random rng of
  --              (x,rng') -> (realToFrac (x::Float), rng')

-- instance Random CDouble where
--   randomR = randomRFloating
--   -- A MYSTERY:
--   -- Presently, this is showing better performance than the Double instance:
--   -- (And yet, if the Double instance uses randomFrac then its performance is much worse!)
--   random  = randomFrac
--   -- random rng = case random rng of
--   --                  (x,rng') -> (realToFrac (x::Double), rng')

-- The two integer functions below take an [inclusive,inclusive] range.
randomIvalIntegral :: (RandomGen g, Integral a) => (a, a) -> g -> (a, g)
randomIvalIntegral (l,h) = randomIvalInteger (toInteger l, toInteger h)

{-# SPECIALIZE randomIvalInteger :: (Num a) =>
    (Integer, Integer) -> StdGen -> (a, StdGen) #-}

randomIvalInteger :: (RandomGen g, Num a) => (Integer, Integer) -> g -> (a, g)
randomIvalInteger (l,h) rng
 | l > h     = randomIvalInteger (h,l) rng
 | otherwise = case (f 1 0 rng) of (v, rng') -> (fromInteger (l + v `mod` k), rng')
     where
       (genlo, genhi) = genRange rng
       b = fromIntegral genhi - fromIntegral genlo + 1

       -- Probabilities of the most likely and least likely result
       -- will differ at most by a factor of (1 +- 1/q).  Assuming the RandomGen
       -- is uniform, of course

       -- On average, log q / log b more random values will be generated
       -- than the minimum
       q = 1000
       k = h - l + 1
       magtgt = k * q

       -- generate random values until we exceed the target magnitude
       f mag v g | mag >= magtgt = (v, g)
                 | otherwise = v' `seq`f (mag*b) v' g' where
                        (x,g') = next g
                        v' = (v * b + (fromIntegral x - fromIntegral genlo))


bitmaskWithRejection ::
     (RandomGen g, FiniteBits a, Num a, Ord a, Random a)
  => (a, a)
  -> g
  -> (a, g)
bitmaskWithRejection (bottom, top)
  | bottom > top = bitmaskWithRejection (top, bottom)
  | bottom == top = (,) top
  | otherwise = first (bottom +) . go
  where
    range = top - bottom
    mask = complement zeroBits `shiftR` countLeadingZeros (range .|. 1)
    go g =
      let (x, g') = random g
          x' = x .&. mask
       in if x' >= range
            then go g'
            else (x', g')
{-# INLINE bitmaskWithRejection #-}


-- FIXME This is likely incorrect for signed integrals.
bitmaskWithRejectionRM ::
     (MonadRandom g m, FiniteBits a, Num a, Ord a, Random a)
  => (a, a)
  -> g
  -> m a
bitmaskWithRejectionRM (bottom, top) gen
  | bottom > top = bitmaskWithRejectionRM (top, bottom) gen
  | bottom == top = pure top
  | otherwise = (bottom +) <$> go
  where
    range = top - bottom
    mask = complement zeroBits `shiftR` countLeadingZeros (range .|. 1)
    go = do
      x <- randomM gen
      let x' = x .&. mask
      if x' >= range
        then go
        else pure x'
{-# INLINE bitmaskWithRejectionRM #-}

bitmaskWithRejectionM :: (Ord a, FiniteBits a, Num a, MonadRandom g m) => (g -> m a) -> a -> g -> m a
bitmaskWithRejectionM genUniform range gen = go
  where
    mask = complement zeroBits `shiftR` countLeadingZeros (range .|. 1)
    go = do
      x <- genUniform gen
      let x' = x .&. mask
      if x' >= range
        then go
        else pure x'


bitmaskWithRejection32M :: MonadRandom g m => Word32 -> g -> m Word32
bitmaskWithRejection32M = bitmaskWithRejectionM uniformWord32

bitmaskWithRejection64M :: MonadRandom g m => Word64 -> g -> m Word64
bitmaskWithRejection64M = bitmaskWithRejectionM uniformWord64

-- The global random number generator

{- $globalrng #globalrng#

There is a single, implicit, global random number generator of type
'StdGen', held in some global variable maintained by the 'IO' monad. It is
initialised automatically in some system-dependent fashion, for example, by
using the time of day, or Linux's kernel random number generator. To get
deterministic behaviour, use 'setStdGen'.
-}

-- |Sets the global random number generator.
setStdGen :: StdGen -> IO ()
setStdGen sgen = writeIORef theStdGen sgen

-- |Gets the global random number generator.
getStdGen :: IO StdGen
getStdGen  = readIORef theStdGen

theStdGen :: IORef StdGen
theStdGen  = unsafePerformIO $ SM.initSMGen >>= newIORef
{-# NOINLINE theStdGen #-}

-- |Applies 'split' to the current global random generator,
-- updates it with one of the results, and returns the other.
newStdGen :: IO StdGen
newStdGen = atomicModifyIORef' theStdGen split

{- |Uses the supplied function to get a value from the current global
random generator, and updates the global generator with the new generator
returned by the function. For example, @rollDice@ gets a random integer
between 1 and 6:

>  rollDice :: IO Int
>  rollDice = getStdRandom (randomR (1,6))

-}

getStdRandom :: (StdGen -> (a,StdGen)) -> IO a
getStdRandom f = atomicModifyIORef' theStdGen (swap . f)
  where swap (v,g) = (g,v)

{- $references

1. Guy L. Steele, Jr., Doug Lea, and Christine H. Flood. 2014. Fast splittable
pseudorandom number generators. In Proceedings of the 2014 ACM International
Conference on Object Oriented Programming Systems Languages & Applications
(OOPSLA '14). ACM, New York, NY, USA, 453-472. DOI:
https://doi.org/10.1145/2660193.2660195

-}
