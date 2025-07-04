{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      :  System.Random.Stateful
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE in the 'random' repository)
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
--
-- This library deals with the common task of pseudo-random number generation.
module System.Random.Stateful
  (
  -- * Pure Random Generator
  module System.Random
  -- * Monadic Random Generator
  -- $introduction

  -- * Usage
  -- $usagemonadic

  -- * Mutable pseudo-random number generator interfaces
  -- $interfaces
  , StatefulGen
      ( uniformWord32R
      , uniformWord64R
      , uniformWord8
      , uniformWord16
      , uniformWord32
      , uniformWord64
      , uniformShortByteString
      )
  , FrozenGen(..)
  , ThawedGen(..)
  , withMutableGen
  , withMutableGen_
  , withSeedMutableGen
  , withSeedMutableGen_
  , randomM
  , randomRM
  , splitGenM
  , splitMutableGenM

  -- ** Deprecated
  , RandomGenM(..)

  -- * Monadic adapters for pure pseudo-random number generators #monadicadapters#
  -- $monadicadapters

  -- ** Pure adapter in 'MonadState'
  , StateGen(..)
  , StateGenM(..)
  , runStateGen
  , runStateGen_
  , runStateGenT
  , runStateGenT_
  , runStateGenST
  , runStateGenST_
  -- ** Mutable thread-safe adapter in 'IO'
  , AtomicGen(..)
  , AtomicGenM(..)
  , newAtomicGenM
  , applyAtomicGen
  , globalStdGen
  -- ** Mutable adapter in 'IO'
  , IOGen(..)
  , IOGenM(..)
  , newIOGenM
  , applyIOGen
  -- ** Mutable adapter in 'ST'
  , STGen(..)
  , STGenM(..)
  , newSTGenM
  , applySTGen
  , runSTGen
  , runSTGen_
  -- ** Mutable thread-safe adapter in 'STM'
  , TGen(..)
  , TGenM(..)
  , newTGenM
  , newTGenMIO
  , applyTGen

  -- * Pseudo-random values of various types
  -- $uniform
  , Uniform(..)
  , uniformViaFiniteM
  , UniformRange(..)
  , isInRangeOrd
  , isInRangeEnum

  -- ** Lists
  , uniformListM
  , uniformListRM
  , uniformShuffleListM

  -- ** Generators for sequences of pseudo-random bytes
  , uniformByteArrayM
  , uniformByteStringM
  , uniformShortByteStringM

  -- * Helper functions for createing instances
  -- ** Sequences of bytes
  , fillByteArrayST
  , genShortByteStringIO
  , genShortByteStringST
  , defaultUnsafeUniformFillMutableByteArray
  -- ** Floating point numbers
  , uniformDouble01M
  , uniformDoublePositive01M
  , uniformFloat01M
  , uniformFloatPositive01M
  -- ** Enum types
  , uniformEnumM
  , uniformEnumRM
  -- ** Word
  , uniformWordR

  -- * Appendix

  -- ** How to implement 'StatefulGen'
  -- $implemenstatefulegen

  -- ** Floating point number caveats #fpcaveats#
  , scaleFloating
  -- $floating

  -- * References
  -- $references
  ) where

import Control.DeepSeq
import Control.Monad.IO.Class
import Control.Monad.ST
import GHC.Conc.Sync (STM, TVar, newTVar, newTVarIO, readTVar, writeTVar)
import Control.Monad.State.Strict (MonadState, state)
import Data.ByteString (ByteString)
import Data.Coerce
import Data.IORef
import Data.STRef
import Foreign.Storable
import System.Random hiding (uniformShortByteString)
import System.Random.Array (shuffleListM, shortByteStringToByteString)
import System.Random.Internal
#if __GLASGOW_HASKELL__ >= 808
import GHC.IORef (atomicModifyIORef2Lazy)
#endif


-- $introduction
--
-- This module provides type classes and instances for the following concepts:
--
-- [Monadic pseudo-random number generators] 'StatefulGen' is an interface to
--     monadic pseudo-random number generators.
--
-- [Monadic adapters] 'StateGenM', 'AtomicGenM', 'IOGenM', 'STGenM` and 'TGenM'
--     turn a 'RandomGen' instance into a 'StatefulGen' instance.
--
-- [Drawing from a range] 'UniformRange' is used to generate a value of a
--     type uniformly within a range.
--
--     This library provides instances of 'UniformRange' for many common
--     numeric types.
--
-- [Drawing from the entire domain of a type] 'Uniform' is used to generate a
--     value of a type uniformly over all possible values of that type.
--
--     This library provides instances of 'Uniform' for many common bounded
--     numeric types.
--
-- $usagemonadic
--
-- In monadic code, use the relevant 'Uniform' and 'UniformRange' instances to
-- generate pseudo-random values via 'uniformM' and 'uniformRM', respectively.
--
-- As an example, @rollsM@ generates @n@ pseudo-random values of @Word@ in the range @[1,
-- 6]@ in a 'StatefulGen' context; given a /monadic/ pseudo-random number generator, you
-- can run this probabilistic computation using
-- [@mwc-random@](https://hackage.haskell.org/package/mwc-random) as follows:
--
-- >>> import Control.Monad (replicateM)
-- >>> :{
-- let rollsM :: StatefulGen g m => Int -> g -> m [Word]
--     rollsM n = replicateM n . uniformRM (1, 6)
-- :}
--
-- > import qualified System.Random.MWC as MWC
-- > >>> monadicGen <- MWC.create
-- > >>> rollsM 10 monadicGen :: IO [Word]
-- > [3,4,3,1,4,6,1,6,1,4]
--
-- Given a /pure/ pseudo-random number generator, you can run the monadic pseudo-random
-- number computation @rollsM@ in 'Control.Monad.State.Strict.StateT', 'IO', 'ST' or 'STM'
-- context by applying a monadic adapter like 'StateGenM', 'AtomicGenM', 'IOGenM',
-- 'STGenM' or 'TGenM' (see [monadic-adapters](#monadicadapters)) to the pure
-- pseudo-random number generator.
--
-- >>> let pureGen = mkStdGen 42
-- >>> newIOGenM pureGen >>= rollsM 10 :: IO [Word]
-- [1,1,3,2,4,5,3,4,6,2]

-------------------------------------------------------------------------------
-- Pseudo-random number generator interfaces
-------------------------------------------------------------------------------

-- $interfaces
--
-- Pseudo-random number generators come in two flavours: /pure/ and /monadic/.
--
-- ['System.Random.RandomGen': pure pseudo-random number generators]
--     See "System.Random" module.
--
-- ['StatefulGen': monadic pseudo-random number generators] These generators mutate their
--     own state as they produce pseudo-random values. They generally live in
--     'Control.Monad.State.Strict.StateT', 'ST', 'IO' or 'STM' or some other transformer
--     on top of those monads.
--

-------------------------------------------------------------------------------
-- Monadic adapters
-------------------------------------------------------------------------------

-- $monadicadapters
--
-- Pure pseudo-random number generators can be used in monadic code via the
-- adapters 'StateGenM', 'AtomicGenM', 'IOGenM', 'STGenM' and 'TGenM'
--
-- * 'StateGenM' can be used in any state monad. With strict
--     'Control.Monad.State.Strict.StateT' there is no performance overhead compared to
--     using the 'RandomGen' instance directly. 'StateGenM' is /not/ safe to use in the
--     presence of exceptions and concurrency.
--
-- *   'AtomicGenM' is safe in the presence of exceptions and concurrency since
--     it performs all actions atomically.
--
-- *   'IOGenM' is a wrapper around an 'IORef' that holds a pure generator.
--     'IOGenM' is safe in the presence of exceptions, but not concurrency.
--
-- *   'STGenM' is a wrapper around an 'STRef' that holds a pure generator.
--     'STGenM' is safe in the presence of exceptions, but not concurrency.
--
-- *   'TGenM' is a wrapper around a 'TVar' that holds a pure generator. 'TGenM'
--     can be used in a software transactional memory monad 'STM`. It is not as
--     performant as 'AtomicGenM`, but it can provide stronger guarantees in a
--     concurrent setting.

-- | Interface to operations on 'RandomGen' wrappers like 'IOGenM' and 'StateGenM'.
--
-- @since 1.2.0
class (RandomGen r, StatefulGen g m) => RandomGenM g r m | g -> r where
  applyRandomGenM :: (r -> (a, r)) -> g -> m a
{-# DEPRECATED applyRandomGenM "In favor of `modifyGen`" #-}
{-# DEPRECATED RandomGenM "In favor of `FrozenGen`" #-}

instance (RandomGen r, MonadIO m) => RandomGenM (IOGenM r) r m where
  applyRandomGenM = applyIOGen

instance (RandomGen r, MonadIO m) => RandomGenM (AtomicGenM r) r m where
  applyRandomGenM = applyAtomicGen

instance (RandomGen r, MonadState r m) => RandomGenM (StateGenM r) r m where
  applyRandomGenM f _ = state f

instance RandomGen r => RandomGenM (STGenM r s) r (ST s) where
  applyRandomGenM = applySTGen

instance RandomGen r => RandomGenM (TGenM r) r STM where
  applyRandomGenM = applyTGen


-- | Shuffle elements of a list in a uniformly random order.
--
-- ====__Examples__
--
-- >>> import System.Random.Stateful
-- >>> runStateGen_ (mkStdGen 127) $ uniformShuffleListM "ELVIS"
-- "LIVES"
--
-- @since 1.3.0
uniformShuffleListM :: StatefulGen g m => [a] -> g -> m [a]
uniformShuffleListM xs gen = shuffleListM (`uniformWordR` gen) xs
{-# INLINE uniformShuffleListM #-}

-- | Runs a mutable pseudo-random number generator from its 'FrozenGen' state.
--
-- ====__Examples__
--
-- >>> import Data.Int (Int8)
-- >>> withMutableGen (IOGen (mkStdGen 217)) (uniformListM 5) :: IO ([Int8], IOGen StdGen)
-- ([-74,37,-50,-2,3],IOGen {unIOGen = StdGen {unStdGen = SMGen 4273268533320920145 15251669095119325999}})
--
-- @since 1.2.0
withMutableGen :: ThawedGen f m => f -> (MutableGen f m -> m a) -> m (a, f)
withMutableGen fg action = do
  g <- thawGen fg
  res <- action g
  fg' <- freezeGen g
  pure (res, fg')

-- | Same as 'withMutableGen', but only returns the generated value.
--
-- ====__Examples__
--
-- >>> import System.Random.Stateful
-- >>> let pureGen = mkStdGen 137
-- >>> withMutableGen_ (IOGen pureGen) (uniformRM (1 :: Int, 6 :: Int))
-- 4
--
-- @since 1.2.0
withMutableGen_ :: ThawedGen f m => f -> (MutableGen f m -> m a) -> m a
withMutableGen_ fg action = thawGen fg >>= action


-- | Just like `withMutableGen`, except uses a `Seed` instead of a frozen generator.
--
-- ====__Examples__
--
-- Here is good example of how `withSeedMutableGen` can be used with `withSeedFile`, which uses a locally stored seed.
--
-- First we define a @reportSeed@ function that will print the contents of a seed file as a list of bytes:
--
-- >>> import Data.ByteString as BS (readFile, writeFile, unpack)
-- >>> :seti -XOverloadedStrings
-- >>> let reportSeed fp = print . ("Seed: " <>) . show . BS.unpack =<< BS.readFile fp
--
-- Given a file path, write an `StdGen` seed into the file:
--
-- >>> :seti -XFlexibleContexts -XScopedTypeVariables
-- >>> let writeInitSeed fp = BS.writeFile fp (unSeedToByteString (toSeed (mkStdGen 2025)))
--
-- Apply a `StatefulGen` monadic action that uses @`IOGen` `StdGen`@, restored from the seed in the given path:
--
-- >>> let withMutableSeedFile fp action = withSeedFile fp (\(seed :: Seed (IOGen StdGen)) -> withSeedMutableGen seed action)
--
-- Given a path and an action initialize the seed file and apply the action using that seed:
--
-- >>> let withInitSeedFile fp action = writeInitSeed fp *> reportSeed fp *> withMutableSeedFile fp action <* reportSeed fp
--
-- For the sake of example we will use a temporary directory for storing the seed. Here we
-- report the contents of the seed file before and after we shuffle a list:
--
-- >>> import UnliftIO.Temporary (withSystemTempDirectory)
-- >>> withSystemTempDirectory "random" (\fp -> withInitSeedFile (fp ++ "/seed.bin") (uniformShuffleListM [1..10]))
-- "Seed: [183,178,143,77,132,163,109,14,157,105,82,99,148,82,109,173]"
-- "Seed: [60,105,117,203,187,138,69,39,157,105,82,99,148,82,109,173]"
-- [7,5,4,3,1,8,10,6,9,2]
--
-- @since 1.3.0
withSeedMutableGen :: (SeedGen g, ThawedGen g m) => Seed g -> (MutableGen g m -> m a) -> m (a, Seed g)
withSeedMutableGen seed f = withSeedM seed (`withMutableGen` f)

-- | Just like `withSeedMutableGen`, except it doesn't return the final generator, only
-- the resulting value. This is slightly more efficient, since it doesn't incur overhead
-- from freezeing the mutable generator
--
-- @since 1.3.0
withSeedMutableGen_ :: (SeedGen g, ThawedGen g m) => Seed g -> (MutableGen g m -> m a) -> m a
withSeedMutableGen_ seed = withMutableGen_ (fromSeed seed)


-- | Generates a pseudo-random value using monadic interface and `Random` instance.
--
-- ====__Examples__
--
-- >>> import System.Random.Stateful
-- >>> let pureGen = mkStdGen 139
-- >>> g <- newIOGenM pureGen
-- >>> randomM g :: IO Double
-- 0.33775117339631733
--
-- You can use type applications to disambiguate the type of the generated numbers:
--
-- >>> :seti -XTypeApplications
-- >>> randomM @Double g
-- 0.9156875994165681
--
-- @since 1.2.0
randomM :: forall a g m. (Random a, RandomGen g, FrozenGen g m) => MutableGen g m -> m a
randomM = flip modifyGen random
{-# INLINE randomM #-}

-- | Generates a pseudo-random value using monadic interface and `Random` instance.
--
-- ====__Examples__
--
-- >>> import System.Random.Stateful
-- >>> let pureGen = mkStdGen 137
-- >>> g <- newIOGenM pureGen
-- >>> randomRM (1, 100) g :: IO Int
-- 52
--
-- You can use type applications to disambiguate the type of the generated numbers:
--
-- >>> :seti -XTypeApplications
-- >>> randomRM @Int (1, 100) g
-- 2
--
-- @since 1.2.0
randomRM :: forall a g m. (Random a, RandomGen g, FrozenGen g m) => (a, a) -> MutableGen g m -> m a
randomRM r = flip modifyGen (randomR r)
{-# INLINE randomRM #-}

-- | Generates a pseudo-random 'ByteString' of the specified size.
--
-- @since 1.2.0
uniformByteStringM :: StatefulGen g m => Int -> g -> m ByteString
uniformByteStringM n g =
  shortByteStringToByteString . byteArrayToShortByteString
    <$> uniformByteArrayM True n g
{-# INLINE uniformByteStringM #-}

-- | Wraps an 'IORef' that holds a pure pseudo-random number generator. All
-- operations are performed atomically.
--
-- *   'AtomicGenM' is safe in the presence of exceptions and concurrency.
-- *   'AtomicGenM' is the slowest of the monadic adapters due to the overhead
--     of its atomic operations.
--
-- @since 1.2.0
newtype AtomicGenM g = AtomicGenM { unAtomicGenM :: IORef g}


-- | Frozen version of mutable `AtomicGenM` generator
--
-- @since 1.2.0
newtype AtomicGen g = AtomicGen { unAtomicGen :: g}
  deriving (Eq, Ord, Show, RandomGen, SplitGen, Storable, NFData)

-- Standalone definition due to GHC-8.0 not supporting deriving with associated type families
instance SeedGen g => SeedGen (AtomicGen g) where
  type SeedSize (AtomicGen g) = SeedSize g
  fromSeed = coerce (fromSeed :: Seed g -> g)
  toSeed = coerce (toSeed :: g -> Seed g)

-- | Creates a new 'AtomicGenM'.
--
-- @since 1.2.0
newAtomicGenM :: MonadIO m => g -> m (AtomicGenM g)
newAtomicGenM = fmap AtomicGenM . liftIO . newIORef


-- | Global mutable standard pseudo-random number generator. This is the same
-- generator that was historically used by `randomIO` and `randomRIO` functions.
--
-- >>> import Control.Monad (replicateM)
-- >>> replicateM 10 (uniformRM ('a', 'z') globalStdGen)
-- "tdzxhyfvgr"
--
-- @since 1.2.1
globalStdGen :: AtomicGenM StdGen
globalStdGen = AtomicGenM theStdGen


instance (RandomGen g, MonadIO m) => StatefulGen (AtomicGenM g) m where
  uniformWord32R r = applyAtomicGen (genWord32R r)
  {-# INLINE uniformWord32R #-}
  uniformWord64R r = applyAtomicGen (genWord64R r)
  {-# INLINE uniformWord64R #-}
  uniformWord8 = applyAtomicGen genWord8
  {-# INLINE uniformWord8 #-}
  uniformWord16 = applyAtomicGen genWord16
  {-# INLINE uniformWord16 #-}
  uniformWord32 = applyAtomicGen genWord32
  {-# INLINE uniformWord32 #-}
  uniformWord64 = applyAtomicGen genWord64
  {-# INLINE uniformWord64 #-}


instance (RandomGen g, MonadIO m) => FrozenGen (AtomicGen g) m where
  type MutableGen (AtomicGen g) m = AtomicGenM g
  freezeGen = fmap AtomicGen . liftIO . readIORef . unAtomicGenM
  modifyGen (AtomicGenM ioRef) f =
    liftIO $ atomicModifyIORefHS ioRef $ \g ->
      case f (AtomicGen g) of
        (a, AtomicGen g') -> (g', a)
  {-# INLINE modifyGen #-}

instance (RandomGen g, MonadIO m) => ThawedGen (AtomicGen g) m where
  thawGen (AtomicGen g) = newAtomicGenM g

-- | Atomically applies a pure operation to the wrapped pseudo-random number
-- generator.
--
-- ====__Examples__
--
-- >>> import System.Random.Stateful
-- >>> let pureGen = mkStdGen 137
-- >>> g <- newAtomicGenM pureGen
-- >>> applyAtomicGen random g :: IO Int
-- 7879794327570578227
--
-- @since 1.2.0
applyAtomicGen :: MonadIO m => (g -> (a, g)) -> AtomicGenM g -> m a
applyAtomicGen op (AtomicGenM gVar) =
  liftIO $ atomicModifyIORefHS gVar $ \g ->
    case op g of
      (a, g') -> (g', a)
{-# INLINE applyAtomicGen #-}

-- HalfStrict version of atomicModifyIORef, i.e. strict in the modifcation of the contents
-- of the IORef, but not in the result produced.
atomicModifyIORefHS :: IORef a -> (a -> (a, b)) -> IO b
atomicModifyIORefHS ref f = do
#if __GLASGOW_HASKELL__ >= 808
  (_old, (_new, res)) <- atomicModifyIORef2Lazy ref $ \old ->
    case f old of
      r@(!_new, _res) -> r
  pure res
#else
  atomicModifyIORef ref $ \old ->
    case f old of
      r@(!_new, _res) -> r
#endif
{-# INLINE atomicModifyIORefHS #-}

-- | Wraps an 'IORef' that holds a pure pseudo-random number generator.
--
-- *   'IOGenM' is safe in the presence of exceptions, but not concurrency.
-- *   'IOGenM' is slower than 'StateGenM' due to the extra pointer indirection.
-- *   'IOGenM' is faster than 'AtomicGenM' since the 'IORef' operations used by
--     'IOGenM' are not atomic.
--
-- An example use case is writing pseudo-random bytes into a file:
--
-- >>> import UnliftIO.Temporary (withSystemTempFile)
-- >>> import Data.ByteString (hPutStr)
-- >>> let ioGen g = withSystemTempFile "foo.bin" $ \_ h -> uniformRM (0, 100) g >>= flip uniformByteStringM g >>= hPutStr h
--
-- and then run it:
--
-- >>> newIOGenM (mkStdGen 1729) >>= ioGen
--
-- @since 1.2.0
newtype IOGenM g = IOGenM { unIOGenM :: IORef g }

-- | Frozen version of mutable `IOGenM` generator
--
-- @since 1.2.0
newtype IOGen g = IOGen { unIOGen :: g }
  deriving (Eq, Ord, Show, RandomGen, SplitGen, Storable, NFData)

-- Standalone definition due to GHC-8.0 not supporting deriving with associated type families
instance SeedGen g => SeedGen (IOGen g) where
  type SeedSize (IOGen g) = SeedSize g
  fromSeed = coerce (fromSeed :: Seed g -> g)
  toSeed = coerce (toSeed :: g -> Seed g)

-- | Creates a new 'IOGenM'.
--
-- @since 1.2.0
newIOGenM :: MonadIO m => g -> m (IOGenM g)
newIOGenM = fmap IOGenM . liftIO . newIORef



instance (RandomGen g, MonadIO m) => StatefulGen (IOGenM g) m where
  uniformWord32R r = applyIOGen (genWord32R r)
  {-# INLINE uniformWord32R #-}
  uniformWord64R r = applyIOGen (genWord64R r)
  {-# INLINE uniformWord64R #-}
  uniformWord8 = applyIOGen genWord8
  {-# INLINE uniformWord8 #-}
  uniformWord16 = applyIOGen genWord16
  {-# INLINE uniformWord16 #-}
  uniformWord32 = applyIOGen genWord32
  {-# INLINE uniformWord32 #-}
  uniformWord64 = applyIOGen genWord64
  {-# INLINE uniformWord64 #-}


instance (RandomGen g, MonadIO m) => FrozenGen (IOGen g) m where
  type MutableGen (IOGen g) m = IOGenM g
  freezeGen = fmap IOGen . liftIO . readIORef . unIOGenM
  modifyGen (IOGenM ref) f = liftIO $ do
    g <- readIORef ref
    let (a, IOGen g') = f (IOGen g)
    g' `seq` writeIORef ref g'
    pure a
  {-# INLINE modifyGen #-}
  overwriteGen (IOGenM ref) = liftIO . writeIORef ref . unIOGen
  {-# INLINE overwriteGen #-}

instance (RandomGen g, MonadIO m) => ThawedGen (IOGen g) m where
  thawGen (IOGen g) = newIOGenM g

-- | Applies a pure operation to the wrapped pseudo-random number generator.
--
-- ====__Examples__
--
-- >>> import System.Random.Stateful
-- >>> let pureGen = mkStdGen 137
-- >>> g <- newIOGenM pureGen
-- >>> applyIOGen random g :: IO Int
-- 7879794327570578227
--
-- @since 1.2.0
applyIOGen :: MonadIO m => (g -> (a, g)) -> IOGenM g -> m a
applyIOGen f (IOGenM ref) = liftIO $ do
  g <- readIORef ref
  case f g of
    (a, !g') -> a <$ writeIORef ref g'
{-# INLINE applyIOGen #-}

-- | Wraps an 'STRef' that holds a pure pseudo-random number generator.
--
-- *   'STGenM' is safe in the presence of exceptions, but not concurrency.
-- *   'STGenM' is slower than 'StateGenM' due to the extra pointer indirection.
--
-- @since 1.2.0
newtype STGenM g s = STGenM { unSTGenM :: STRef s g }

-- | Frozen version of mutable `STGenM` generator
--
-- @since 1.2.0
newtype STGen g = STGen { unSTGen :: g }
  deriving (Eq, Ord, Show, RandomGen, SplitGen, Storable, NFData)

-- Standalone definition due to GHC-8.0 not supporting deriving with associated type families
instance SeedGen g => SeedGen (STGen g) where
  type SeedSize (STGen g) = SeedSize g
  fromSeed = coerce (fromSeed :: Seed g -> g)
  toSeed = coerce (toSeed :: g -> Seed g)

-- | Creates a new 'STGenM'.
--
-- @since 1.2.0
newSTGenM :: g -> ST s (STGenM g s)
newSTGenM = fmap STGenM . newSTRef


instance RandomGen g => StatefulGen (STGenM g s) (ST s) where
  uniformWord32R r = applySTGen (genWord32R r)
  {-# INLINE uniformWord32R #-}
  uniformWord64R r = applySTGen (genWord64R r)
  {-# INLINE uniformWord64R #-}
  uniformWord8 = applySTGen genWord8
  {-# INLINE uniformWord8 #-}
  uniformWord16 = applySTGen genWord16
  {-# INLINE uniformWord16 #-}
  uniformWord32 = applySTGen genWord32
  {-# INLINE uniformWord32 #-}
  uniformWord64 = applySTGen genWord64
  {-# INLINE uniformWord64 #-}

instance RandomGen g => FrozenGen (STGen g) (ST s) where
  type MutableGen (STGen g) (ST s) = STGenM g s
  freezeGen = fmap STGen . readSTRef . unSTGenM
  modifyGen (STGenM ref) f = do
    g <- readSTRef ref
    let (a, STGen g') = f (STGen g)
    g' `seq` writeSTRef ref g'
    pure a
  {-# INLINE modifyGen #-}
  overwriteGen (STGenM ref) = writeSTRef ref . unSTGen
  {-# INLINE overwriteGen #-}

instance RandomGen g => ThawedGen (STGen g) (ST s) where
  thawGen (STGen g) = newSTGenM g


-- | Applies a pure operation to the wrapped pseudo-random number generator.
--
-- ====__Examples__
--
-- >>> import System.Random.Stateful
-- >>> let pureGen = mkStdGen 137
-- >>> (runSTGen pureGen (\g -> applySTGen random g)) :: (Int, StdGen)
-- (7879794327570578227,StdGen {unStdGen = SMGen 11285859549637045894 7641485672361121627})
--
-- @since 1.2.0
applySTGen :: (g -> (a, g)) -> STGenM g s -> ST s a
applySTGen f (STGenM ref) = do
  g <- readSTRef ref
  case f g of
    (a, !g') -> a <$ writeSTRef ref g'
{-# INLINE applySTGen #-}

-- | Runs a monadic generating action in the `ST` monad using a pure
-- pseudo-random number generator.
--
-- ====__Examples__
--
-- >>> import System.Random.Stateful
-- >>> let pureGen = mkStdGen 137
-- >>> (runSTGen pureGen (\g -> applySTGen random g)) :: (Int, StdGen)
-- (7879794327570578227,StdGen {unStdGen = SMGen 11285859549637045894 7641485672361121627})
--
-- @since 1.2.0
runSTGen :: RandomGen g => g -> (forall s . STGenM g s -> ST s a) -> (a, g)
runSTGen g action = unSTGen <$> runST (withMutableGen (STGen g) action)

-- | Runs a monadic generating action in the `ST` monad using a pure
-- pseudo-random number generator. Returns only the resulting pseudo-random
-- value.
--
-- ====__Examples__
--
-- >>> import System.Random.Stateful
-- >>> let pureGen = mkStdGen 137
-- >>> (runSTGen_ pureGen (\g -> applySTGen random g)) :: Int
-- 7879794327570578227
--
-- @since 1.2.0
runSTGen_ :: RandomGen g => g -> (forall s . STGenM g s -> ST s a) -> a
runSTGen_ g action = fst $ runSTGen g action


-- | Wraps a 'TVar' that holds a pure pseudo-random number generator.
--
-- @since 1.2.1
newtype TGenM g = TGenM { unTGenM :: TVar g }

-- | Frozen version of mutable `TGenM` generator
--
-- @since 1.2.1
newtype TGen g = TGen { unTGen :: g }
  deriving (Eq, Ord, Show, RandomGen, SplitGen, Storable, NFData)

-- Standalone definition due to GHC-8.0 not supporting deriving with associated type families
instance SeedGen g => SeedGen (TGen g) where
  type SeedSize (TGen g) = SeedSize g
  fromSeed = coerce (fromSeed :: Seed g -> g)
  toSeed = coerce (toSeed :: g -> Seed g)

-- | Creates a new 'TGenM' in `STM`.
--
-- @since 1.2.1
newTGenM :: g -> STM (TGenM g)
newTGenM = fmap TGenM . newTVar


-- | Creates a new 'TGenM' in `IO`.
--
-- @since 1.2.1
newTGenMIO :: MonadIO m => g -> m (TGenM g)
newTGenMIO g = liftIO (TGenM <$> newTVarIO g)


-- | @since 1.2.1
instance RandomGen g => StatefulGen (TGenM g) STM where
  uniformWord32R r = applyTGen (genWord32R r)
  {-# INLINE uniformWord32R #-}
  uniformWord64R r = applyTGen (genWord64R r)
  {-# INLINE uniformWord64R #-}
  uniformWord8 = applyTGen genWord8
  {-# INLINE uniformWord8 #-}
  uniformWord16 = applyTGen genWord16
  {-# INLINE uniformWord16 #-}
  uniformWord32 = applyTGen genWord32
  {-# INLINE uniformWord32 #-}
  uniformWord64 = applyTGen genWord64
  {-# INLINE uniformWord64 #-}

-- | @since 1.2.1
instance RandomGen g => FrozenGen (TGen g) STM where
  type MutableGen (TGen g) STM = TGenM g
  freezeGen = fmap TGen . readTVar . unTGenM
  modifyGen (TGenM ref) f = do
    g <- readTVar ref
    let (a, TGen g') = f (TGen g)
    g' `seq` writeTVar ref g'
    pure a
  {-# INLINE modifyGen #-}
  overwriteGen (TGenM ref) = writeTVar ref . unTGen
  {-# INLINE overwriteGen #-}

instance RandomGen g => ThawedGen (TGen g) STM where
  thawGen (TGen g) = newTGenM g


-- | Applies a pure operation to the wrapped pseudo-random number generator.
--
-- ====__Examples__
--
-- >>> import Control.Concurrent.STM
-- >>> import System.Random.Stateful
-- >>> import Data.Int (Int32)
-- >>> let pureGen = mkStdGen 137
-- >>> stmGen <- newTGenMIO pureGen
-- >>> atomically $ applyTGen uniform stmGen :: IO Int32
-- 637238067
--
-- @since 1.2.1
applyTGen :: (g -> (a, g)) -> TGenM g -> STM a
applyTGen f (TGenM tvar) = do
  g <- readTVar tvar
  case f g of
    (a, !g') -> a <$ writeTVar tvar g'
{-# INLINE applyTGen #-}



class UniformLazy a where
  uniformLazyM :: (SplitGen f, FrozenGen f m) => MutableGen f m -> m a
  default uniformLazyM :: (Uniform a, SplitGen f, FrozenGen f m) => MutableGen f m -> m a
  uniformLazyM g = fst . uniform <$> splitGenM g

class UniformRangeLazy a where
  uniformLazyRM :: (SplitGen f, FrozenGen f m) => (a, a) -> MutableGen f m -> m a
  default uniformLazyRM :: (UniformRange a, SplitGen f, FrozenGen f m) => (a, a) -> MutableGen f m -> m a
  uniformLazyRM r g = fst . uniformR r <$> splitGenM g

instance UniformLazy Int

-- $uniform
--
-- This library provides two type classes to generate pseudo-random values:
--
-- *   'UniformRange' is used to generate a value of a type uniformly within a
--     range.
-- *   'Uniform' is used to generate a value of a type uniformly over all
--     possible values of that type.
--
-- Types may have instances for both or just one of 'UniformRange' and
-- 'Uniform'. A few examples illustrate this:
--
-- *   'Int', 'Data.Word.Word16' and 'Bool' are instances of both 'UniformRange' and
--     'Uniform'.
-- *   'Integer', 'Float' and 'Double' each have an instance for 'UniformRange'
--     but no 'Uniform' instance.
-- *   A hypothetical type @Radian@ representing angles by taking values in the
--     range @[0, 2π)@ has a trivial 'Uniform' instance, but no 'UniformRange'
--     instance: the problem is that two given @Radian@ values always span /two/
--     ranges, one clockwise and one anti-clockwise.
-- *   It is trivial to construct a @Uniform (a, b)@ instance given
--     @Uniform a@ and @Uniform b@ (and this library provides this tuple
--     instance).
-- *   On the other hand, there is no correct way to construct a
--     @UniformRange (a, b)@ instance based on just @UniformRange a@ and
--     @UniformRange b@.

-------------------------------------------------------------------------------
-- Notes
-------------------------------------------------------------------------------

-- $floating
--
-- Due to rounding errors, floating point operations are neither associative nor
-- distributive the way the corresponding operations on real numbers are. Additionally,
-- floating point numbers admit special values @NaN@ as well as negative and positive
-- infinity.
--
-- The 'UniformRange' instances for 'Float' and 'Double' use the following
-- procedure to generate a random value in a range for @uniformRM (l, h) g@:
--
-- * If @__l == h__@, return: @__l__@.
-- * If @__`isInfinite` l == True__@ or @__`isInfinite` h == True__@, return: @__l + h__@
-- * Otherwise:
--
--     1.  Generate an unsigned integral of matching width @__w__@ uniformly.
--
--     2.  Check whether @__h - l__@ overflows to infinity and, if it does, then convert
--         @__w__@ to a floating point number in @__[0.0, 1.0]__@ range through division
--         of @__w__@ by the highest possible value:
--
--         @
--         x = `fromIntegral` w / `fromIntegral` `maxBound`
--         @
--
--         Then we scale and clamp it before returning it:
--
--         @
--         `max` (`min` (x * l + (1 - x) * h) (`max` l h)) (`min` l h)
--         @
--
--         Clamping is necessary, because otherwise it would be possible to run into a
--         degenerate case when a scaled value is outside the specified range due to
--         rounding errors.
--
--     3.  Whenever @__h - l__@ does not overflow, we use this common formula for scaling:
--         @__ l + (h - l) * x__@.  However, instead of using @__[0.0, 1.0]__@ range we
--         use the top most bit of @__w__@ to decide whether we will treat the generated
--         floating point value as @__[0.0, 0.5]__@ range or @__[0.5, 1.0]__@ range and
--         use the left over bits to produce a floating point value in the half unit
--         range:
--
--         @
--         x = `fromIntegral` (`clearBit` w 31) / `fromIntegral` `maxBound`
--         @
--
--         Further scaling depends on the top most bit:
--
--         @
--         if `testBit` w 31
--            then l + (h - l) * x
--            else h + (l - h) * x
--         @
--
--         Because of this clever technique the result does not need clamping, since
--         scaled values are guaranteed to stay within the specified range. Another reason
--         why this tecnique is used for the common case instead of the one described in
--         @2.@ is because it avoids usage of @__1 - x__@, which consequently reduces loss
--         of randomness due to rounding.
--
--
-- What happens when @__NaN__@ or @__Infinity__@ are given to 'uniformRM'? We first
-- define them as constants:
--
-- >>> nan = read "NaN" :: Float
-- >>> inf = read "Infinity" :: Float
-- >>> g <- newIOGenM (mkStdGen 2024)
--
-- *   If at least one of \(l\) or \(h\) is @__NaN__@, the result is @__NaN__@.
--
--     >>> uniformRM (nan, 1) g
--     NaN
--     >>> uniformRM (-1, nan) g
--     NaN
--
-- *   If \(l\) and \(h\) are both @__Infinity__@ with opposing signs, then the result is @__NaN__@.
--
--     >>> uniformRM (-inf, inf) g
--     NaN
--     >>> uniformRM (inf, -inf) g
--     NaN
--
-- *   Otherwise, if \(l\) is @__Infinity__@ or @__-Infinity__@, the result is \(l\).
--
--     >>> uniformRM (inf, 1) g
--     Infinity
--     >>> uniformRM (-inf, 1) g
--     -Infinity
--
-- *   Otherwise, if \(h\) is @__Infinity__@ or @__-Infinity__@, the result is \(h\).
--
--     >>> uniformRM (1, inf) g
--     Infinity
--     >>> uniformRM (1, -inf) g
--     -Infinity
--
-- Note that the [GCC 10.1.0 C++ standard library](https://gcc.gnu.org/git/?p=gcc.git;a=blob;f=libstdc%2B%2B-v3/include/bits/random.h;h=19307fbc3ca401976ef6823e8fda893e4a263751;hb=63fa67847628e5f358e7e2e7edb8314f0ee31f30#l1859),
-- the [Java 10 standard library](https://docs.oracle.com/javase/10/docs/api/java/util/Random.html#doubles%28double,double%29)
-- and [CPython 3.8](https://github.com/python/cpython/blob/3.8/Lib/random.py#L417)
-- use a similar procedure to generate floating point values in a range.
--
-- $implemenstatefulegen
--
-- Typically, a monadic pseudo-random number generator has facilities to save
-- and restore its internal state in addition to generating pseudo-random numbers.
--
-- Here is an example instance for the monadic pseudo-random number generator
-- from the @mwc-random@ package:
--
-- > import qualified System.Random.MWC as MWC
-- > import qualified Data.Vector.Generic as G
--
-- > instance (s ~ PrimState m, PrimMonad m) => StatefulGen (MWC.Gen s) m where
-- >   uniformWord8 = MWC.uniform
-- >   uniformWord16 = MWC.uniform
-- >   uniformWord32 = MWC.uniform
-- >   uniformWord64 = MWC.uniform
-- >   uniformByteArrayM isPinned n g = stToPrim (fillByteArrayST isPinned n (MWC.uniform g))
--
-- > instance PrimMonad m => FrozenGen MWC.Seed m where
-- >   type MutableGen MWC.Seed m = MWC.Gen (PrimState m)
-- >   freezeGen = MWC.save
-- >   overwriteGen (Gen mv) (Seed v) = G.copy mv v
--
-- > instance PrimMonad m => ThawedGen MWC.Seed m where
-- >   thawGen = MWC.restore
--
-- === @FrozenGen@
--
-- `FrozenGen` gives us ability to use most of stateful pseudo-random number generator in
-- its immutable form, if one exists that is.  The biggest benefit that can be drawn from
-- a polymorphic access to a stateful pseudo-random number generator in a frozen form is
-- the ability to serialize, deserialize and possibly even use the stateful generator in a
-- pure setting without knowing the actual type of a generator ahead of time. For example
-- we can write a function that accepts a frozen state of some pseudo-random number
-- generator and produces a short list with random even integers.
--
-- >>> import Data.Int (Int8)
-- >>> import Control.Monad (replicateM)
-- >>> :{
-- myCustomRandomList :: ThawedGen f m => f -> m [Int8]
-- myCustomRandomList f =
--   withMutableGen_ f $ \gen -> do
--     len <- uniformRM (5, 10) gen
--     replicateM len $ do
--       x <- uniformM gen
--       pure $ if even x then x else x + 1
-- :}
--
-- and later we can apply it to a frozen version of a stateful generator, such as `STGen`:
--
-- >>> print $ runST $ myCustomRandomList (STGen (mkStdGen 217))
-- [-50,-2,4,-8,-58,-40,24,-32,-110,24]
--
-- Alternatively, instead of discarding the final state of the generator, as it happens
-- above, we could have used `withMutableGen`, which together with the result would give
-- us back its frozen form. This would allow us to store the end state of our generator
-- somewhere for the later reuse.
--
--
-- $references
--
-- 1. Guy L. Steele, Jr., Doug Lea, and Christine H. Flood. 2014. Fast
-- splittable pseudorandom number generators. In Proceedings of the 2014 ACM
-- International Conference on Object Oriented Programming Systems Languages &
-- Applications (OOPSLA '14). ACM, New York, NY, USA, 453-472. DOI:
-- <https://doi.org/10.1145/2660193.2660195>

-- $setup
-- >>> writeIORef theStdGen $ mkStdGen 2021
--
-- >>> :seti -XFlexibleContexts
-- >>> :seti -XFlexibleInstances
-- >>> :seti -XMultiParamTypeClasses
-- >>> :seti -XTypeFamilies
-- >>> :seti -XUndecidableInstances
--
--
