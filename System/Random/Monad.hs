{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  System.Random.Monad
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE in the 'random' repository)
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
--
-- This library deals with the common task of pseudo-random number generation.
module System.Random.Monad
  (
  -- * Pure Random Generator
  module System.Random
  -- * Monadic Random Generator
  -- $introduction

  -- * Usage
  -- ** How to generate pseudo-random values in monadic code
  -- $usagemonadic

  -- ** How to generate pseudo-random values in pure code
  -- $usagepure

  -- * Pure and monadic pseudo-random number generator interfaces
  -- $interfaces
  , MonadRandom(..)
  , Frozen(..)
  , runGenM
  , runGenM_
  , RandomGenM(..)
  , splitRandomGenM

  -- * Monadic adapters for pure pseudo-random number generators
  -- $monadicadapters

  -- ** Pure adapter
  , PureGen
  , splitGen
  , genRandom
  , runGenState
  , runGenState_
  , runGenStateT
  , runGenStateT_
  , runPureGenST
  -- ** Mutable adapter with atomic operations
  , AtomicGen
  , applyAtomicGen
  -- ** Mutable adapter in 'IO'
  , IOGen
  , applyIOGen
  -- ** Mutable adapter in 'ST'
  , STGen
  , applySTGen
  , runSTGen
  , runSTGen_

  -- * Pseudo-random values of various types
  -- $uniform
  , Uniform(..)
  , uniformListM
  , UniformRange(..)

  -- * Generators for sequences of pseudo-random bytes
  , genShortByteStringIO
  , genShortByteStringST
  , uniformByteString

  -- * Notes for pseudo-random number generator implementors

  -- ** How to implement 'MonadRandom'
  -- $implementmonadrandom

  -- * References
  -- $references
  ) where

import Control.Monad.IO.Class
import Control.Monad.ST
import Control.Monad.State.Strict
import Data.IORef
import Data.STRef
import System.Random
import System.Random.Internal

-- $introduction
--
-- This module provides type classes and instances for the following concepts:
--
-- [Monadic pseudo-random number generators] 'MonadRandom' is an interface to
--     monadic pseudo-random number generators.
--
-- [Monadic adapters] 'PureGen', 'AtomicGen', 'IOGen' and 'STGen' turn a
--     'RandomGen' instance into a 'MonadRandom' instance.
--
-- [Drawing from a range] 'UniformRange' is used to generate a value of a
--     datatype uniformly within an inclusive range.
--
--     This library provides instances of 'UniformRange' for many common
--     numeric datatypes.
--
-- [Drawing from the entire domain of a type] 'Uniform' is used to generate a
--     value of a datatype uniformly over all possible values of that datatype.
--
--     This library provides instances of 'Uniform' for many common bounded
--     numeric datatypes.
--
-- $usagemonadic
--
-- In monadic code, use the relevant 'Uniform' and 'UniformRange' instances to
-- generate pseudo-random values via 'uniformM' and 'uniformRM', respectively.
--
-- As an example, @rolls@ generates @n@ pseudo-random values of @Word8@ in the
-- range @[1, 6]@.
--
-- > rolls :: MonadRandom g s m => Int -> g s -> m [Word8]
-- > rolls n = replicateM n . uniformR (1, 6)
--
-- Given a /monadic/ pseudo-random number generator, you can run this
-- probabilistic computation as follows:
--
-- >>> monadicGen <- MWC.create
-- >>> rolls 10 monadicGen :: IO [Word8]
-- [2,3,6,6,4,4,3,1,5,4]
--
-- Given a /pure/ pseudo-random number generator, you can run it in an 'IO' or
-- 'ST' context by first applying a monadic adapter like 'AtomicGen', 'IOGen'
-- or 'STGen' and then running it with 'runGenM'.
--
-- >>> let pureGen = mkStdGen 42
-- >>> runGenM_ (IOGen pureGen) (rolls 10) :: IO [Word8]
-- [1,1,3,2,4,5,3,4,6,2]
--
-- $usagepure
--
-- In pure code, use 'runGenState' and its variants to extract the pure
-- pseudo-random value from a monadic computation based on a pure pseudo-random
-- number generator.
--
-- >>> let pureGen = mkStdGen 42
-- >>> runGenState_ pureGen (rolls 10) :: [Word8]
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
-- ['MonadRandom': monadic pseudo-random number generators] These generators
--     mutate their own state as they produce pseudo-random values. They
--     generally live in 'ST' or 'IO' or some transformer that implements
--     @PrimMonad@.
--

-------------------------------------------------------------------------------
-- Monadic adapters
-------------------------------------------------------------------------------

-- $monadicadapters
--
-- Pure pseudo-random number generators can be used in monadic code via the
-- adapters 'PureGen', 'AtomicGen', 'IOGen' and 'STGen'.
--
-- *   'PureGen' can be used in any state monad. With strict 'StateT' there is
--     no performance overhead compared to using the 'RandomGen' instance
--     directly. 'PureGen' is /not/ safe to use in the presence of exceptions
--     and concurrency.
--
-- *   'AtomicGen' is safe in the presence of exceptions and concurrency since
--     it performs all actions atomically.
--
-- *   'IOGen' is a wrapper around an 'IORef' that holds a pure generator.
--     'IOGen' is safe in the presence of exceptions, but not concurrency.
--
-- *   'STGen' is a wrapper around an 'STRef' that holds a pure generator.
--     'STGen' is safe in the presence of exceptions, but not concurrency.

-- | Interface to operations on 'RandomGen' wrappers like 'IOGen' and 'PureGen'.
--
-- @since 1.2
class (RandomGen r, MonadRandom (g r) s m) => RandomGenM g r s m where
  applyRandomGenM :: (r -> (a, r)) -> g r s -> m a

-- | Splits a pseudo-random number generator into two. Overwrites the mutable
-- wrapper with one of the resulting generators and returns the other.
--
-- @since 1.2
splitRandomGenM :: RandomGenM g r s m => g r s -> m r
splitRandomGenM = applyRandomGenM split

instance (RandomGen r, MonadIO m) => RandomGenM IOGen r RealWorld m where
  applyRandomGenM = applyIOGen

instance (RandomGen r, MonadIO m) => RandomGenM AtomicGen r RealWorld m where
  applyRandomGenM = applyAtomicGen

instance (RandomGen r, MonadState r m) => RandomGenM PureGen r r m where
  applyRandomGenM f _ = state f

instance RandomGen r => RandomGenM STGen r s (ST s) where
  applyRandomGenM = applySTGen

-- | Runs a mutable pseudo-random number generator from its 'Frozen' state.
--
-- >>> import Data.Int (Int8)
-- >>> runGenM (IOGen (mkStdGen 217)) (`uniformListM` 5) :: IO ([Int8], Frozen (IOGen StdGen))
-- ([-74,37,-50,-2,3],IOGen {unIOGen = SMGen 4273268533320920145 15251669095119325999})
--
-- @since 1.2
runGenM :: MonadRandom g s m => Frozen g -> (g s -> m a) -> m (a, Frozen g)
runGenM fg action = do
  g <- thawGen fg
  res <- action g
  fg' <- freezeGen g
  pure (res, fg')

-- | Same as 'runGenM', but only returns the generated value.
--
-- @since 1.2
runGenM_ :: MonadRandom g s m => Frozen g -> (g s -> m a) -> m a
runGenM_ fg action = fst <$> runGenM fg action

-- | Generates a list of pseudo-random values.
--
-- @since 1.2
uniformListM :: (MonadRandom g s m, Uniform a) => g s -> Int -> m [a]
uniformListM gen n = replicateM n (uniformM gen)

-- | Generates a pseudo-random value in a state monad.
--
-- @since 1.2
genRandom :: (RandomGen g, Random a, MonadState g m) => PureGen g g -> m a
genRandom _ = state random

-- | This is a wrapper around pure generator that can be used in a monadic
-- environment. It is safe in presence of exceptions and concurrency since all
-- operations are performed atomically.
--
-- @since 1.2
newtype AtomicGen g s = AtomicGenI (IORef g)

instance (RandomGen g, MonadIO m) => MonadRandom (AtomicGen g) RealWorld m where
  newtype Frozen (AtomicGen g) = AtomicGen { unAtomicGen :: g }
    deriving (Eq, Show, Read)
  thawGen (AtomicGen g) = fmap AtomicGenI (liftIO $ newIORef g)
  freezeGen (AtomicGenI gVar) = fmap AtomicGen (liftIO $ readIORef gVar)
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
  uniformShortByteString n = applyAtomicGen (genShortByteString n)

-- | Atomically applies a pure operation to the wrapped pseudo-random number
-- generator.
--
-- @since 1.2
applyAtomicGen :: MonadIO m => (g -> (a, g)) -> AtomicGen g RealWorld -> m a
applyAtomicGen op (AtomicGenI gVar) =
  liftIO $ atomicModifyIORef' gVar $ \g ->
    case op g of
      (a, g') -> (g', a)
{-# INLINE applyAtomicGen #-}

-- | This is a wrapper around an @IORef@ that holds a pure generator. Because of
-- extra pointer indirection it will be slightly slower than if `PureGen` is
-- being used, but faster than `AtomicGen` wrapper, since atomic modification is
-- not being used with `IOGen`. Which also means that it is not safe in a
-- concurrent setting.
--
-- Both `IOGen` and `AtomicGen` are necessary when generation of pseudo-random
-- values happens in `IO` and especially when dealing with exception handling
-- and resource allocation, which is where `StateT` should never be used. For
-- example writing a pseudo-random number of bytes into a temporary file:
--
-- >>> import UnliftIO.Temporary (withSystemTempFile)
-- >>> import Data.ByteString (hPutStr)
-- >>> let ioGen g = withSystemTempFile "foo.bin" $ \_ h -> uniformRM (0, 100) g >>= flip uniformByteString g >>= hPutStr h
--
-- and then run it:
--
-- >>> runGenM_ (IOGen (mkStdGen 1729)) ioGen
--
-- @since 1.2
newtype IOGen g s = IOGenI (IORef g)

instance (RandomGen g, MonadIO m) => MonadRandom (IOGen g) RealWorld m where
  newtype Frozen (IOGen g) = IOGen { unIOGen :: g }
    deriving (Eq, Show, Read)
  thawGen (IOGen g) = fmap IOGenI (liftIO $ newIORef g)
  freezeGen (IOGenI gVar) = fmap IOGen (liftIO $ readIORef gVar)
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
  uniformShortByteString n = applyIOGen (genShortByteString n)

-- | Applies a pure operation to the wrapped pseudo-random number generator.
--
-- @since 1.2
applyIOGen :: MonadIO m => (g -> (a, g)) -> IOGen g RealWorld -> m a
applyIOGen f (IOGenI ref) = liftIO $ do
  g <- readIORef ref
  case f g of
    (!a, !g') -> a <$ writeIORef ref g'
{-# INLINE applyIOGen #-}


-- | This is a wrapper wround an @STRef@ that holds a pure generator. Because of
-- extra pointer indirection it will be slightly slower than if `PureGen` is
-- being used.
--
-- @since 1.2
newtype STGen g s = STGenI (STRef s g)

instance RandomGen g => MonadRandom (STGen g) s (ST s) where
  newtype Frozen (STGen g) = STGen { unSTGen :: g }
    deriving (Eq, Show, Read)
  thawGen (STGen g) = fmap STGenI (newSTRef g)
  freezeGen (STGenI gVar) = fmap STGen (readSTRef gVar)
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
  uniformShortByteString n = applySTGen (genShortByteString n)

-- | Applies a pure operation to the wrapped pseudo-random number generator.
--
-- @since 1.2
applySTGen :: (g -> (a, g)) -> STGen g s -> ST s a
applySTGen f (STGenI ref) = do
  g <- readSTRef ref
  case f g of
    (!a, !g') -> a <$ writeSTRef ref g'
{-# INLINE applySTGen #-}

-- | Runs a monadic generating action in the `ST` monad using a pure
-- pseudo-random number generator.
--
-- @since 1.2
runSTGen :: RandomGen g => g -> (forall s . STGen g s -> ST s a) -> (a, g)
runSTGen g action = unSTGen <$> runST (runGenM (STGen g) action)

-- | Runs a monadic generating action in the `ST` monad using a pure
-- pseudo-random number generator. Returns only the resulting pseudo-random
-- value.
--
-- @since 1.2
runSTGen_ :: RandomGen g => g -> (forall s . STGen g s -> ST s a) -> a
runSTGen_ g action = fst $ runSTGen g action


-- $uniform
-- This library provides two type classes to generate pseudo-random values:
--
-- *   'UniformRange' is used to generate a value of a datatype uniformly
--     within an inclusive range.
-- *   'Uniform' is used to generate a value of a datatype uniformly over all
--     possible values of that datatype.
--
-- Types may have instances for both or just one of 'UniformRange' and
-- 'Uniform'. A few examples illustrate this:
--
-- *   'Int', 'Word16' and 'Bool' are instances of both 'UniformRange' and
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

-- $implementmonadrandom
--
-- Typically, a monadic pseudo-random number generator has facilities to save
-- and restore its internal state in addition to generating pseudo-random
-- pseudo-random numbers.
--
-- Here is an example instance for the monadic pseudo-random number generator
-- from the @mwc-random@ package:
--
-- > instance (s ~ PrimState m, PrimMonad m) => MonadRandom MWC.Gen s m where
-- >   newtype Frozen MWC.Gen = Frozen { unFrozen :: MWC.Seed }
-- >   thawGen = fmap MWC.restore unFrozen
-- >   freezeGen = fmap Frozen . MWC.save
-- >   uniformWord8 = MWC.uniform
-- >   uniformWord16 = MWC.uniform
-- >   uniformWord32 = MWC.uniform
-- >   uniformWord64 = MWC.uniform
-- >   uniformShortByteString n g = unsafeSTToPrim (genShortByteStringST n (MWC.uniform g))
--
-- $references
--
-- 1. Guy L. Steele, Jr., Doug Lea, and Christine H. Flood. 2014. Fast
-- splittable pseudorandom number generators. In Proceedings of the 2014 ACM
-- International Conference on Object Oriented Programming Systems Languages &
-- Applications (OOPSLA '14). ACM, New York, NY, USA, 453-472. DOI:
-- <https://doi.org/10.1145/2660193.2660195>

-- $setup
-- >>> import Control.Arrow (first, second)
-- >>> import Control.Monad (replicateM)
-- >>> import Control.Monad.Primitive
-- >>> import Data.Bits
-- >>> import Data.Int (Int32)
-- >>> import Data.Word (Word8, Word16, Word32, Word64)
-- >>> import System.IO (IOMode(WriteMode), withBinaryFile)
-- >>> import System.Random.Monad
-- >>> import qualified System.Random.MWC as MWC
--
-- >>> :set -XFlexibleContexts
-- >>> :set -XFlexibleInstances
-- >>> :set -XMultiParamTypeClasses
-- >>> :set -XTypeFamilies
-- >>> :set -XUndecidableInstances
--
-- >>> :set -fno-warn-missing-methods
--
-- >>> :{
-- instance (s ~ PrimState m, PrimMonad m) => MonadRandom MWC.Gen s m where
--   newtype Frozen MWC.Gen = Frozen { unFrozen :: MWC.Seed }
--   thawGen = fmap MWC.restore unFrozen
--   freezeGen = fmap Frozen . MWC.save
--   uniformWord8 = MWC.uniform
--   uniformWord16 = MWC.uniform
--   uniformWord32 = MWC.uniform
--   uniformWord64 = MWC.uniform
--   uniformShortByteString n g = unsafeSTToPrim (genShortByteStringST n (MWC.uniform g))
-- :}
--
-- >>> :{
-- let rolls :: MonadRandom g s m => Int -> g s -> m [Word8]
--     rolls n = replicateM n . uniformRM (1, 6)
-- :}
