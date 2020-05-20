{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
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
  -- $usagemonadic

  -- * Pure and monadic pseudo-random number generator interfaces
  -- $interfaces
  , MonadRandom(..)
  , runGenM
  , runGenM_
  , RandomGenM(..)
  , randomM
  , randomRM
  , splitGenM

  -- * Monadic adapters for pure pseudo-random number generators
  -- $monadicadapters

  -- ** Pure adapter
  , StateGen(..)
  , StateGenM(..)
  , runStateGen
  , runStateGen_
  , runStateGenT
  , runStateGenT_
  , runStateGenST
  -- ** Mutable adapter with atomic operations
  , AtomicGen(..)
  , AtomicGenM(..)
  , applyAtomicGen
  -- ** Mutable adapter in 'IO'
  , IOGen(..)
  , IOGenM(..)
  , applyIOGen
  -- ** Mutable adapter in 'ST'
  , STGen(..)
  , STGenM(..)
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

  -- * Appendix

  -- ** How to implement 'MonadRandom'
  -- $implementmonadrandom

  -- ** Floating point number caveats
  -- $floating

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
-- [Monadic adapters] 'StateGenM', 'AtomicGenM', 'IOGenM' and 'STGenM' turn a
--     'RandomGen' instance into a 'MonadRandom' instance.
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
-- As an example, @rollsM@ generates @n@ pseudo-random values of @Word8@ in the
-- range @[1, 6]@ in a 'MonadRandom' context; given a /monadic/ pseudo-random
-- number generator, you can run this probabilistic computation as follows:
--
-- >>> :{
-- let rollsM :: MonadRandom g s m => Int -> g s -> m [Word8]
--     rollsM n = replicateM n . uniformRM (1, 6)
-- in do
--     monadicGen <- MWC.create
--     rollsM 10 monadicGen :: IO [Word8]
-- :}
-- [4,1,2,4,4,5,2,1,5,4]
--
-- Given a /pure/ pseudo-random number generator, you can run the monadic
-- pseudo-random number computation @rollsM@ in an 'IO' or 'ST' context by
-- first applying a monadic adapter like 'AtomicGen', 'IOGen' or 'STGen' to the
-- pure pseudo-random number generator and then running it with 'runGenM'.
--
-- >>> :{
-- let rollsM :: MonadRandom g s m => Int -> g s -> m [Word8]
--     rollsM n = replicateM n . uniformRM (1, 6)
--     pureGen = mkStdGen 42
-- in
--     runGenM_ (IOGen pureGen) (rollsM 10) :: IO [Word8]
-- :}
-- [5,1,4,3,3,2,5,2,2,4]

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
-- adapters 'StateGenM', 'AtomicGenM', 'IOGenM' and 'STGenM'.
--
-- *   'StateGenM' can be used in any state monad. With strict 'StateT' there is
--     no performance overhead compared to using the 'RandomGen' instance
--     directly. 'StateGenM' is /not/ safe to use in the presence of exceptions
--     and concurrency.
--
-- *   'AtomicGenM' is safe in the presence of exceptions and concurrency since
--     it performs all actions atomically.
--
-- *   'IOGenM' is a wrapper around an 'IORef' that holds a pure generator.
--     'IOGenM' is safe in the presence of exceptions, but not concurrency.
--
-- *   'STGenM' is a wrapper around an 'STRef' that holds a pure generator.
--     'STGenM' is safe in the presence of exceptions, but not concurrency.

-- | Interface to operations on 'RandomGen' wrappers like 'IOGenM' and 'StateGenM'.
--
-- @since 1.2
class (RandomGen r, MonadRandom (g r) s m) => RandomGenM g r s m where
  applyRandomGenM :: (r -> (a, r)) -> g r s -> m a

-- | Splits a pseudo-random number generator into two. Overwrites the mutable
-- wrapper with one of the resulting generators and returns the other.
--
-- @since 1.2
splitGenM :: RandomGenM g r s m => g r s -> m r
splitGenM = applyRandomGenM split

instance (RandomGen r, MonadIO m) => RandomGenM IOGenM r RealWorld m where
  applyRandomGenM = applyIOGen

instance (RandomGen r, MonadIO m) => RandomGenM AtomicGenM r RealWorld m where
  applyRandomGenM = applyAtomicGen

instance (RandomGen r, MonadState r m) => RandomGenM StateGenM r r m where
  applyRandomGenM f _ = state f

instance RandomGen r => RandomGenM STGenM r s (ST s) where
  applyRandomGenM = applySTGen

-- | Runs a mutable pseudo-random number generator from its 'Frozen' state.
--
-- >>> import Data.Int (Int8)
-- >>> runGenM (IOGen (mkStdGen 217)) (`uniformListM` 5) :: IO ([Int8], IOGen StdGen)
-- ([-74,37,-50,-2,3],IOGen {unIOGen = StdGen {unStdGen = SMGen 4273268533320920145 15251669095119325999}})
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

-- | Generates a pseudo-random value using monadic interface and `Random` instance.
--
-- @since 1.2
randomM :: (RandomGenM g r s m, Random a) => g r s -> m a
randomM = applyRandomGenM random

-- | Generates a pseudo-random value using monadic interface and `Random` instance.
--
-- @since 1.2
randomRM :: (RandomGenM g r s m, Random a) => (a, a) -> g r s -> m a
randomRM r = applyRandomGenM (randomR r)

-- | Wraps an 'IORef' that holds a pure pseudo-random number generator. All
-- operations are performed atomically.
--
-- *   'AtomicGenM' is safe in the presence of exceptions and concurrency.
-- *   'AtomicGenM' is the slowest of the monadic adapters due to the overhead
--     of its atomic operations.
--
-- @since 1.2
newtype AtomicGenM g s = AtomicGenM { unAtomicGenM :: IORef g}

newtype AtomicGen g = AtomicGen { unAtomicGen :: g }
    deriving stock (Eq, Show, Read)

instance (RandomGen g, MonadIO m) => MonadRandom (AtomicGenM g) RealWorld m where
  type Frozen (AtomicGenM g) = AtomicGen g
  thawGen (AtomicGen g) = fmap AtomicGenM (liftIO $ newIORef g)
  freezeGen (AtomicGenM gVar) = fmap AtomicGen (liftIO $ readIORef gVar)
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
applyAtomicGen :: MonadIO m => (g -> (a, g)) -> AtomicGenM g RealWorld -> m a
applyAtomicGen op (AtomicGenM gVar) =
  liftIO $ atomicModifyIORef' gVar $ \g ->
    case op g of
      (a, g') -> (g', a)
{-# INLINE applyAtomicGen #-}

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
-- >>> let ioGen g = withSystemTempFile "foo.bin" $ \_ h -> uniformRM (0, 100) g >>= flip uniformByteString g >>= hPutStr h
--
-- and then run it:
--
-- >>> runGenM_ (IOGen (mkStdGen 1729)) ioGen
--
-- @since 1.2
newtype IOGenM g s = IOGenM { unIOGenM :: IORef g }

newtype IOGen g = IOGen { unIOGen :: g }
    deriving stock (Eq, Show, Read)

instance (RandomGen g, MonadIO m) => MonadRandom (IOGenM g) RealWorld m where
  type Frozen (IOGenM g) = IOGen g
  thawGen (IOGen g) = fmap IOGenM (liftIO $ newIORef g)
  freezeGen (IOGenM gVar) = fmap IOGen (liftIO $ readIORef gVar)
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
applyIOGen :: MonadIO m => (g -> (a, g)) -> IOGenM g RealWorld -> m a
applyIOGen f (IOGenM ref) = liftIO $ do
  g <- readIORef ref
  case f g of
    (!a, !g') -> a <$ writeIORef ref g'
{-# INLINE applyIOGen #-}

-- | Wraps an 'STRef' that holds a pure pseudo-random number generator.
--
-- *   'STGenM' is safe in the presence of exceptions, but not concurrency.
-- *   'STGenM' is slower than 'StateGenM' due to the extra pointer indirection.
--
-- @since 1.2
newtype STGenM g s = STGenM { unSTGenM :: STRef s g }

newtype STGen g = STGen { unSTGen :: g }
    deriving stock (Eq, Show, Read)

instance RandomGen g => MonadRandom (STGenM g) s (ST s) where
  type Frozen (STGenM g) = STGen g
  thawGen (STGen g) = fmap STGenM (newSTRef g)
  freezeGen (STGenM gVar) = fmap STGen (readSTRef gVar)
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
applySTGen :: (g -> (a, g)) -> STGenM g s -> ST s a
applySTGen f (STGenM ref) = do
  g <- readSTRef ref
  case f g of
    (!a, !g') -> a <$ writeSTRef ref g'
{-# INLINE applySTGen #-}

-- | Runs a monadic generating action in the `ST` monad using a pure
-- pseudo-random number generator.
--
-- @since 1.2
runSTGen :: RandomGen g => g -> (forall s . STGenM g s -> ST s a) -> (a, g)
runSTGen g action = unSTGen <$> runST (runGenM (STGen g) action)

-- | Runs a monadic generating action in the `ST` monad using a pure
-- pseudo-random number generator. Returns only the resulting pseudo-random
-- value.
--
-- @since 1.2
runSTGen_ :: RandomGen g => g -> (forall s . STGenM g s -> ST s a) -> a
runSTGen_ g action = fst $ runSTGen g action


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

-- $floating
--
-- The 'UniformRange' instances for 'Float' and 'Double' use the following
-- procedure to generate a random value in a range for @uniformRM (a, b) g@:
--
-- 1.  Generate \(x\) uniformly such that \(0 \leq x \leq 1\).
--
--     The method by which \(x\) is sampled does not cover all representable
--     floating point numbers in the unit interval. The method never generates
--     denormal floating point numbers, for example.
--
-- 2.  Return \((b - a) * x + a\).
--
--     Due to rounding errors, floating point operations are neither
--     commutative, nor associative, nor distributive the way the corresponding
--     operations on real numbers are. Additionally, floating point numbers
--     admit special values @NaN@ as well as negative and positive infinity.
--
-- For pathological values, step 2 can yield surprising results.
--
-- *   The result may be greater than @max a b@.
--
--     >>> :{
--     let (a, b, x) = (-4.7021254e-38, -1.481e-42, 1.0)
--         result = (b - a) * x + a :: Float
--     in (result, result > max a b)
--     :}
--     (-1.48e-42,True)
--
-- *   The result may be @NaN@ even if \(a\) and \(b\) are not.
--
--     >>> :{
--     let (a, b, x) = (-1.7159568e38, 1.7159568e38, 0.0)
--     in (b - a) * x + a :: Float
--     :}
--     NaN
--
-- What happens when @NaN@ or @Infinity@ are given to 'uniformRM'? We first
-- define them as constants:
--
-- >>> nan = read "NaN" :: Float
-- >>> inf = read "Infinity" :: Float
--
-- *   If at least one of \(a\) or \(b\) is @NaN@, the result is @NaN@.
--
--     >>> let (a, b, x) = (nan, 1, 0.5) in (b - a) * x + a
--     NaN
--     >>> let (a, b, x) = (-1, nan, 0.5) in (b - a) * x + a
--     NaN
--
-- *   Otherwise, if \(a\) is @Infinity@ or @-Infinity@, the result is @NaN@.
--
--     >>> let (a, b, x) = (inf, 1, 0.5) in (b - a) * x + a
--     NaN
--     >>> let (a, b, x) = (-inf, 1, 0.5) in (b - a) * x + a
--     NaN
--
-- *   Otherwise, if \(b\) is @Infinity@ or @-Infinity@, the result is \(b\).
--
--     >>> let (a, b, x) = (1, inf, 0.5) in (b - a) * x + a
--     Infinity
--     >>> let (a, b, x) = (1, -inf, 0.5) in (b - a) * x + a
--     -Infinity
--
-- Note that the [GCC 10.1.0 C++ standard library](https://gcc.gnu.org/git/?p=gcc.git;a=blob;f=libstdc%2B%2B-v3/include/bits/random.h;h=19307fbc3ca401976ef6823e8fda893e4a263751;hb=63fa67847628e5f358e7e2e7edb8314f0ee31f30#l1859),
-- the [Java 10 standard library](https://docs.oracle.com/javase/10/docs/api/java/util/Random.html#doubles%28double,double%29)
-- and [CPython 3.8](https://github.com/python/cpython/blob/3.8/Lib/random.py#L417)
-- use the same procedure to generate floating point values in a range.
--
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
-- >   type Frozen MWC.Gen = MWC.Seed
-- >   thawGen = MWC.restore
-- >   freezeGen = MWC.save
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
-- >>> :{
-- instance (s ~ PrimState m, PrimMonad m) => MonadRandom MWC.Gen s m where
--   type Frozen MWC.Gen = MWC.Seed
--   thawGen = MWC.restore
--   freezeGen = MWC.save
--   uniformWord8 = MWC.uniform
--   uniformWord16 = MWC.uniform
--   uniformWord32 = MWC.uniform
--   uniformWord64 = MWC.uniform
--   uniformShortByteString n g = unsafeSTToPrim (genShortByteStringST n (MWC.uniform g))
-- :}
--
