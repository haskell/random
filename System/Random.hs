{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

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
-- This implementation uses the Portable Combined Generator of L'Ecuyer
-- ["System.Random\#LEcuyer"] for 32-bit computers, transliterated by
-- Lennart Augustsson.  It has a period of roughly 2.30584e18.
--
-----------------------------------------------------------------------------

module System.Random
	(

	-- $intro

	-- * Random number generators

#ifdef ENABLE_SPLITTABLEGEN
	  RandomGen(..)
	, SplittableGen(..)
#else
	  RandomGen(..)
#endif
        , MonadRandom(..)
	-- ** Standard random number generators
	, StdGen
	, mkStdGen

        , genRandom
        , genRandomR
        , runStateGen
        , runStateGen_
        , runStateTGen
        , runStateTGen_

	-- ** The global random number generator

	-- $globalrng

	, getStdRandom
	, getStdGen
	, setStdGen
	, newStdGen

	-- * Random values of various types
	, Random(..)

        -- * Default generators
        , uniformByteArrayPrim

	-- * References
	-- $references

	) where

import Prelude

import Control.Arrow
import Control.Monad.ST
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Data.Bits
import Data.Int
import Data.Primitive.Types (Prim)
import Data.Primitive.ByteArray
import Data.Word
import Foreign.C.Types
import qualified System.Random.MWC as MWC

import Foreign.Ptr (plusPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import Foreign.Marshal.Alloc (alloca)
import Data.ByteString.Builder.Prim (word64LE)
import Data.ByteString.Builder.Prim.Internal (runF)

import System.CPUTime	( getCPUTime )
import Data.Time	( getCurrentTime, UTCTime(..) )
import Data.Ratio       ( numerator, denominator )
import Data.Char	( isSpace, chr, ord )
import System.IO.Unsafe ( unsafePerformIO )
import Data.IORef       ( IORef, newIORef, readIORef, writeIORef,
#if MIN_VERSION_base (4,6,0)
                          atomicModifyIORef' )
#else
                          atomicModifyIORef )
#endif
import Numeric		( readDec )

import GHC.Exts         ( Ptr(..), build, byteArrayContents#, unsafeCoerce# )

#if !MIN_VERSION_base (4,6,0)
atomicModifyIORef' :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORef' ref f = do
    b <- atomicModifyIORef ref
            (\x -> let (a, b) = f x
                    in (a, a `seq` b))
    b `seq` return b
#endif

mutableByteArrayContentsCompat :: MutableByteArray s -> Ptr Word8
{-# INLINE mutableByteArrayContentsCompat #-}
#if !MIN_VERSION_primitive(0,7,0)
mutableByteArrayContentsCompat (MutableByteArray arr#)
  = Ptr (byteArrayContents# (unsafeCoerce# arr#))
#else
mutableByteArrayContentsCompat = mutableByteArrayContents
#endif

getTime :: IO (Integer, Integer)
getTime = do
  utc <- getCurrentTime
  let daytime = toRational $ utctDayTime utc
  return $ quotRem (numerator daytime) (denominator daytime)

-- | The class 'RandomGen' provides a common interface to random number
-- generators.
--
#ifdef ENABLE_SPLITTABLEGEN
-- Minimal complete definition: 'next'.
#else
-- Minimal complete definition: 'next' and 'split'.
#endif

class RandomGen g where
  -- |The 'next' operation returns an 'Int' that is uniformly distributed
  -- in the range returned by 'genRange' (including both end points),
  -- and a new generator.
  next     :: g -> (Int, g)
  -- `next` can be deprecated over time

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

  genBytes :: Int -> g -> (ByteArray, g)
  genBytes n g = runST $ runStateTGen g $ uniformByteArrayPrim n PureGen
  {-# INLINE genBytes #-}


  -- |The 'genRange' operation yields the range of values returned by
  -- the generator.
  --
  -- It is required that:
  --
  -- * If @(a,b) = 'genRange' g@, then @a < b@.
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
  genRange :: g -> (Int,Int)

  -- default method
  genRange _ = (minBound, maxBound)

#ifdef ENABLE_SPLITTABLEGEN
-- | The class 'SplittableGen' proivides a way to specify a random number
--   generator that can be split into two new generators.
class SplittableGen g where
#endif
  -- |The 'split' operation allows one to obtain two distinct random number
  -- generators. This is very useful in functional programs (for example, when
  -- passing a random number generator down to recursive calls), but very
  -- little work has been done on statistically robust implementations of
  -- 'split' (["System.Random\#Burton", "System.Random\#Hellekalek"]
  -- are the only examples we know of).
  split    :: g -> (g, g)


class Monad m => MonadRandom g m where
  type Seed g :: *
  {-# MINIMAL save,restore,(uniformWord32R|uniformWord32),(uniformWord64R|uniformWord64) #-}

  restore :: Seed g -> m g
  save :: g -> m (Seed g)
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
  uniformBytes :: Int -> g -> m ByteArray
  default uniformBytes :: PrimMonad m => Int -> g -> m ByteArray
  uniformBytes = uniformByteArrayPrim
  {-# INLINE uniformBytes #-}


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
          unsafeIOToPrim $ runF word64LE w64 ptr
          go (i + 1) (ptr `plusPtr` 8)
        | otherwise = return ptr
  ptr <- go 0 (mutableByteArrayContentsCompat ma)
  when (nrem64 > 0) $ do
    w64 <- uniformWord64 gen
    -- in order not to mess up the byte order we write generated Word64 into a temporary
    -- pointer and then copy only the missing bytes over to the array
    unsafeIOToPrim $
      alloca $ \w64ptr -> do
        runF word64LE w64 w64ptr
        forM_ [0 .. nrem64 - 1] $ \i -> do
          w8 :: Word8 <- peekByteOff w64ptr i
          pokeByteOff ptr i w8
  unsafeFreezeByteArray ma
{-# INLINE uniformByteArrayPrim #-}


data SysRandom = SysRandom

-- Example /dev/urandom
instance MonadIO m => MonadRandom SysRandom m where
  uniformBytes n gen = liftIO $ uniformByteArrayPrim n gen

-- Example mwc-random
instance (s ~ PrimState m, PrimMonad m) => MonadRandom (MWC.Gen s) m where
  type Seed (MWC.Gen s) = MWC.Seed
  restore = MWC.restore
  save = MWC.save
  uniformWord32R u = MWC.uniformR (0, u)
  uniformWord64R u = MWC.uniformR (0, u)
  uniformWord8 = MWC.uniform
  uniformWord16 = MWC.uniform
  {-# INLINE uniformWord32 #-}
  uniformWord32 = MWC.uniform
  {-# INLINE uniformWord64 #-}
  uniformWord64 = MWC.uniform

-- | An opaque data type that carries the state of a pure generator at the type level
data PureGen g = PureGen

instance (MonadState g m, RandomGen g) => MonadRandom (PureGen g) m where
  type Seed (PureGen g) = g
  restore g = PureGen <$ put g
  save _ = get
  uniformWord32R r _ = state (genWord32R r)
  uniformWord64R r _ = state (genWord64R r)
  uniformWord8 _ = state genWord8
  uniformWord16 _ = state genWord16
  uniformWord32 _ = state genWord32
  uniformWord64 _ = state genWord64
  uniformBytes n _ = state (genBytes n)

genRandom :: (RandomGen g, Random a, MonadState g m) => m a
genRandom = randomM PureGen

genRandomR :: (RandomGen g, Random a, MonadState g m) => (a, a) -> m a
genRandomR r = randomRM r PureGen

-- | Split current generator and update the state with one part, while returning the other.
splitGen :: (MonadState g m, RandomGen g) => m g
splitGen = state split

runStateGen :: RandomGen g => g -> State g a -> (a, g)
runStateGen = flip runState

runStateGen_ :: RandomGen g => g -> State g a -> a
runStateGen_ g = fst . flip runState g

runStateTGen :: RandomGen g => g -> StateT g m a -> m (a, g)
runStateTGen = flip runStateT

runStateTGen_ :: (RandomGen g, Functor f) => g -> StateT g f a -> f a
runStateTGen_ g = fmap fst . flip runStateT g

randomList :: (Random a, RandomGen g, Num a) => Int -> g -> [a]
randomList n g = runStateGen_ g $ replicateM n (randomM PureGen)


-- | Example:
--
-- λ> runStateGen_ (mkGen 217 :: StdGen) (randomListM PureGen 10) :: [Word64]
-- [1,2,3,5,5,2,5,1,4,1]
randomListM :: (Random a, MonadRandom g m, Num a) => g -> Int -> m [a]
randomListM gen n = replicateM n (randomRM (1, 6) gen)

rlist :: Int -> ([Word64], [Word64])
rlist n = (xs, ys)
  where
    xs = runStateGen_ (mkStdGen 217 :: StdGen) (randomListM PureGen n) :: [Word64]
    ys = runST $ MWC.create >>= (`randomListM` n)


randomListM' :: MonadRandom g m => Seed g -> Int -> m (g, [Word64])
randomListM' seed n = do
  gen <- restore seed
  xs <- replicateM n (randomRM (1, 6) gen)
  return (gen, xs)


{- |
The 'StdGen' instance of 'RandomGen' has a 'genRange' of at least 30 bits.

The result of repeatedly using 'next' should be at least as statistically
robust as the /Minimal Standard Random Number Generator/ described by
["System.Random\#Park", "System.Random\#Carta"].
Until more is known about implementations of 'split', all we require is
that 'split' deliver generators that are (a) not identical and
(b) independently robust in the sense just given.

The 'Show' and 'Read' instances of 'StdGen' provide a primitive way to save the
state of a random number generator.
It is required that @'read' ('show' g) == g@.

In addition, 'reads' may be used to map an arbitrary string (not necessarily one
produced by 'show') onto a value of type 'StdGen'. In general, the 'Read'
instance of 'StdGen' has the following properties: 

* It guarantees to succeed on any string. 

* It guarantees to consume only a finite portion of the string. 

* Different argument strings are likely to result in different results.

-}

data StdGen 
 = StdGen !Int32 !Int32

instance RandomGen StdGen where
  next  = stdNext
  genRange _ = stdRange
  --                     ^  this is likely incorrect, but we'll switch to splitmix anyways

#ifdef ENABLE_SPLITTABLEGEN
instance SplittableGen StdGen where
#endif
  split = stdSplit

instance Show StdGen where
  showsPrec p (StdGen s1 s2) = 
     showsPrec p s1 . 
     showChar ' ' .
     showsPrec p s2

instance Read StdGen where
  readsPrec _p = \ r ->
     case try_read r of
       r'@[_] -> r'
       _   -> [stdFromString r] -- because it shouldn't ever fail.
    where 
      try_read r = do
         (s1, r1) <- readDec (dropWhile isSpace r)
	 (s2, r2) <- readDec (dropWhile isSpace r1)
	 return (StdGen s1 s2, r2)

{-
 If we cannot unravel the StdGen from a string, create
 one based on the string given.
-}
stdFromString         :: String -> (StdGen, String)
stdFromString s        = (mkStdGen num, rest)
	where (cs, rest) = splitAt 6 s
              num        = foldl (\a x -> x + 3 * a) 1 (map ord cs)


{- |
The function 'mkStdGen' provides an alternative way of producing an initial
generator, by mapping an 'Int' into a generator. Again, distinct arguments
should be likely to produce distinct generators.
-}
mkStdGen :: Int -> StdGen -- why not Integer ?
mkStdGen s = mkStdGen32 $ fromIntegral s

{-
From ["System.Random\#LEcuyer"]: "The integer variables s1 and s2 ... must be
initialized to values in the range [1, 2147483562] and [1, 2147483398]
respectively."
-}
mkStdGen32 :: Int32 -> StdGen
mkStdGen32 sMaybeNegative = StdGen (s1+1) (s2+1)
      where
	-- We want a non-negative number, but we can't just take the abs
	-- of sMaybeNegative as -minBound == minBound.
	s       = sMaybeNegative .&. maxBound
	(q, s1) = s `divMod` 2147483562
	s2      = q `mod` 2147483398

createStdGen :: Integer -> StdGen
createStdGen s = mkStdGen32 $ fromIntegral s

{- |
With a source of random number supply in hand, the 'Random' class allows the
programmer to extract random values of a variety of types.

Minimal complete definition: 'randomR' and 'random'.

-}

class Random a where
  randomRM :: MonadRandom g m => (a, a) -> g -> m a

  randomM :: MonadRandom g m => g -> m a


  -- | Takes a range /(lo,hi)/ and a random number generator
  -- /g/, and returns a random value uniformly distributed in the closed
  -- interval /[lo,hi]/, together with a new generator. It is unspecified
  -- what happens if /lo>hi/. For continuous types there is no requirement
  -- that the values /lo/ and /hi/ are ever produced, but they may be,
  -- depending on the implementation and the interval.
  {-# INLINE randomR #-}
  randomR :: RandomGen g => (a, a) -> g -> (a, g)
  randomR r g = runStateGen g (genRandomR r)

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
  random g = runStateGen g genRandom

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
  randomIO	   = getStdRandom random

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
  randomR ival g = randomIvalInteger ival g
  random g	 = randomR (toInteger (minBound::Int), toInteger (maxBound::Int)) g

instance Random Int8       where
  randomR = bitmaskWithRejection
  random = first (fromIntegral :: Word8 -> Int8) . genWord8
  randomM = fmap (fromIntegral :: Word8 -> Int8) . uniformWord8
  randomRM = bitmaskWithRejectionRM
instance Random Int16      where
  randomR = bitmaskWithRejection
  random = first (fromIntegral :: Word16 -> Int16) . genWord16
  randomM = fmap (fromIntegral :: Word16 -> Int16) . uniformWord16
  randomRM = bitmaskWithRejectionRM
instance Random Int32      where
  randomR = bitmaskWithRejection
  random = first (fromIntegral :: Word32 -> Int32) . genWord32
  randomM = fmap (fromIntegral :: Word32 -> Int32) . uniformWord32
  randomRM = bitmaskWithRejectionRM
instance Random Int64      where
  randomR = bitmaskWithRejection
  random = first (fromIntegral :: Word64 -> Int64) . genWord64
  randomM = fmap (fromIntegral :: Word64 -> Int64) . uniformWord64
  randomRM = bitmaskWithRejectionRM

instance Random Int        where
  randomR = bitmaskWithRejection
  randomRM = bitmaskWithRejectionRM
#if WORD_SIZE_IN_BITS < 64
  random = first (fromIntegral :: Word32 -> Int) . genWord32
  randomM = fmap (fromIntegral :: Word32 -> Int) . uniformWord32
#else
  random = first (fromIntegral :: Word64 -> Int) . genWord64
  randomM = fmap (fromIntegral :: Word64 -> Int) . uniformWord64
#endif

instance Random Word        where
  randomR = bitmaskWithRejection
  randomRM = bitmaskWithRejectionRM
#if WORD_SIZE_IN_BITS < 64
  random = first (fromIntegral :: Word32 -> Word) . genWord32
  randomM = fmap (fromIntegral :: Word32 -> Word) . uniformWord32
#else
  random = first (fromIntegral :: Word64 -> Word) . genWord64
  randomM = fmap (fromIntegral :: Word64 -> Word) . uniformWord64
#endif

instance Random Word8      where
  {-# INLINE randomR #-}
  randomR     = bitmaskWithRejection
  {-# INLINE random #-}
  random      = genWord8
  {-# INLINE randomRM #-}
  randomRM    = bitmaskWithRejectionRM
  {-# INLINE randomM #-}
  randomM     = uniformWord8
instance Random Word16     where
  {-# INLINE randomR #-}
  randomR     = bitmaskWithRejection
  {-# INLINE random #-}
  random      = genWord16
  {-# INLINE randomRM #-}
  randomRM    = bitmaskWithRejectionRM
  {-# INLINE randomM #-}
  randomM     = uniformWord16
instance Random Word32     where
  {-# INLINE randomR #-}
  randomR     = bitmaskWithRejection
  {-# INLINE random #-}
  random      = genWord32
  {-# INLINE randomM #-}
  randomM  = uniformWord32
  {-# INLINE randomRM #-}
  randomRM = bitmaskWithRejectionRM
instance Random Word64     where
  {-# INLINE randomR #-}
  randomR     = bitmaskWithRejection
  {-# INLINE random #-}
  random      = genWord64
  {-# INLINE randomM #-}
  randomM  = uniformWord64
  {-# INLINE randomRM #-}
  randomRM = bitmaskWithRejectionRM

instance Random CChar      where
  randomR (CChar b, CChar t)               = first CChar . randomR (b, t)
  random                                   = first CChar . random
  randomM                               = fmap CChar . randomM
  randomRM (CChar b, CChar t)           = fmap CChar . randomRM (b, t)
instance Random CSChar     where
  randomR (CSChar b, CSChar t)             = first CSChar . randomR (b, t)
  random                                   = first CSChar . random
  randomM                               = fmap CSChar . randomM
  randomRM (CSChar b, CSChar t)         = fmap CSChar . randomRM (b, t)
instance Random CUChar     where
  randomR (CUChar b, CUChar t)             = first CUChar . randomR (b, t)
  random                                   = first CUChar . random
  randomM                               = fmap CUChar . randomM
  randomRM (CUChar b, CUChar t)         = fmap CUChar . randomRM (b, t)
instance Random CShort     where
  randomR (CShort b, CShort t)             = first CShort . randomR (b, t)
  random                                   = first CShort . random
  randomM                               = fmap CShort . randomM
  randomRM (CShort b, CShort t)         = fmap CShort . randomRM (b, t)
instance Random CUShort    where
  randomR (CUShort b, CUShort t)           = first CUShort . randomR (b, t)
  random                                   = first CUShort . random
  randomM                               = fmap CUShort . randomM
  randomRM (CUShort b, CUShort t)       = fmap CUShort . randomRM (b, t)
instance Random CInt       where
  randomR (CInt b, CInt t)                 = first CInt . randomR (b, t)
  random                                   = first CInt . random
  randomM                               = fmap CInt . randomM
  randomRM (CInt b, CInt t)             = fmap CInt . randomRM (b, t)
instance Random CUInt      where
  randomR (CUInt b, CUInt t)               = first CUInt . randomR (b, t)
  random                                   = first CUInt . random
  randomM                               = fmap CUInt . randomM
  randomRM (CUInt b, CUInt t)           = fmap CUInt . randomRM (b, t)
instance Random CLong      where
  randomR (CLong b, CLong t)               = first CLong . randomR (b, t)
  random                                   = first CLong . random
  randomM                               = fmap CLong . randomM
  randomRM (CLong b, CLong t)           = fmap CLong . randomRM (b, t)
instance Random CULong     where
  randomR (CULong b, CULong t)             = first CULong . randomR (b, t)
  random                                   = first CULong . random
  randomM                               = fmap CULong . randomM
  randomRM (CULong b, CULong t)         = fmap CULong . randomRM (b, t)
instance Random CPtrdiff   where
  randomR (CPtrdiff b, CPtrdiff t)         = first CPtrdiff . randomR (b, t)
  random                                   = first CPtrdiff . random
  randomM                               = fmap CPtrdiff . randomM
  randomRM (CPtrdiff b, CPtrdiff t)     = fmap CPtrdiff . randomRM (b, t)
instance Random CSize      where
  randomR (CSize b, CSize t)               = first CSize . randomR (b, t)
  random                                   = first CSize . random
  randomM                               = fmap CSize . randomM
  randomRM (CSize b, CSize t)           = fmap CSize . randomRM (b, t)
instance Random CWchar     where
  randomR (CWchar b, CWchar t)             = first CWchar . randomR (b, t)
  random                                   = first CWchar . random
  randomM                               = fmap CWchar . randomM
  randomRM (CWchar b, CWchar t)         = fmap CWchar . randomRM (b, t)
instance Random CSigAtomic where
  randomR (CSigAtomic b, CSigAtomic t)     = first CSigAtomic . randomR (b, t)
  random                                   = first CSigAtomic . random
  randomM                               = fmap CSigAtomic . randomM
  randomRM (CSigAtomic b, CSigAtomic t) = fmap CSigAtomic . randomRM (b, t)
instance Random CLLong     where
  randomR (CLLong b, CLLong t)             = first CLLong . randomR (b, t)
  random                                   = first CLLong . random
  randomM                               = fmap CLLong . randomM
  randomRM (CLLong b, CLLong t)         = fmap CLLong . randomRM (b, t)
instance Random CULLong    where
  randomR (CULLong b, CULLong t)           = first CULLong . randomR (b, t)
  random                                   = first CULLong . random
  randomM                               = fmap CULLong . randomM
  randomRM (CULLong b, CULLong t)       = fmap CULLong . randomRM (b, t)
instance Random CIntPtr    where
  randomR (CIntPtr b, CIntPtr t)           = first CIntPtr . randomR (b, t)
  random                                   = first CIntPtr . random
  randomM                               = fmap CIntPtr . randomM
  randomRM (CIntPtr b, CIntPtr t)       = fmap CIntPtr . randomRM (b, t)
instance Random CUIntPtr   where
  randomR (CUIntPtr b, CUIntPtr t)         = first CUIntPtr . randomR (b, t)
  random                                   = first CUIntPtr . random
  randomM                               = fmap CUIntPtr . randomM
  randomRM (CUIntPtr b, CUIntPtr t)     = fmap CUIntPtr . randomRM (b, t)
instance Random CIntMax    where
  randomR (CIntMax b, CIntMax t)           = first CIntMax . randomR (b, t)
  random                                   = first CIntMax . random
  randomM                               = fmap CIntMax . randomM
  randomRM (CIntMax b, CIntMax t)       = fmap CIntMax . randomRM (b, t)
instance Random CUIntMax   where
  randomR (CUIntMax b, CUIntMax t)         = first CUIntMax . randomR (b, t)
  random                                   = first CUIntMax . random
  randomM                               = fmap CUIntMax . randomM
  randomRM (CUIntMax b, CUIntMax t)     = fmap CUIntMax . randomRM (b, t)

instance Random Char where
  randomR (a,b) g = 
       case (randomIvalInteger (toInteger (ord a), toInteger (ord b)) g) of
         (x,g') -> (chr x, g')
  random g	  = randomR (minBound,maxBound) g

instance Random Bool where
  randomR (a,b) g = 
      case (randomIvalInteger (bool2Int a, bool2Int b) g) of
        (x, g') -> (int2Bool x, g')
       where
         bool2Int :: Bool -> Integer
         bool2Int False = 0
         bool2Int True  = 1

	 int2Bool :: Int -> Bool
	 int2Bool 0	= False
	 int2Bool _	= True

  random g	  = randomR (minBound,maxBound) g

{-# INLINE randomRFloating #-}
randomRFloating :: (Fractional a, Num a, Ord a, Random a, RandomGen g) => (a, a) -> g -> (a, g)
randomRFloating (l,h) g 
    | l>h       = randomRFloating (h,l) g
    | otherwise = let (coef,g') = random g in 
		  (2.0 * (0.5*l + coef * (0.5*h - 0.5*l)), g')  -- avoid overflow

instance Random Double where
  randomR = randomRFloating
  random = randomDouble

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
  -- 	         (x,rng') -> (realToFrac (x::Float), rng')

-- instance Random CDouble where
--   randomR = randomRFloating
--   -- A MYSTERY:
--   -- Presently, this is showing better performance than the Double instance:
--   -- (And yet, if the Double instance uses randomFrac then its performance is much worse!)
--   random  = randomFrac
--   -- random rng = case random rng of 
--   -- 	         (x,rng') -> (realToFrac (x::Double), rng')

mkStdRNG :: Integer -> IO StdGen
mkStdRNG o = do
    ct          <- getCPUTime
    (sec, psec) <- getTime
    return (createStdGen (sec * 12345 + psec + ct + o))

randomBounded :: (RandomGen g, Random a, Bounded a) => g -> (a, g)
randomBounded = randomR (minBound, maxBound)

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


-- The continuous functions on the other hand take an [inclusive,exclusive) range.
randomFrac :: (RandomGen g, Fractional a) => g -> (a, g)
randomFrac = randomIvalDouble (0::Double,1) realToFrac

randomIvalDouble :: (RandomGen g, Fractional a) => (Double, Double) -> (Double -> a) -> g -> (a, g)
randomIvalDouble (l,h) fromDouble rng 
  | l > h     = randomIvalDouble (h,l) fromDouble rng
  | otherwise = 
       case (randomIvalInteger (toInteger (minBound::Int32), toInteger (maxBound::Int32)) rng) of
         (x, rng') -> 
	    let
	     scaled_x = 
		fromDouble (0.5*l + 0.5*h) +                   -- previously (l+h)/2, overflowed
                fromDouble ((0.5*h - 0.5*l) / (0.5 * realToFrac int32Count)) *  -- avoid overflow
		fromIntegral (x::Int32)
	    in
	    (scaled_x, rng')


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
bitmaskWithRejectionM uniform range gen = go
  where
    mask = complement zeroBits `shiftR` countLeadingZeros (range .|. 1)
    go = do
      x <- uniform gen
      let x' = x .&. mask
      if x' >= range
        then go
        else pure x'


bitmaskWithRejection32M :: MonadRandom g m => Word32 -> g -> m Word32
bitmaskWithRejection32M = bitmaskWithRejectionM uniformWord32

bitmaskWithRejection64M :: MonadRandom g m => Word64 -> g -> m Word64
bitmaskWithRejection64M = bitmaskWithRejectionM uniformWord64

int32Count :: Integer
int32Count = toInteger (maxBound::Int32) - toInteger (minBound::Int32) + 1  -- GHC ticket #3982

stdRange :: (Int,Int)
stdRange = (1, 2147483562)

stdNext :: StdGen -> (Int, StdGen)
-- Returns values in the range stdRange
stdNext (StdGen s1 s2) = (fromIntegral z', StdGen s1'' s2'')
	where	z'   = if z < 1 then z + 2147483562 else z
		z    = s1'' - s2''

		k    = s1 `quot` 53668
		s1'  = 40014 * (s1 - k * 53668) - k * 12211
		s1'' = if s1' < 0 then s1' + 2147483563 else s1'
    
		k'   = s2 `quot` 52774
		s2'  = 40692 * (s2 - k' * 52774) - k' * 3791
		s2'' = if s2' < 0 then s2' + 2147483399 else s2'

stdSplit            :: StdGen -> (StdGen, StdGen)
stdSplit std@(StdGen s1 s2)
                     = (left, right)
                       where
                        -- no statistical foundation for this!
                        left    = StdGen new_s1 t2
                        right   = StdGen t1 new_s2

                        new_s1 | s1 == 2147483562 = 1
                               | otherwise        = s1 + 1

                        new_s2 | s2 == 1          = 2147483398
                               | otherwise        = s2 - 1

                        StdGen t1 t2 = snd (next std)

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
theStdGen  = unsafePerformIO $ do
   rng <- mkStdRNG 0
   newIORef rng

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

1. FW #Burton# Burton and RL Page, /Distributed random number generation/,
Journal of Functional Programming, 2(2):203-212, April 1992.

2. SK #Park# Park, and KW Miller, /Random number generators -
good ones are hard to find/, Comm ACM 31(10), Oct 1988, pp1192-1201.

3. DG #Carta# Carta, /Two fast implementations of the minimal standard
random number generator/, Comm ACM, 33(1), Jan 1990, pp87-88.

4. P #Hellekalek# Hellekalek, /Don\'t trust parallel Monte Carlo/,
Department of Mathematics, University of Salzburg,
<http://random.mat.sbg.ac.at/~peter/pads98.ps>, 1998.

5. Pierre #LEcuyer# L'Ecuyer, /Efficient and portable combined random
number generators/, Comm ACM, 31(6), Jun 1988, pp742-749.

The Web site <http://random.mat.sbg.ac.at/> is a great source of information.

-}
