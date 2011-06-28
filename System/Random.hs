{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Random
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
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

#include "MachDeps.h"

module System.Random
	(

	-- $intro

	-- * Random number generators

	  RandomGen(next, genRange)
	, SplittableGen(split)

	-- ** Standard random number generators
	, StdGen
	, mkStdGen

	-- ** The global random number generator

	-- $globalrng

	, getStdRandom
	, getStdGen
	, setStdGen
	, newStdGen

	-- * Random values of various types
	, Random ( random,   randomR,
		   randoms,  randomRs,
		   randomIO, randomRIO )

	-- * References
	-- $references

	) where

import Prelude

import Data.Bits
import Data.Int
import Data.Word
import Foreign.C.Types

#ifdef __NHC__
import CPUTime		( getCPUTime )
import Foreign.Ptr      ( Ptr, nullPtr )
import Foreign.C	( CTime, CUInt )
#else
import System.CPUTime	( getCPUTime )
import Data.Time	( getCurrentTime, UTCTime(..) )
import Data.Ratio       ( numerator, denominator )
#endif
import Data.Char	( isSpace, chr, ord )
import System.IO.Unsafe ( unsafePerformIO )
import Data.IORef
import Numeric		( readDec )

-- #define DEBUGRAND
#ifdef DEBUGRAND
import Numeric		( showIntAtBase )
import Data.Char     ( intToDigit )
import Debug.Trace
#endif

-- The standard nhc98 implementation of Time.ClockTime does not match
-- the extended one expected in this module, so we lash-up a quick
-- replacement here.
#ifdef __NHC__
foreign import ccall "time.h time" readtime :: Ptr CTime -> IO CTime
getTime :: IO (Integer, Integer)
getTime = do CTime t <- readtime nullPtr;  return (toInteger t, 0)
#else
getTime :: IO (Integer, Integer)
getTime = do
  utc <- getCurrentTime
  let daytime = toRational $ utctDayTime utc
  return $ quotRem (numerator daytime) (denominator daytime)
#endif

-- | The class 'RandomGen' provides a common interface to random number
-- generators.
--
-- Minimal complete definition: 'next'.

class RandomGen g where

   -- |The 'next' operation returns an 'Int' that is uniformly distributed
   -- in the range returned by 'genRange' (including both end points),
   -- and a new generator.
   next     :: g -> (Int, g)

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

   -- If the RandomGen can produce at least @N@ uniformly distributed
   -- random bits via the @next@ method, then genBits may indicate how many.
   genBits :: g -> Maybe Int

   -- default method
   -- TODO: Write this in terms of genRange:
   genBits = error "genBits: implement me!"

-- | The class 'SplittableGen' proivides a way to specify a random number
-- generator that can be split into two new generators.
class SplittableGen g where
   -- |The 'split' operation allows one to obtain two distinct random number
   -- generators. This is very useful in functional programs (for example, when
   -- passing a random number generator down to recursive calls), but very
   -- little work has been done on statistically robust implementations of
   -- 'split' (["System.Random\#Burton", "System.Random\#Hellekalek"]
   -- are the only examples we know of).
   split    :: g -> (g, g)

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
 = StdGen Int32 Int32

instance RandomGen StdGen where
  next  = stdNext
  genRange _ = stdRange
  -- Warning: Because snd genRange is just shy of 2^31 this is actually slightly inaccurate.
  -- We accept a very small non-uniformity of output here to enable us to 
  genBits  _ = Just 31

instance SplittableGen StdGen where
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

-- FIXME: 1/2/3 below should be ** (vs@30082002) XXX

{- |
With a source of random number supply in hand, the 'Random' class allows the
programmer to extract random values of a variety of types.

Minimal complete definition: 'randomR' and 'random'.

-}

class Random a where
  -- | Takes a range /(lo,hi)/ and a random number generator
  -- /g/, and returns a random value uniformly distributed in the closed
  -- interval /[lo,hi]/, together with a new generator. It is unspecified
  -- what happens if /lo>hi/. For continuous types there is no requirement
  -- that the values /lo/ and /hi/ are ever produced, but they may be,
  -- depending on the implementation and the interval.
  randomR :: RandomGen g => (a,a) -> g -> (a,g)

  -- | The same as 'randomR', but using a default range determined by the type:
  --
  -- * For bounded types (instances of 'Bounded', such as 'Char'),
  --   the range is normally the whole type.
  --
  -- * For fractional types, the range is normally the semi-closed interval
  -- @[0,1)@.
  --
  -- * For 'Integer', the range is (arbitrarily) the range of 'Int'.
  random  :: RandomGen g => g -> (a, g)

  -- | Plural variant of 'randomR', producing an infinite list of
  -- random values instead of returning a new generator.
  randomRs :: RandomGen g => (a,a) -> g -> [a]
  randomRs ival g = x : randomRs ival g' where (x,g') = randomR ival g

  -- | Plural variant of 'random', producing an infinite list of
  -- random values instead of returning a new generator.
  randoms  :: RandomGen g => g -> [a]
  randoms  g      = (\(x,g') -> x : randoms g') (random g)

  -- | A variant of 'randomR' that uses the global random number generator
  -- (see "System.Random#globalrng").
  randomRIO :: (a,a) -> IO a
  randomRIO range  = getStdRandom (randomR range)

  -- | A variant of 'random' that uses the global random number generator
  -- (see "System.Random#globalrng").
  randomIO  :: IO a
  randomIO	   = getStdRandom random


instance Random Integer where
  -- randomR cannot use the "Bits" version here:
  randomR ival@(lo,hi) = 
      let bits = (1 + max (bitOccupancy lo) (bitOccupancy hi)) in
      randomIvalBits_raw bits ival 
  random g = case random g of (x,g') -> (toInteger (x::Int), g')

instance Random Int        where randomR = randomIvalBits; random = randomBits WORD_SIZE_IN_BITS
instance Random Int8       where randomR = randomIvalBits; random = randomBits 8
instance Random Int16      where randomR = randomIvalBits; random = randomBits 16
instance Random Int32      where randomR = randomIvalBits; random = randomBits 32 
instance Random Int64      where randomR = randomIvalBits; random = randomBits 64

#ifndef __NHC__
-- Word is a type synonym in nhc98.
instance Random Word       where randomR = randomIvalBits; random = randomBits WORD_SIZE_IN_BITS
#endif
instance Random Word8      where randomR = randomIvalBits; random = randomBits 8
instance Random Word16     where randomR = randomIvalBits; random = randomBits 16
instance Random Word32     where randomR = randomIvalBits; random = randomBits 32
instance Random Word64     where randomR = randomIvalBits; random = randomBits 64

instance Random CChar      where randomR = randomIvalBits; random = randomBits 8
instance Random CSChar     where randomR = randomIvalBits; random = randomBits 8
instance Random CUChar     where randomR = randomIvalBits; random = randomBits 8
instance Random CShort     where randomR = randomIvalBits; random = randomBits 16
instance Random CUShort    where randomR = randomIvalBits; random = randomBits 16
instance Random CInt       where randomR = randomIvalBits; random = randomBits 32
instance Random CUInt      where randomR = randomIvalBits; random = randomBits 32
instance Random CLong      where randomR = randomIvalBits; random = randomBits WORD_SIZE_IN_BITS
instance Random CULong     where randomR = randomIvalBits; random = randomBits WORD_SIZE_IN_BITS
instance Random CPtrdiff   where randomR = randomIvalBits; random = randomBits WORD_SIZE_IN_BITS
instance Random CSize      where randomR = randomIvalBits; random = randomBits WORD_SIZE_IN_BITS
instance Random CWchar     where randomR = randomIvalBits; random = randomBits 32
instance Random CSigAtomic where randomR = randomIvalBits; random = randomBits 32
instance Random CLLong     where randomR = randomIvalBits; random = randomBits 64
instance Random CULLong    where randomR = randomIvalBits; random = randomBits 64
instance Random CIntPtr    where randomR = randomIvalBits; random = randomBits WORD_SIZE_IN_BITS
instance Random CUIntPtr   where randomR = randomIvalBits; random = randomBits WORD_SIZE_IN_BITS
instance Random CIntMax    where randomR = randomIvalBits; random = randomBits 64
instance Random CUIntMax   where randomR = randomIvalBits; random = randomBits 64

instance Random Char where
  randomR (a,b) g = case randomR (ord a, ord b) g of
		     (x,g') -> (chr x, g')
  random g = randomR (minBound,maxBound) g

instance Random Bool where
  randomR (False,False) g = (False,g)
  randomR (True,True)   g = (True, g)
  randomR _             g = random g
  random g = case random g of 
	      (x,g') -> (testBit (x::Word8) 0, g')

instance Random Double where
  randomR = randomRFloating
  random rng     = 
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
  random rng = 
    case rand of 
      (x,rng') -> 
          -- We use 24 bits of randomness corresponding to the 24 bit significand:
          ((fromIntegral (mask24 .&. (x::Int)) :: Float) 
	   /  fromIntegral twoto24, rng')
   where
     rand = case genBits rng of 
	      Just n | n >= 24 -> next rng
	      _                -> random rng
     mask24 = twoto24 - 1
     twoto24 = (2::Int) ^ (24::Int)

-- CFloat/CDouble are basically the same as a Float/Double:
instance Random CFloat where
  randomR = randomRFloating
  random rng = case random rng of 
  	         (x,rng') -> (realToFrac (x::Float), rng')

instance Random CDouble where
  randomR = randomRFloating
  random rng = case random rng of 
   	         (x,rng') -> (realToFrac (x::Double), rng')

mkStdRNG :: Integer -> IO StdGen
mkStdRNG o = do
    ct          <- getCPUTime
    (sec, psec) <- getTime
    return (createStdGen (sec * 12345 + psec + ct + o))

{-# INLINE randomRFloating #-}
randomRFloating :: (Num a, Ord a, Random a, RandomGen g) => (a, a) -> g -> (a, g)
randomRFloating (l,h) g 
    | l>h       = randomRFloating (h,l) g
    | otherwise = let (coef,g') = random g in 
		  (l + coef * (h-l), g')


-- Create a specific number of random bits.
randomBits :: (RandomGen g, Bits a) => Int -> g -> (a,g)
randomBits desired gen =
  case genBits gen of 
    Just bits -> 
	let   
	    loop g !acc 0 = (acc,g)
	    loop g !acc c = 
	      case next g of 
	       (x,g') -> 
		 if bits <= c
		 then loop g' (acc `shiftL` bits .|. fromIntegral x) (c - bits)
		 -- Otherwise we must make sure not to generate too many bits:
	         else 
		      let shifted = fromIntegral (x `shiftR` (bits - c)) in
#ifdef DEBUGRAND
		      trace ("    Got random "++ showIntAtBase 16 intToDigit x "" ++
		      	     ", shifted "++ show (bits-c)++": " ++ show shifted) $
#endif
		      (acc `shiftL` c .|. shifted, g')
	in loop gen 0 desired
    Nothing -> error "TODO: IMPLEMENT ME - handle undesirable bit sources"    
 where 

--------------------------------------------------------------------------------
-- TEMP: This should probably be in Data.Bits AND they should have hardware support.
-- (See trac ticket #4102.)

-- Determine the number of leading zero bits:
bitScanReverse :: Bits a => Int -> a -> Int
bitScanReverse size num = loop (size - 1)
  where 
   loop i | i < 0         = size
          | testBit num i = size - 1 - i
	  | otherwise     = loop (i-1)
--------------------------------------------------------------------------------

-- This new version uses randomBits to generate a number in an interval.
randomIvalBits :: (RandomGen g, Integral a, Bits a) => (a, a) -> g -> (a, g)
randomIvalBits bounds@(lo,_) rng = 
  randomIvalBits_raw (bitSize lo) bounds rng

randomIvalBits_raw :: (RandomGen g, Integral a, Bits a) => 
		      Int -> (a, a) -> g -> (a, g)
randomIvalBits_raw maxbits (l,h) rng 
  | l > h     = randomIvalBits (h,l) rng
  | otherwise = 
#ifdef DEBUGRAND
      trace ("  Got pow2: "++show pow2++" bounding "++show bounding++" maxbits "++show maxbits++
	     " range " ++ show range ++ " cutoff "++ show cutoff) $ 
#endif
    if special_case 
    -- In the special case we don't offset from the lower bound:
    then (h - cutoff + fin_x + 1, fin_rng)
    else (l + fin_x, fin_rng)
 where 

-- TODO - USE IS_SIGNED!!!

    (fin_x,fin_rng) = 
       if range == bit (pow2 - 1)
       -- If we have a power-of-two-sized interval life is easy!
       then randomBits (pow2 - 1) rng
       else rollAndTrash rng

    -- range is the number of distinct values we wish to generate:
    -- If we are dealing with a signed type, range may be negative!
    range  = h - l + 1

    -- With randomBits we can only generate power-of-two ranges.  We
    -- need to find the smallest power-of-two that is bigger than range.
    pow2 = findBoundingPow2 maxbits range
    -- Bounding is the largest number we will generate with pow2 random bits:
    -- Here we explicitly counter sign-extension in shiftR:
    special_case = range < 0 -- Special case for signed numbers and range overflow.
--    bounding = let pow = if isSigned l then pow2-2 else pow2-1
    bounding = let pow = if special_case then pow2-2 else pow2-1
	           n = 1 `shiftL` pow in
	       n - 1 + n
	-- if special_case
	-- then bnd -- clearBit (complement 0) (maxbits-1)
	-- else bnd `shiftR` (maxbits - pow2 - 1)
    cutoff = 
	if special_case
	then bounding - (bounding - h) - (l - complement bounding) + 1
	else bounding - (bounding `rem` range)

    -- rollAndTrash rolls the dice repeatedly, trashing results it doesn't
    -- like.  In the worst case, it can trash up to 50% of the
    -- results (but typically much much less).
    rollAndTrash g = 
      case randomBits pow2 g of 
        (x,g') | x >= cutoff -> rollAndTrash g'
        (x,g')               -> (if special_case then x 
				 else x `mod` range, g')

-- Find the smallest power of two greater than the given number, that
-- is, the number of bits needed to represent the number.
-- Treat all numbers as unsigned irrespective of type:
findBoundingPow2 :: (Bits a, Ord a) => Int -> a -> Int
-- findBoundingPow2 num | num <= 0 = error "findBoundingPow2 should not be given a non-positive number"
findBoundingPow2 bitsize num = bitsize - bitScanReverse bitsize num

-- How many bits does it take to represent this integer?
-- NOT counting the sign bit.
bitOccupancy :: Integer -> Int
bitOccupancy i | i < 0 = bitOccupancy (-i)
bitOccupancy i         = if i == 0 then 0 else 1 + bitOccupancy (i `shiftR` 1)

stdRange :: (Int,Int)
stdRange = (0, 2147483562)

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
newStdGen = atomicModifyIORef theStdGen split

{- |Uses the supplied function to get a value from the current global
random generator, and updates the global generator with the new generator
returned by the function. For example, @rollDice@ gets a random integer
between 1 and 6:

>  rollDice :: IO Int
>  rollDice = getStdRandom (randomR (1,6))

-}

getStdRandom :: (StdGen -> (a,StdGen)) -> IO a
getStdRandom f = atomicModifyIORef theStdGen (swap . f)
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
