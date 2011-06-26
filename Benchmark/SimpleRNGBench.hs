{-# LANGUAGE BangPatterns, ScopedTypeVariables, ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

-- | A simple script to do some very basic timing of the RNGs.

module Main where

import System.Exit (exitSuccess, exitFailure)
import System.Environment
import System.Random
import System.CPUTime  (getCPUTime)
import System.CPUTime.Rdtsc
import System.Console.GetOpt

import GHC.Conc
import Control.Concurrent
import Control.Monad 
import Control.Exception

import Data.IORef
import Data.List hiding (last,sum)
import Data.Int
import Data.List.Split
import Text.Printf

import Foreign.Ptr
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Storable (peek,poke)

import Benchmark.BinSearch
import Prelude  hiding (last,sum)

----------------------------------------------------------------------------------------------------
-- Miscellaneous helpers:

-- Readable large integer printing:
commaint :: Integral a => a -> String
commaint n = 
   reverse $ concat $
   intersperse "," $ 
   chunk 3 $ 
   reverse (show n)

padleft :: Int -> String -> String
padleft n str | length str >= n = str
padleft n str | otherwise       = take (n - length str) (repeat ' ') ++ str

padright :: Int -> String -> String
padright n str | length str >= n = str
padright n str | otherwise       = str ++ take (n - length str) (repeat ' ')

fmt_num :: (RealFrac a, PrintfArg a) => a -> String
fmt_num n = if n < 100 
	    then printf "%.2f" n
	    else commaint (round n :: Integer)


-- Measure clock frequency, spinning rather than sleeping to try to
-- stay on the same core.
measureFreq :: IO Int64
measureFreq = do 
  let second = 1000 * 1000 * 1000 * 1000 -- picoseconds are annoying
  t1 <- rdtsc 
  start <- getCPUTime
  let loop !n !last = 
       do t2 <- rdtsc 
	  when (t2 < last) $
	       putStrLn$ "COUNTERS WRAPPED "++ show (last,t2) 
	  cput <- getCPUTime		
	  if (cput - start < second) 
	   then loop (n+1) t2
	   else return (n,t2)
  (n,t2) <- loop 0 t1
  putStrLn$ "  Approx getCPUTime calls per second: "++ commaint (n::Int64)
  when (t2 < t1) $ 
    putStrLn$ "WARNING: rdtsc not monotonically increasing, first "++show t1++" then "++show t2++" on the same OS thread"

  return$ fromIntegral (t2 - t1)

----------------------------------------------------------------------------------------------------
-- Drivers to get random numbers repeatedly.

-- Test overheads without actually generating any random numbers:
data NoopRNG = NoopRNG
instance RandomGen NoopRNG where 
  next g     = (0,g)
#if 1
  genRange _ = (0,0)
instance SplittableGen NoopRNG where
#endif
  split g = (g,g)

type Kern = Int -> Ptr Int -> IO ()

-- [2011.01.28] Changing this to take "count" and "accumulator ptr" as arguments:
-- foreign import ccall "cbits/c_test.c" blast_rands :: Kern
-- foreign import ccall "cbits/c_test.c" store_loop  :: Kern
-- foreign import ccall unsafe "stdlib.hs" rand :: IO Int

{-# INLINE timeit #-}
timeit :: (Random a, RandomGen g) => 
	  Int -> Int64 -> String -> g -> (g -> (a,g)) -> IO ()
timeit numthreads freq msg gen nxt =
  do 
     counters <- forM [1..numthreads] (const$ newIORef (1::Int64)) 
     tids <- forM counters $ \counter -> 
	        forkIO $ infloop counter (nxt gen)   
     threadDelay (1000*1000) -- One second
     mapM_ killThread tids

     finals <- mapM readIORef counters
     let mean :: Double = fromIntegral (foldl1 (+) finals) / fromIntegral numthreads
         cycles_per :: Double = fromIntegral freq / mean
     printResult (round mean :: Int64) msg cycles_per

 where 
   infloop !counter !(!_,!g) = 
     do incr counter
	infloop counter (nxt g)

   incr !counter = 
     do -- modifyIORef counter (+1) -- Not strict enough!
	c <- readIORef counter
	let c' = c+1
	_ <- evaluate c'
	writeIORef counter c'     


-- This function times an IO function on one or more threads.  Rather
-- than running a fixed number of iterations, it uses a binary search
-- to find out how many iterations can be completed in a second.
timeit_foreign :: Int -> Int64 -> String -> (Int -> Ptr Int -> IO ()) -> IO Int64
timeit_foreign numthreads freq msg ffn = do 
  ptr     :: ForeignPtr Int <- mallocForeignPtr

  let kern = if numthreads == 1
	     then ffn
	     else replicate_kernel numthreads ffn 
      wrapped n = withForeignPtr ptr (kern$ fromIntegral n)
  (n,t) <- binSearch False 1 (1.0, 1.05) wrapped

  let total_per_second = round $ fromIntegral n * (1 / t)
      cycles_per = fromIntegral freq * t / fromIntegral n
  printResult total_per_second msg cycles_per
  return total_per_second

 where 
   -- This lifts a C kernel to operate simultaneously on N threads.
   replicate_kernel :: Int -> Kern -> Kern
   replicate_kernel nthreads kern n ptr = do
     ptrs <- forM [1..nthreads]
	       (const mallocForeignPtr) 
     tmpchan <- newChan
     -- let childwork = ceiling$ fromIntegral n / fromIntegral nthreads
     let childwork = n -- Keep it the same.. interested in per-thread throughput.
     -- Fork/join pattern:
     _ <- forM ptrs $ \pt -> forkIO $ 
	      withForeignPtr pt $ \p -> do
		 kern (fromIntegral childwork) p
		 result <- peek p
		 writeChan tmpchan result

     results <- forM [1..nthreads] $ \_ -> 
		  readChan tmpchan
     -- Meaningless semantics here... sum the child ptrs and write to the input one:
     poke ptr (foldl1 (+) results)
     return ()


printResult ::  Int64 -> String -> Double -> IO ()
printResult total msg cycles_per = 
     putStrLn$ "    "++ padleft 11 (commaint total) ++" randoms generated "++ padright 27 ("["++msg++"]") ++" ~ "
	       ++ fmt_num cycles_per ++" cycles/int"


-- Take many measurements and record the max/min/average random values.
approxBounds :: (RandomGen g, Random a, Ord a, Num a, Fractional a) => 
		g -> (g -> (a,g)) -> Int -> (a,a,a)
approxBounds initrng nxt iters = (mn_, mx_, sum_ / fromIntegral iters)
 where 
  (mn_, mx_, sum_) = loop initrng iters 100 (-100) 0 -- Oops, can't use minBound/maxBound here.
  loop _ 0 mn mx sum = (mn,mx,sum)
  loop rng  n mn mx sum = 
    case nxt rng of 
      (x, rng') -> loop rng' (n-1) (min x mn) (max x mx) (x+sum)

floatBounds :: IO (Float, Float, Float)
floatBounds = do g<-getStdGen; return$ approxBounds g random 100000 :: IO (Float,Float,Float)


----------------------------------------------------------------------------------------------------
-- Main Script

data Flag = NoC | Help 
  deriving (Show, Eq)

options :: [OptDescr Flag]
options = 
   [ Option ['h']  ["help"]  (NoArg Help)  "print program help"
   , Option []     ["noC"]   (NoArg NoC)   "omit C benchmarks, haskell only"
   ]

main :: IO ()
main = do 
   argv <- getArgs
   let (opts,_,other) = getOpt Permute options argv

   when (not$ null other) $ do
       putStrLn$ "ERROR: Unrecognized options: " 
       mapM_ putStr other
       exitFailure

   when (Help `elem` opts) $ do
       putStr$ usageInfo "Benchmark random number generation" options
       exitSuccess

   putStrLn$ "\nHow many random numbers can we generate in a second on one thread?"

   t1 <- rdtsc
   t2 <- rdtsc
   putStrLn ("  Cost of rdtsc (ffi call):    " ++ show (t2 - t1))

   freq <- measureFreq
   putStrLn$ "  Approx clock frequency:  " ++ commaint freq

   let 
       randInt     = random :: RandomGen g => g -> (Int,g)
       randFloat   = random :: RandomGen g => g -> (Float,g)
       randCFloat  = random :: RandomGen g => g -> (CFloat,g)
       randDouble  = random :: RandomGen g => g -> (Double,g)
       randCDouble = random :: RandomGen g => g -> (CDouble,g)
       randInteger = random :: RandomGen g => g -> (Integer,g)
       randBool    = random :: RandomGen g => g -> (Bool,g)

       -- rangeFloat  g = randomR (-100, 100::Float)   g
       -- rangeCFloat  g = randomR (-100, 100::CFloat)  g
       -- rangeDouble  g = randomR (-100, 100::Double)  g
       -- rangeCDouble g = randomR (-100, 100::CDouble) g
       -- rangeInteger g = randomR (-100, 100::Integer) g
       -- rangeBool    g = randomR (False, True)        g

       gen = mkStdGen 23852358661234
       gamut th = do
	 putStrLn$ "  First, timing System.Random.next:"
	 timeit th freq "constant zero gen"    NoopRNG next 
	 timeit th freq "System.Random stdGen" gen     next 

	 putStrLn$ "\n  Second, timing System.Random.random at different types:"
	 timeit th freq "System.Random Ints"     gen   randInt
	 timeit th freq "System.Random Floats"   gen   randFloat
	 timeit th freq "System.Random CFloats"  gen   randCFloat
	 timeit th freq "System.Random Doubles"  gen   randDouble
	 timeit th freq "System.Random CDoubles" gen   randCDouble
	 timeit th freq "System.Random Integers" gen   randInteger
	 timeit th freq "System.Random Bools"    gen   randBool

	 putStrLn$ "\n  Third, timing range-restricted System.Random.randomR:"
	 timeit th freq "System.Random Ints"     gen   (randomR (-100, 100::Int))
	 timeit th freq "System.Random Floats"   gen   (randomR (-100, 100::Float))
	 timeit th freq "System.Random CFloats"  gen   (randomR (-100, 100::CFloat))
	 timeit th freq "System.Random Doubles"  gen   (randomR (-100, 100::Double))
	 timeit th freq "System.Random CDoubles" gen   (randomR (-100, 100::CDouble))
	 timeit th freq "System.Random Integers" gen   (randomR (-100, 100::Integer))
	 timeit th freq "System.Random Bools"    gen   (randomR (False, True::Bool))


  --       when (not$ NoC `elem` opts) $ do
  --	  putStrLn$ "  Comparison to C's rand():"
  --	  timeit_foreign th freq "ptr store in C loop"   store_loop
  --	  timeit_foreign th freq "rand/store in C loop"  blast_rands
  --	  timeit_foreign th freq "rand in Haskell loop" (\n ptr -> forM_ [1..n]$ \_ -> rand )
  --	  timeit_foreign th freq "rand/store in Haskell loop"  (\n ptr -> forM_ [1..n]$ \_ -> do n <- rand; poke ptr n )
  --	  return ()

   -- Test with 1 thread and numCapabilities threads:
   gamut 1
   when (numCapabilities > 1) $ do 
       putStrLn$ "\nNow "++ show numCapabilities ++" threads, reporting mean randoms-per-second-per-thread:"
       gamut numCapabilities
       return ()

   putStrLn$ "Finished."
