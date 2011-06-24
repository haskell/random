{-# LANGUAGE BangPatterns, ScopedTypeVariables, ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

-- | A simple script to do some very basic timing of the RNGs.

--   It is important that we also run established stastical tests on
--   these RNGs a some point...

module Main where

import qualified Codec.Encryption.BurtonRNGSlow as BS

-- --import qualified Codec.Crypto.IntelAES.GladmanAES  as GA
-- import qualified Codec.Crypto.GladmanAES           as GA
-- import qualified Codec.Crypto.IntelAES.AESNI       as NI
-- import qualified Codec.Crypto.IntelAES             as IA
-- import qualified Codec.Crypto.ConvertRNG           as CR
-- -- import qualified Codec.Crypto.AES.Random        as Svein

import System.Exit (exitSuccess, exitFailure)
import System.Environment
import System.Random
-- import System.PosixCompat (sleep)
import System.Posix (sleep)
import System.CPUTime  (getCPUTime)
-- import Data.Time.Clock (diffUTCTime)
import System.CPUTime.Rdtsc
import System.Console.GetOpt

import GHC.Conc
import Control.Concurrent
import Control.Monad 
import Control.Concurrent.Chan
import Control.Exception

-- import Crypto.Random (CryptoRandomGen(..))

import Data.IORef
import Data.List
import Data.Int
import Data.Word
import Data.List.Split
import Data.Serialize
import qualified Data.ByteString as B
import Text.Printf

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable (peek,poke)

import Benchmark.BinSearch

----------------------------------------------------------------------------------------------------
-- Miscellaneous helpers:

-- I cannot *believe* there is not a standard call or an
-- easily-findable hackage library supporting locale-based printing of
-- numbers. [2011.01.28]
commaint :: Integral a => a -> String
commaint n = 
   reverse $
   concat $
   intersperse "," $ 
   chunk 3 $ 
   reverse (show n)

padleft n str | length str >= n = str
padleft n str | otherwise       = take (n - length str) (repeat ' ') ++ str

padright n str | length str >= n = str
padright n str | otherwise       = str ++ take (n - length str) (repeat ' ')

fmt_num n = if n < 100 
	    then printf "%.2f" n
	    else commaint (round n)

-- This version simply busy-waits to stay on the same core:
measure_freq2 :: IO Int64
measure_freq2 = do 
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
  putStrLn$ "  Approx getCPUTime calls per second: "++ commaint n
  when (t2 < t1) $ 
    putStrLn$ "WARNING: rdtsc not monotonically increasing, first "++show t1++" then "++show t2++" on the same OS thread"

  return$ fromIntegral (t2 - t1)

----------------------------------------------------------------------------------------------------
-- Drivers to get random numbers repeatedly.

incr !counter = 
  do -- modifyIORef counter (+1) -- Not strict enough!
     -- Incrementing counter strictly (avoiding stack overflow) is annoying:
     c <- readIORef counter
     let c' = c+1
     evaluate c'
     writeIORef counter c'     

loop :: RandomGen g => IORef Int -> (Int,g) -> IO b
loop !counter !(!n,!g) = 
  do incr counter
     loop counter (next g)

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

----------------------------------------------------------------------------------------------------
-- Timing:

timeit numthreads freq msg mkgen =
  do 
     counters <- forM [1..numthreads] (const$ newIORef 1) 
     tids <- forM counters $ \counter -> 
	        forkIO $ loop counter (next$ mkgen 23852358661234)   
     threadDelay (1000*1000) -- One second
     mapM_ killThread tids

     finals <- mapM readIORef counters
     let mean :: Double = fromIntegral (foldl1 (+) finals) / fromIntegral numthreads
         cycles_per :: Double = fromIntegral freq / mean
     print_result (round mean) msg cycles_per

print_result total msg cycles_per = 
     putStrLn$ "    "++ padleft 11 (commaint total) ++" random ints generated "++ padright 27 ("["++msg++"]") ++" ~ "
	       ++ fmt_num cycles_per ++" cycles/int"


-- This function times a function on one or more threads.  Rather than
-- running a fixed number of iterations, this number does a binary
-- search to find out how many iterations can be completed in a second.
timeit2 :: Int -> Int64 -> String -> (Int -> Ptr Int -> IO ()) -> IO Int
timeit2 numthreads freq msg ffn = do 
  ptr     :: ForeignPtr Int <- mallocForeignPtr

  let kern = if numthreads == 1
	     then ffn
	     else replicate_kernel numthreads ffn 
      wrapped n = withForeignPtr ptr (kern$ fromIntegral n)
  (n,t) <- binSearch False 1 (1.0, 1.05) wrapped

  -- ONLY if we're in multi-threaded mode do we then run again with
  -- that input size on all threads:
----------------------------------------
-- NOTE, this approach is TOO SLOW.  For workloads that take a massive
-- parallel slowdown it doesn't make sense to use the same input size
-- in serial and in parallel.
-- DISABLING:
{-
  (n2,t2) <- 
    if numthreads > 1 then do
      ptrs <- mapM (const mallocForeignPtr) [1..numthreads]
      tmpchan <- newChan
      putStrLn$ "       [forking threads for multithreaded measurement, input size "++ show n++"]"
      start <- getCPUTime
      tids <- forM ptrs $ \ptr -> forkIO $ 
	       do withForeignPtr ptr (ffn$ fromIntegral n)
		  writeChan tmpchan ()     
      forM ptrs $ \_ -> readChan tmpchan
      end <- getCPUTime
      let t2 :: Double = fromIntegral (end-start) / 1000000000000.0
      putStrLn$ "       [joined threads, time "++ show t2 ++"]"
      return (n * fromIntegral numthreads, t2)
    else do 
      return (n,t)
-}
----------------------------------------

  let total_per_second = round $ fromIntegral n * (1 / t)
      cycles_per = fromIntegral freq * t / fromIntegral n
  print_result total_per_second msg cycles_per
  return total_per_second

-- This lifts the C kernel to operate 
replicate_kernel :: Int -> Kern -> Kern
replicate_kernel numthreads kern n ptr = do
  ptrs <- forM [1..numthreads]
	    (const mallocForeignPtr) 
  tmpchan <- newChan
  -- let childwork = ceiling$ fromIntegral n / fromIntegral numthreads
  let childwork = n -- Keep it the same.. interested in per-thread throughput.
  -- Fork/join pattern:
  tids <- forM ptrs $ \ptr -> forkIO $ 
	   withForeignPtr ptr $ \p -> do
	      kern (fromIntegral childwork) p
	      result <- peek p
	      writeChan tmpchan result

  results <- forM [1..numthreads] $ \_ -> 
	       readChan tmpchan
  -- Meaningless semantics here... sum the child ptrs and write to the input one:
  poke ptr (foldl1 (+) results)
  return ()

----------------------------------------------------------------------------------------------------
-- Main Script

data Flag = NoC | Help | Test
  deriving (Show, Eq)

options = 
   [ Option ['h']  ["help"]  (NoArg Help)  "print program help"
   , Option []     ["noC"]   (NoArg NoC)   "omit C benchmarks, haskell only"
   , Option ['t']  ["test"]  (NoArg Test)  "run some basic tests"
   ]

  
main = do 
   argv <- getArgs
   let (opts,_,other) = getOpt Permute options argv

   -- when (Test `elem` opts)$ do
   --     IA.testIntelAES
   --     NI.testAESNI
   --     exitSuccess

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

   freq <- measure_freq2
   putStrLn$ "  Approx clock frequency:  " ++ commaint freq

   let gamut th = do
       putStrLn$ "  First, timing with System.Random interface:"
       timeit th freq "constant zero gen" (const NoopRNG)
       timeit th freq "System.Random stdGen" mkStdGen
       -- timeit th freq "PureHaskell/reference" BS.mkBurtonGen_reference
       -- timeit th freq "PureHaskell"           BS.mkBurtonGen
       -- timeit th freq "Gladman inefficient"     mkAESGen_gladman0
       -- timeit th freq "Gladman"                 mkAESGen_gladman
       -- timeit th freq "Compound gladman/intel"  IA.mkAESGen

       -- if IA.supportsAESNI then do 
       -- 	  timeit th freq "IntelAES inefficient"    NI.mkAESGen0
       -- 	  timeit th freq "IntelAES"                NI.mkAESGen
       --   else 
       --    putStrLn$ "   [Skipping AESNI-only tests, current machine does not support these instructions.]"

--       when (not$ NoC `elem` opts) $ do
--	  putStrLn$ "  Comparison to C's rand():"
--	  timeit2 th freq "ptr store in C loop"   store_loop
--	  timeit2 th freq "rand/store in C loop"  blast_rands
--	  timeit2 th freq "rand in Haskell loop" (\n ptr -> forM_ [1..n]$ \_ -> rand )
--	  timeit2 th freq "rand/store in Haskell loop"  (\n ptr -> forM_ [1..n]$ \_ -> do n <- rand; poke ptr n )
--	  return ()

   -- Test with 1 thread and numCapabilities threads:
   gamut 1
   when (numCapabilities > 1) $ do 
       putStrLn$ "\nNow "++ show numCapabilities ++" threads, reporting mean randoms-per-second-per-thread:"
       gamut numCapabilities
       return ()

   putStrLn$ "Finished."
