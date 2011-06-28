{-# LANGUAGE CPP #-}
import Control.Monad
import System.Random
import Data.Int
import Data.Word
import Data.Bits
import Foreign.C.Types

-- Take many measurements and record the max/min/average random values.
approxBounds :: (RandomGen g, Random a, Ord a, Num a) => 
		g -> (g -> (a,g)) -> Int -> a -> ((a,a,a),g)
-- Here we do a little hack to essentiall pass in the type in the last argument:
approxBounds initrng nxt iters unused = 
   if False 
   then ((unused,unused,unused),undefined)
   else loop initrng iters 100 (-100) 0 -- Oops, can't use minBound/maxBound here.
 where 
  loop rng 0 mn mx sum = ((mn,mx,sum),rng)
  loop rng  n mn mx sum = 
    case nxt rng of 
      (x, rng') -> loop rng' (n-1) (min x mn) (max x mx) (x+sum)


-- We check that:
--     (1) all generated numbers are in bounds
--     (2) we get "close" to the bounds
-- The with (2) is that we do enough trials to ensure that we can at
-- least hit the 95% mark.

--checkBounds:: (Fractional a, Ord a) =>
checkBounds:: (Real a, Ord a) =>
	      String -> (Bool, a, a) -> (StdGen -> ((a, a, t), StdGen)) -> IO ()
checkBounds msg (exclusive,lo,hi) fun = 
 -- (lo,hi) is [inclusive,exclusive) 
 do putStr$ msg 
--	    ++ ", expected range " ++ show (lo,hi) 
	    ++ ":  "
    (mn,mx,sum) <- getStdRandom fun
    when (mn <  lo)$ error$ "broke lower bound: " ++ show mn
    when (mx > hi) $ error$ "broke upper bound: " ++ show mx
    when (exclusive && mx > hi)$ error$ "hit upper bound: " ++ show mx

    let epsilon = 0.1 * (toRational hi - toRational lo)

    when (toRational (hi - mx) > epsilon)$ error$ "didn't get close enough to upper bound: "++ show mx
    when (toRational (mn - lo) > epsilon)$ error$ "didn't get close enough to lower bound: "++ show mn
    putStrLn "Passed" 

intRange bits = ( False, 0 - x - x, x - 1 + x)
  where x = 2 ^ (bits-2)

wordRange bits = ( False, 0, x - 1 + x )
  where x = 2 ^ (bits-1)

nb = bitSize (0::Int) -- Native bits

trials = 10000
testlist = 
  [ 
    checkBounds "Int"     (intRange nb)  (\g -> approxBounds g random trials (undefined::Int))
  , checkBounds "Integer" (intRange nb)  (\g -> approxBounds g random trials (undefined::Integer))
  , checkBounds "Int8"    (intRange 8)   (\g -> approxBounds g random trials (undefined::Int8))
  , checkBounds "Int16"   (intRange 16)  (\g -> approxBounds g random trials (undefined::Int16))
  , checkBounds "Int32"   (intRange 32)  (\g -> approxBounds g random trials (undefined::Int32))
  , checkBounds "Int64"   (intRange 64)  (\g -> approxBounds g random trials (undefined::Int64))

  , checkBounds "Word"   (wordRange nb) (\g -> approxBounds g random trials (undefined::Word))
  , checkBounds "Word8"  (wordRange 8)  (\g -> approxBounds g random trials (undefined::Word8))
  , checkBounds "Word16" (wordRange 16) (\g -> approxBounds g random trials (undefined::Word16))
  , checkBounds "Word32" (wordRange 32) (\g -> approxBounds g random trials (undefined::Word32))
  , checkBounds "Word64" (wordRange 64) (\g -> approxBounds g random trials (undefined::Word64))

  , checkBounds "Double" (True,0.0,1.0) (\g -> approxBounds g random trials (undefined::Double))
  , checkBounds "Float"  (True,0.0,1.0) (\g -> approxBounds g random trials (undefined::Float))

  , checkBounds "CChar"      (intRange 8)   (\g -> approxBounds g random trials (undefined:: CChar))
  , checkBounds "CSChar"     (intRange 8)   (\g -> approxBounds g random trials (undefined:: CSChar))
  , checkBounds "CUChar"     (wordRange 8)  (\g -> approxBounds g random trials (undefined:: CUChar))
  , checkBounds "CShort"     (intRange 16)  (\g -> approxBounds g random trials (undefined:: CShort))
  , checkBounds "CUShort"    (wordRange 16) (\g -> approxBounds g random trials (undefined:: CUShort))
  , checkBounds "CInt"       (intRange 32)  (\g -> approxBounds g random trials (undefined:: CInt))
  , checkBounds "CUInt"      (wordRange 32) (\g -> approxBounds g random trials (undefined:: CUInt))
  , checkBounds "CLong"      (intRange  nb) (\g -> approxBounds g random trials (undefined:: CLong))
  , checkBounds "CULong"     (wordRange nb) (\g -> approxBounds g random trials (undefined:: CULong))
  , checkBounds "CPtrdiff"   (intRange  nb) (\g -> approxBounds g random trials (undefined:: CPtrdiff))
  , checkBounds "CSize"      (wordRange nb) (\g -> approxBounds g random trials (undefined:: CSize))
  , checkBounds "CWchar"     (intRange 32)  (\g -> approxBounds g random trials (undefined:: CWchar))
  , checkBounds "CSigAtomic" (intRange 32)  (\g -> approxBounds g random trials (undefined:: CWchar))
  , checkBounds "CLLong"     (intRange 64)  (\g -> approxBounds g random trials (undefined:: CLLong))
  , checkBounds "CULLong"    (wordRange 64) (\g -> approxBounds g random trials (undefined:: CULLong))
  , checkBounds "CIntPtr"    (intRange nb)  (\g -> approxBounds g random trials (undefined:: CIntPtr))
  , checkBounds "CUIntPtr"   (wordRange nb) (\g -> approxBounds g random trials (undefined:: CUIntPtr))
  , checkBounds "CIntMax"    (intRange 64)  (\g -> approxBounds g random trials (undefined:: CIntMax))
  , checkBounds "CUIntMax"   (wordRange 64) (\g -> approxBounds g random trials (undefined:: CUIntMax))
  ]

main = sequence_ testlist

-- Untested:
-- instance Random Char where
-- instance Random Bool where
