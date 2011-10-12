{-# LANGUAGE CPP #-}
import Control.Monad
import System.Random
import Data.Int
import Data.Word
import Data.Bits
import Foreign.C.Types

-- Take many measurements and record the max/min/average random values.
approxBounds :: (RandomGen g, Random a, Ord a, Num a) => 
		(g -> (a,g)) -> Int -> a -> (a,a) -> g -> ((a,a,a),g)
-- Here we do a little hack to essentiall pass in the type in the last argument:
approxBounds nxt iters unused (explo,exphi) initrng = 
   if False 
   then ((unused,unused,unused),undefined)
--   else loop initrng iters 100 (-100) 0 -- Oops, can't use minBound/maxBound here.
   else loop initrng iters exphi explo 0 -- Oops, can't use minBound/maxBound here.
 where 
  loop rng 0 mn mx sum = ((mn,mx,sum),rng)
  loop rng  n mn mx sum = 
    case nxt rng of 
      (x, rng') -> loop rng' (n-1) (min x mn) (max x mx) (x+sum)


-- We check that:
--     (1) all generated numbers are in bounds
--     (2) we get "close" to the bounds
-- The with (2) is that we do enough trials to ensure that we can at
-- least hit the 90% mark.
checkBounds:: (Real a, Show a, Ord a) =>
	      String -> (Bool, a, a) -> ((a,a) -> StdGen -> ((a, a, t), StdGen)) -> IO ()
checkBounds msg (exclusive,lo,hi) fun = 
 -- (lo,hi) is [inclusive,exclusive) 
 do putStr$ msg 
--	    ++ ", expected range " ++ show (lo,hi) 
	    ++ ":  "
    (mn,mx,sum) <- getStdRandom (fun (lo,hi))
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

trials = 5000
nb = bitSize (0::Int) -- Native bits

main = 
 do 
    checkBounds "Int"     (intRange nb)  (approxBounds random trials (undefined::Int))
    checkBounds "Integer" (intRange nb)  (approxBounds random trials (undefined::Integer))
--    checkBounds "Integer Rbig"    (False,-(2^500), 2^500) (approxBounds (randomR (-(2^500), 2^500)) trials (undefined::Integer))
--    checkBounds "Integer RbigPos" (False,1,2^5000)        (approxBounds (randomR (1,2^5000))        trials (undefined::Integer))
    checkBounds "Int8"    (intRange 8)   (approxBounds random trials (undefined::Int8))
    checkBounds "Int16"   (intRange 16)  (approxBounds random trials (undefined::Int16))
    checkBounds "Int32"   (intRange 32)  (approxBounds random trials (undefined::Int32))
    checkBounds "Int64"   (intRange 64)  (approxBounds random trials (undefined::Int64))
    checkBounds "Word"   (wordRange nb ) (approxBounds random trials (undefined::Word))
    checkBounds "Word8"  (wordRange 8)   (approxBounds random trials (undefined::Word8))
    checkBounds "Word16" (wordRange 16)  (approxBounds random trials (undefined::Word16))
    checkBounds "Word32" (wordRange 32)  (approxBounds random trials (undefined::Word32))
    checkBounds "Word64" (wordRange 64)  (approxBounds random trials (undefined::Word64))
    checkBounds "Double" (True,0.0,1.0)  (approxBounds random trials (undefined::Double))
    checkBounds "Float"  (True,0.0,1.0)  (approxBounds random trials (undefined::Float))

    checkBounds "CChar"      (intRange 8)   (approxBounds random trials (undefined:: CChar))
    checkBounds "CSChar"     (intRange 8)   (approxBounds random trials (undefined:: CSChar))
    checkBounds "CUChar"     (wordRange 8)  (approxBounds random trials (undefined:: CUChar))
    checkBounds "CShort"     (intRange 16)  (approxBounds random trials (undefined:: CShort))
    checkBounds "CUShort"    (wordRange 16) (approxBounds random trials (undefined:: CUShort))
    checkBounds "CInt"       (intRange 32)  (approxBounds random trials (undefined:: CInt))
    checkBounds "CUInt"      (wordRange 32) (approxBounds random trials (undefined:: CUInt))
    checkBounds "CLong"      (intRange  nb) (approxBounds random trials (undefined:: CLong))
    checkBounds "CULong"     (wordRange nb) (approxBounds random trials (undefined:: CULong))
    checkBounds "CPtrdiff"   (intRange  nb) (approxBounds random trials (undefined:: CPtrdiff))
    checkBounds "CSize"      (wordRange nb) (approxBounds random trials (undefined:: CSize))
    checkBounds "CWchar"     (intRange 32)  (approxBounds random trials (undefined:: CWchar))
    checkBounds "CSigAtomic" (intRange 32)  (approxBounds random trials (undefined:: CWchar))
    checkBounds "CLLong"     (intRange 64)  (approxBounds random trials (undefined:: CLLong))
    checkBounds "CULLong"    (wordRange 64) (approxBounds random trials (undefined:: CULLong))
    checkBounds "CIntPtr"    (intRange nb)  (approxBounds random trials (undefined:: CIntPtr))
    checkBounds "CUIntPtr"   (wordRange nb) (approxBounds random trials (undefined:: CUIntPtr))
    checkBounds "CIntMax"    (intRange 64)  (approxBounds random trials (undefined:: CIntMax))
    checkBounds "CUIntMax"   (wordRange 64) (approxBounds random trials (undefined:: CUIntMax))

  -- Then check all the range-restricted versions:
    checkBounds "Int R"     (False,-100,100)  (approxBounds (randomR (-100,100)) trials (undefined::Int))
    checkBounds "Integer R" (False,-100,100)  (approxBounds (randomR (-100,100)) trials (undefined::Integer))
    checkBounds "Int8 R"    (False,-100,100)  (approxBounds (randomR (-100,100)) trials (undefined::Int8))
    checkBounds "Int8 Rsmall" (False,-50,50)  (approxBounds (randomR (-50,50))   trials (undefined::Int8))
    checkBounds "Int8 Rmini"    (False,3,4)   (approxBounds (randomR (3,4))      trials (undefined::Int8))
    checkBounds "Int8 Rtrivial" (False,3,3)   (approxBounds (randomR (3,3))      trials (undefined::Int8))

    checkBounds "Int16 R"   (False,-100,100)  (approxBounds (randomR (-100,100)) trials (undefined::Int16))
    checkBounds "Int32 R"   (False,-100,100)  (approxBounds (randomR (-100,100)) trials (undefined::Int32))
    checkBounds "Int64 R"   (False,-100,100)  (approxBounds (randomR (-100,100)) trials (undefined::Int64))
    checkBounds "Word R"    (False,0,200)     (approxBounds (randomR (0,200))    trials (undefined::Word))
    checkBounds "Word8 R"   (False,0,200)     (approxBounds (randomR (0,200))    trials (undefined::Word8))
    checkBounds "Word16 R"  (False,0,200)     (approxBounds (randomR (0,200))    trials (undefined::Word16))
    checkBounds "Word32 R"  (False,0,200)     (approxBounds (randomR (0,200))    trials (undefined::Word32))
    checkBounds "Word64 R"  (False,0,200)     (approxBounds (randomR (0,200))    trials (undefined::Word64))
    checkBounds "Double R" (True,10.0,77.0)   (approxBounds (randomR (10,77)) trials (undefined::Double))
    checkBounds "Float R"  (True,10.0,77.0)   (approxBounds (randomR (10,77)) trials (undefined::Float))

    checkBounds "CChar R"   (False,-100,100)     (approxBounds (randomR (-100,100)) trials (undefined:: CChar))
    checkBounds "CSChar R"  (False,-100,100)     (approxBounds (randomR (-100,100)) trials (undefined:: CSChar))
    checkBounds "CUChar R"  (False,0,200)        (approxBounds (randomR (0,200))    trials (undefined:: CUChar))
    checkBounds "CShort R"  (False,-100,100)     (approxBounds (randomR (-100,100)) trials (undefined:: CShort))
    checkBounds "CUShort R" (False,0,200)        (approxBounds (randomR (0,200))    trials (undefined:: CUShort))
    checkBounds "CInt R"    (False,-100,100)     (approxBounds (randomR (-100,100)) trials (undefined:: CInt))
    checkBounds "CUInt R"   (False,0,200)        (approxBounds (randomR (0,200))    trials (undefined:: CUInt))
    checkBounds "CLong R"   (False,-100,100)     (approxBounds (randomR (-100,100)) trials (undefined:: CLong))
    checkBounds "CULong R"     (False,0,200)     (approxBounds (randomR (0,200))    trials (undefined:: CULong))
    checkBounds "CPtrdiff R"   (False,-100,100)  (approxBounds (randomR (-100,100)) trials (undefined:: CPtrdiff))
    checkBounds "CSize R"      (False,0,200)     (approxBounds (randomR (0,200))    trials (undefined:: CSize))
    checkBounds "CWchar R"     (False,-100,100)  (approxBounds (randomR (-100,100)) trials (undefined:: CWchar))
    checkBounds "CSigAtomic R" (False,-100,100)  (approxBounds (randomR (-100,100)) trials (undefined:: CWchar))
    checkBounds "CLLong R"     (False,-100,100)  (approxBounds (randomR (-100,100)) trials (undefined:: CLLong))
    checkBounds "CULLong R"    (False,0,200)     (approxBounds (randomR (0,200))    trials (undefined:: CULLong))
    checkBounds "CIntPtr R"    (False,-100,100)  (approxBounds (randomR (-100,100)) trials (undefined:: CIntPtr))
    checkBounds "CUIntPtr R"   (False,0,200)     (approxBounds (randomR (0,200))    trials (undefined:: CUIntPtr))
    checkBounds "CIntMax R"    (False,-100,100)  (approxBounds (randomR (-100,100)) trials (undefined:: CIntMax))
    checkBounds "CUIntMax R"   (False,0,200)     (approxBounds (randomR (0,200))    trials (undefined:: CUIntMax))

-- Untested:
-- instance Random Char where
-- instance Random Bool where
