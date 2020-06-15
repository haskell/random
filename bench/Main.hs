{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Data.Int
import Data.Proxy
import Data.Typeable
import Data.Word
import Foreign.C.Types
import Gauge.Main

import System.Random

main :: IO ()
main = do
  let !sz = 100000
  defaultMain
    [ bgroup "pure"
      [ bgroup "random"
        [ pureRandomBench (Proxy :: Proxy Float) sz
        , pureRandomBench (Proxy :: Proxy Double) sz
        , pureRandomBench (Proxy :: Proxy Integer) sz
        , pureRandomBench (Proxy :: Proxy Word8) sz
        , pureRandomBench (Proxy :: Proxy Word16) sz
        , pureRandomBench (Proxy :: Proxy Word32) sz
        , pureRandomBench (Proxy :: Proxy Word64) sz
        , pureRandomBench (Proxy :: Proxy Word) sz
        , pureRandomBench (Proxy :: Proxy Int8) sz
        , pureRandomBench (Proxy :: Proxy Int16) sz
        , pureRandomBench (Proxy :: Proxy Int32) sz
        , pureRandomBench (Proxy :: Proxy Int64) sz
        , pureRandomBench (Proxy :: Proxy Int) sz
        , pureRandomBench (Proxy :: Proxy Char) sz
        , pureRandomBench (Proxy :: Proxy Bool) sz
        -- , pureRandomBench (Proxy :: Proxy CBool) sz
        , pureRandomBench (Proxy :: Proxy CChar) sz
        , pureRandomBench (Proxy :: Proxy CSChar) sz
        , pureRandomBench (Proxy :: Proxy CUChar) sz
        , pureRandomBench (Proxy :: Proxy CShort) sz
        , pureRandomBench (Proxy :: Proxy CUShort) sz
        , pureRandomBench (Proxy :: Proxy CInt) sz
        , pureRandomBench (Proxy :: Proxy CUInt) sz
        , pureRandomBench (Proxy :: Proxy CLong) sz
        , pureRandomBench (Proxy :: Proxy CULong) sz
        , pureRandomBench (Proxy :: Proxy CPtrdiff) sz
        , pureRandomBench (Proxy :: Proxy CSize) sz
        , pureRandomBench (Proxy :: Proxy CWchar) sz
        , pureRandomBench (Proxy :: Proxy CSigAtomic) sz
        , pureRandomBench (Proxy :: Proxy CLLong) sz
        , pureRandomBench (Proxy :: Proxy CULLong) sz
        , pureRandomBench (Proxy :: Proxy CIntPtr) sz
        , pureRandomBench (Proxy :: Proxy CUIntPtr) sz
        , pureRandomBench (Proxy :: Proxy CIntMax) sz
        , pureRandomBench (Proxy :: Proxy CUIntMax) sz
        ]
      , bgroup "randomR"
        [ bgroup "full"
          [ pureUniformRFullBench (Proxy :: Proxy Word8) sz
          , pureUniformRFullBench (Proxy :: Proxy Word16) sz
          , pureUniformRFullBench (Proxy :: Proxy Word32) sz
          , pureUniformRFullBench (Proxy :: Proxy Word64) sz
          , pureUniformRFullBench (Proxy :: Proxy Word) sz
          , pureUniformRFullBench (Proxy :: Proxy Int8) sz
          , pureUniformRFullBench (Proxy :: Proxy Int16) sz
          , pureUniformRFullBench (Proxy :: Proxy Int32) sz
          , pureUniformRFullBench (Proxy :: Proxy Int64) sz
          , pureUniformRFullBench (Proxy :: Proxy Int) sz
          , pureUniformRFullBench (Proxy :: Proxy Char) sz
          , pureUniformRFullBench (Proxy :: Proxy Bool) sz
          -- , pureUniformRFullBench (Proxy :: Proxy CBool) sz
          , pureUniformRFullBench (Proxy :: Proxy CChar) sz
          , pureUniformRFullBench (Proxy :: Proxy CSChar) sz
          , pureUniformRFullBench (Proxy :: Proxy CUChar) sz
          , pureUniformRFullBench (Proxy :: Proxy CShort) sz
          , pureUniformRFullBench (Proxy :: Proxy CUShort) sz
          , pureUniformRFullBench (Proxy :: Proxy CInt) sz
          , pureUniformRFullBench (Proxy :: Proxy CUInt) sz
          , pureUniformRFullBench (Proxy :: Proxy CLong) sz
          , pureUniformRFullBench (Proxy :: Proxy CULong) sz
          , pureUniformRFullBench (Proxy :: Proxy CPtrdiff) sz
          , pureUniformRFullBench (Proxy :: Proxy CSize) sz
          , pureUniformRFullBench (Proxy :: Proxy CWchar) sz
          , pureUniformRFullBench (Proxy :: Proxy CSigAtomic) sz
          , pureUniformRFullBench (Proxy :: Proxy CLLong) sz
          , pureUniformRFullBench (Proxy :: Proxy CULLong) sz
          , pureUniformRFullBench (Proxy :: Proxy CIntPtr) sz
          , pureUniformRFullBench (Proxy :: Proxy CUIntPtr) sz
          , pureUniformRFullBench (Proxy :: Proxy CIntMax) sz
          , pureUniformRFullBench (Proxy :: Proxy CUIntMax) sz
          ]
        , bgroup "excludeMax"
          [ pureUniformRExcludeMaxBench (Proxy :: Proxy Word8) sz
          , pureUniformRExcludeMaxBench (Proxy :: Proxy Word16) sz
          , pureUniformRExcludeMaxBench (Proxy :: Proxy Word32) sz
          , pureUniformRExcludeMaxBench (Proxy :: Proxy Word64) sz
          , pureUniformRExcludeMaxBench (Proxy :: Proxy Word) sz
          , pureUniformRExcludeMaxBench (Proxy :: Proxy Int8) sz
          , pureUniformRExcludeMaxBench (Proxy :: Proxy Int16) sz
          , pureUniformRExcludeMaxBench (Proxy :: Proxy Int32) sz
          , pureUniformRExcludeMaxBench (Proxy :: Proxy Int64) sz
          , pureUniformRExcludeMaxBench (Proxy :: Proxy Int) sz
          , pureUniformRExcludeMaxBench (Proxy :: Proxy Char) sz
          -- , pureUniformRExcludeMaxBench (Proxy :: Proxy CBool) sz
          , pureUniformRExcludeMaxBench (Proxy :: Proxy Bool) sz
          , pureUniformRExcludeMaxBench (Proxy :: Proxy CChar) sz
          , pureUniformRExcludeMaxBench (Proxy :: Proxy CSChar) sz
          , pureUniformRExcludeMaxBench (Proxy :: Proxy CUChar) sz
          , pureUniformRExcludeMaxBench (Proxy :: Proxy CShort) sz
          , pureUniformRExcludeMaxBench (Proxy :: Proxy CUShort) sz
          , pureUniformRExcludeMaxBench (Proxy :: Proxy CInt) sz
          , pureUniformRExcludeMaxBench (Proxy :: Proxy CUInt) sz
          , pureUniformRExcludeMaxBench (Proxy :: Proxy CLong) sz
          , pureUniformRExcludeMaxBench (Proxy :: Proxy CULong) sz
          , pureUniformRExcludeMaxBench (Proxy :: Proxy CPtrdiff) sz
          , pureUniformRExcludeMaxBench (Proxy :: Proxy CSize) sz
          , pureUniformRExcludeMaxBench (Proxy :: Proxy CWchar) sz
          , pureUniformRExcludeMaxBench (Proxy :: Proxy CSigAtomic) sz
          , pureUniformRExcludeMaxBench (Proxy :: Proxy CLLong) sz
          , pureUniformRExcludeMaxBench (Proxy :: Proxy CULLong) sz
          , pureUniformRExcludeMaxBench (Proxy :: Proxy CIntPtr) sz
          , pureUniformRExcludeMaxBench (Proxy :: Proxy CUIntPtr) sz
          , pureUniformRExcludeMaxBench (Proxy :: Proxy CIntMax) sz
          , pureUniformRExcludeMaxBench (Proxy :: Proxy CUIntMax) sz
          ]
        , bgroup "includeHalf"
          [ pureUniformRIncludeHalfBench (Proxy :: Proxy Word8) sz
          , pureUniformRIncludeHalfBench (Proxy :: Proxy Word16) sz
          , pureUniformRIncludeHalfBench (Proxy :: Proxy Word32) sz
          , pureUniformRIncludeHalfBench (Proxy :: Proxy Word64) sz
          , pureUniformRIncludeHalfBench (Proxy :: Proxy Word) sz
          , pureUniformRIncludeHalfBench (Proxy :: Proxy Int8) sz
          , pureUniformRIncludeHalfBench (Proxy :: Proxy Int16) sz
          , pureUniformRIncludeHalfBench (Proxy :: Proxy Int32) sz
          , pureUniformRIncludeHalfBench (Proxy :: Proxy Int64) sz
          , pureUniformRIncludeHalfBench (Proxy :: Proxy Int) sz
          , pureUniformRIncludeHalfEnumBench (Proxy :: Proxy Char) sz
          -- , pureUniformRIncludeHalfEnumBench (Proxy :: Proxy CBool) sz
          , pureUniformRIncludeHalfEnumBench (Proxy :: Proxy Bool) sz
          , pureUniformRIncludeHalfBench (Proxy :: Proxy CChar) sz
          , pureUniformRIncludeHalfBench (Proxy :: Proxy CSChar) sz
          , pureUniformRIncludeHalfBench (Proxy :: Proxy CUChar) sz
          , pureUniformRIncludeHalfBench (Proxy :: Proxy CShort) sz
          , pureUniformRIncludeHalfBench (Proxy :: Proxy CUShort) sz
          , pureUniformRIncludeHalfBench (Proxy :: Proxy CInt) sz
          , pureUniformRIncludeHalfBench (Proxy :: Proxy CUInt) sz
          , pureUniformRIncludeHalfBench (Proxy :: Proxy CLong) sz
          , pureUniformRIncludeHalfBench (Proxy :: Proxy CULong) sz
          , pureUniformRIncludeHalfBench (Proxy :: Proxy CPtrdiff) sz
          , pureUniformRIncludeHalfBench (Proxy :: Proxy CSize) sz
          , pureUniformRIncludeHalfBench (Proxy :: Proxy CWchar) sz
          , pureUniformRIncludeHalfBench (Proxy :: Proxy CSigAtomic) sz
          , pureUniformRIncludeHalfBench (Proxy :: Proxy CLLong) sz
          , pureUniformRIncludeHalfBench (Proxy :: Proxy CULLong) sz
          , pureUniformRIncludeHalfBench (Proxy :: Proxy CIntPtr) sz
          , pureUniformRIncludeHalfBench (Proxy :: Proxy CUIntPtr) sz
          , pureUniformRIncludeHalfBench (Proxy :: Proxy CIntMax) sz
          , pureUniformRIncludeHalfBench (Proxy :: Proxy CUIntMax) sz
          ]
        , bgroup "unbounded"
          [ pureUniformRBench (Proxy :: Proxy Float) (1.23e-4, 5.67e8) sz
          , pureUniformRBench (Proxy :: Proxy Double) (1.23e-4, 5.67e8) sz
          , let !i = (10 :: Integer) ^ (100 :: Integer)
                !range = (-i - 1, i + 1)
            in pureUniformRBench (Proxy :: Proxy Integer) range sz
          -- , let !n = (10 :: Natural) ^ (100 :: Natural)
          --       !range = (1, n - 1)
          --   in pureUniformRBench (Proxy :: Proxy Natural) range sz
          ]
        ]
      ]
    ]

pureRandomBench :: forall a. (Typeable a, Random a) => Proxy a -> Int -> Benchmark
pureRandomBench px =
  let !stdGen = mkStdGen 1337
   in pureBench px (genMany (random :: StdGen -> (a, StdGen)) stdGen)

pureUniformRFullBench ::
     forall a. (Typeable a, Random a, Bounded a)
  => Proxy a
  -> Int
  -> Benchmark
pureUniformRFullBench px =
  let range = (minBound :: a, maxBound :: a)
   in pureUniformRBench px range

pureUniformRExcludeMaxBench ::
     forall a. (Typeable a, Random a, Bounded a, Enum a)
  => Proxy a
  -> Int
  -> Benchmark
pureUniformRExcludeMaxBench px =
  let range = (minBound :: a, pred (maxBound :: a))
   in pureUniformRBench px range

pureUniformRIncludeHalfBench ::
     forall a. (Typeable a, Random a, Bounded a, Integral a)
  => Proxy a
  -> Int
  -> Benchmark
pureUniformRIncludeHalfBench px =
  let range = ((minBound :: a) + 1, ((maxBound :: a) `div` 2) + 1)
  in pureUniformRBench px range

pureUniformRIncludeHalfEnumBench ::
     forall a. (Typeable a, Random a, Bounded a, Enum a)
  => Proxy a
  -> Int
  -> Benchmark
pureUniformRIncludeHalfEnumBench px =
  let range = (succ (minBound :: a), toEnum ((fromEnum (maxBound :: a) `div` 2) + 1))
  in pureUniformRBench px range

pureUniformRBench ::
     forall a. (Typeable a, Random a)
  => Proxy a
  -> (a, a)
  -> Int
  -> Benchmark
pureUniformRBench px range@(!_, !_) =
  let !stdGen = mkStdGen 1337
  in pureBench px (genMany (randomR range) stdGen)

pureBench :: forall a. (Typeable a) => Proxy a -> (Int -> ()) -> Int -> Benchmark
pureBench px f sz = bench (showsTypeRep (typeRep px) "") $ nf f sz

genMany :: (g -> (a, g)) -> g -> Int -> ()
genMany f g0 n = go g0 0
  where
    go g i
      | i < n =
        case f g of
          (x, g') -> x `seq` go g' (i + 1)
      | otherwise = g `seq` ()
