{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Int
import Data.Proxy
import Data.Typeable
import Data.Word
import Foreign.C.Types
import Gauge.Main
import System.Random.SplitMix as SM

import System.Random

main :: IO ()
main = do
  let !sz = 100000
  defaultMain
    [ bgroup "baseline"
      [ let !smGen = SM.mkSMGen 1337 in bench "nextWord32" $ nf (genMany SM.nextWord32 smGen) sz
      , let !smGen = SM.mkSMGen 1337 in bench "nextWord64" $ nf (genMany SM.nextWord64 smGen) sz
      , let !smGen = SM.mkSMGen 1337 in bench "nextInt" $ nf (genMany SM.nextInt smGen) sz
      , let !smGen = SM.mkSMGen 1337 in bench "split" $ nf (genMany SM.splitSMGen smGen) sz
      ]
    , bgroup "pure"
      [ bgroup "random"
        [ pureRandomBench @Float sz
        , pureRandomBench @Double sz
        , pureRandomBench @Integer sz
        ]
      , bgroup "uniform"
        [ pureUniformBench @Word8 sz
        , pureUniformBench @Word16 sz
        , pureUniformBench @Word32 sz
        , pureUniformBench @Word64 sz
        , pureUniformBench @Word sz
        , pureUniformBench @Int8 sz
        , pureUniformBench @Int16 sz
        , pureUniformBench @Int32 sz
        , pureUniformBench @Int64 sz
        , pureUniformBench @Int sz
        , pureUniformBench @Char sz
        , pureUniformBench @Bool sz
        , pureUniformBench @CBool sz
        , pureUniformBench @CChar sz
        , pureUniformBench @CSChar sz
        , pureUniformBench @CUChar sz
        , pureUniformBench @CShort sz
        , pureUniformBench @CUShort sz
        , pureUniformBench @CInt sz
        , pureUniformBench @CUInt sz
        , pureUniformBench @CLong sz
        , pureUniformBench @CULong sz
        , pureUniformBench @CPtrdiff sz
        , pureUniformBench @CSize sz
        , pureUniformBench @CWchar sz
        , pureUniformBench @CSigAtomic sz
        , pureUniformBench @CLLong sz
        , pureUniformBench @CULLong sz
        , pureUniformBench @CIntPtr sz
        , pureUniformBench @CUIntPtr sz
        , pureUniformBench @CIntMax sz
        , pureUniformBench @CUIntMax sz
        ]
      , bgroup "uniformR"
        [ bgroup "full"
          [ pureUniformRFullBench @Word8 sz
          , pureUniformRFullBench @Word16 sz
          , pureUniformRFullBench @Word32 sz
          , pureUniformRFullBench @Word64 sz
          , pureUniformRFullBench @Word sz
          , pureUniformRFullBench @Int8 sz
          , pureUniformRFullBench @Int16 sz
          , pureUniformRFullBench @Int32 sz
          , pureUniformRFullBench @Int64 sz
          , pureUniformRFullBench @Int sz
          , pureUniformRFullBench @Char sz
          , pureUniformRFullBench @Bool sz
          , pureUniformRFullBench @CBool sz
          , pureUniformRFullBench @CChar sz
          , pureUniformRFullBench @CSChar sz
          , pureUniformRFullBench @CUChar sz
          , pureUniformRFullBench @CShort sz
          , pureUniformRFullBench @CUShort sz
          , pureUniformRFullBench @CInt sz
          , pureUniformRFullBench @CUInt sz
          , pureUniformRFullBench @CLong sz
          , pureUniformRFullBench @CULong sz
          , pureUniformRFullBench @CPtrdiff sz
          , pureUniformRFullBench @CSize sz
          , pureUniformRFullBench @CWchar sz
          , pureUniformRFullBench @CSigAtomic sz
          , pureUniformRFullBench @CLLong sz
          , pureUniformRFullBench @CULLong sz
          , pureUniformRFullBench @CIntPtr sz
          , pureUniformRFullBench @CUIntPtr sz
          , pureUniformRFullBench @CIntMax sz
          , pureUniformRFullBench @CUIntMax sz
          ]
        , bgroup "excludeMax"
          [ pureUniformRExcludeMaxBench @Word8 sz
          , pureUniformRExcludeMaxBench @Word16 sz
          , pureUniformRExcludeMaxBench @Word32 sz
          , pureUniformRExcludeMaxBench @Word64 sz
          , pureUniformRExcludeMaxBench @Word sz
          , pureUniformRExcludeMaxBench @Int8 sz
          , pureUniformRExcludeMaxBench @Int16 sz
          , pureUniformRExcludeMaxBench @Int32 sz
          , pureUniformRExcludeMaxBench @Int64 sz
          , pureUniformRExcludeMaxBench @Int sz
          , pureUniformRExcludeMaxEnumBench @Char sz
          , pureUniformRExcludeMaxEnumBench @Bool sz
          , pureUniformRExcludeMaxBench @CBool sz
          , pureUniformRExcludeMaxBench @CChar sz
          , pureUniformRExcludeMaxBench @CSChar sz
          , pureUniformRExcludeMaxBench @CUChar sz
          , pureUniformRExcludeMaxBench @CShort sz
          , pureUniformRExcludeMaxBench @CUShort sz
          , pureUniformRExcludeMaxBench @CInt sz
          , pureUniformRExcludeMaxBench @CUInt sz
          , pureUniformRExcludeMaxBench @CLong sz
          , pureUniformRExcludeMaxBench @CULong sz
          , pureUniformRExcludeMaxBench @CPtrdiff sz
          , pureUniformRExcludeMaxBench @CSize sz
          , pureUniformRExcludeMaxBench @CWchar sz
          , pureUniformRExcludeMaxBench @CSigAtomic sz
          , pureUniformRExcludeMaxBench @CLLong sz
          , pureUniformRExcludeMaxBench @CULLong sz
          , pureUniformRExcludeMaxBench @CIntPtr sz
          , pureUniformRExcludeMaxBench @CUIntPtr sz
          , pureUniformRExcludeMaxBench @CIntMax sz
          , pureUniformRExcludeMaxBench @CUIntMax sz
          ]
        , bgroup "includeHalf"
          [ pureUniformRIncludeHalfBench @Word8 sz
          , pureUniformRIncludeHalfBench @Word16 sz
          , pureUniformRIncludeHalfBench @Word32 sz
          , pureUniformRIncludeHalfBench @Word64 sz
          , pureUniformRIncludeHalfBench @Word sz
          , pureUniformRIncludeHalfBench @Int8 sz
          , pureUniformRIncludeHalfBench @Int16 sz
          , pureUniformRIncludeHalfBench @Int32 sz
          , pureUniformRIncludeHalfBench @Int64 sz
          , pureUniformRIncludeHalfBench @Int sz
          , pureUniformRIncludeHalfEnumBench @Char sz
          , pureUniformRIncludeHalfEnumBench @Bool sz
          , pureUniformRIncludeHalfBench @CBool sz
          , pureUniformRIncludeHalfBench @CChar sz
          , pureUniformRIncludeHalfBench @CSChar sz
          , pureUniformRIncludeHalfBench @CUChar sz
          , pureUniformRIncludeHalfBench @CShort sz
          , pureUniformRIncludeHalfBench @CUShort sz
          , pureUniformRIncludeHalfBench @CInt sz
          , pureUniformRIncludeHalfBench @CUInt sz
          , pureUniformRIncludeHalfBench @CLong sz
          , pureUniformRIncludeHalfBench @CULong sz
          , pureUniformRIncludeHalfBench @CPtrdiff sz
          , pureUniformRIncludeHalfBench @CSize sz
          , pureUniformRIncludeHalfBench @CWchar sz
          , pureUniformRIncludeHalfBench @CSigAtomic sz
          , pureUniformRIncludeHalfBench @CLLong sz
          , pureUniformRIncludeHalfBench @CULLong sz
          , pureUniformRIncludeHalfBench @CIntPtr sz
          , pureUniformRIncludeHalfBench @CUIntPtr sz
          , pureUniformRIncludeHalfBench @CIntMax sz
          , pureUniformRIncludeHalfBench @CUIntMax sz
          ]
        , bgroup "unbounded"
          [ pureUniformRBench @Float (1.23e-4, 5.67e8) sz
          , pureUniformRBench @Double (1.23e-4, 5.67e8) sz
          , let !i = (10 :: Integer) ^ (100 :: Integer)
                !range = (-i - 1, i + 1)
            in pureUniformRBench @Integer range sz
          ]
        ]
      ]
    ]

pureRandomBench :: forall a. (Typeable a, Random a) => Int -> Benchmark
pureRandomBench = let !stdGen = mkStdGen 1337 in pureBench @a (genMany (random @a) stdGen)

pureUniformBench :: forall a. (Typeable a, Uniform a) => Int -> Benchmark
pureUniformBench = let !stdGen = mkStdGen 1337 in pureBench @a (genMany (uniform @_ @a) stdGen)

pureUniformRFullBench :: forall a. (Typeable a, UniformRange a, Bounded a) => Int -> Benchmark
pureUniformRFullBench = let !range = (minBound @a, maxBound @a) in pureUniformRBench range

pureUniformRExcludeMaxBench :: forall a. (Typeable a, UniformRange a, Bounded a, Num a) => Int -> Benchmark
pureUniformRExcludeMaxBench = let !range = (minBound @a, maxBound @a - 1) in pureUniformRBench range

pureUniformRExcludeMaxEnumBench :: forall a. (Typeable a, UniformRange a, Bounded a, Enum a) => Int -> Benchmark
pureUniformRExcludeMaxEnumBench = let !range = (minBound @a, pred (maxBound @a)) in pureUniformRBench range

pureUniformRIncludeHalfBench :: forall a. (Typeable a, UniformRange a, Bounded a, Integral a) => Int -> Benchmark
pureUniformRIncludeHalfBench = let !range = (minBound @a, (maxBound @a `div` 2) + 1) in pureUniformRBench range

pureUniformRIncludeHalfEnumBench :: forall a. (Typeable a, UniformRange a, Bounded a, Enum a) => Int -> Benchmark
pureUniformRIncludeHalfEnumBench =
  let !range = (succ (minBound @a), toEnum ((fromEnum (maxBound @a) `div` 2) + 1))
  in pureUniformRBench range

pureUniformRBench :: forall a. (Typeable a, UniformRange a) => (a, a) -> Int -> Benchmark
pureUniformRBench range =
  let !stdGen = mkStdGen 1337
  in pureBench @a (genMany (uniformR range) stdGen)

pureBench :: forall a. (Typeable a) => (Int -> ()) -> Int -> Benchmark
pureBench f sz = bench (showsTypeRep (typeRep (Proxy :: Proxy a)) "") $ nf f sz

genMany :: (g -> (a, g)) -> g -> Int -> ()
genMany f g0 n = go g0 0
  where
    go g i
      | i < n =
        case f g of
          (x, g') -> x `seq` go g' (i + 1)
      | otherwise = g `seq` ()
