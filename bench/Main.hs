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
  let !sz = 1000000
  defaultMain
    [ bgroup "baseline"
      [ let !stdGen = mkStdGen 1337 in bench "nextWord32" $ nf (genMany SM.nextWord32 stdGen) sz
      , let !stdGen = mkStdGen 1337 in bench "nextWord64" $ nf (genMany SM.nextWord64 stdGen) sz
      , let !stdGen = mkStdGen 1337 in bench "nextInt" $ nf (genMany SM.nextInt stdGen) sz
      , let !stdGen = mkStdGen 1337 in bench "split" $ nf (genMany SM.splitSMGen stdGen) sz
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
      ]
    ]

pureRandomBench :: forall a. (Typeable a, Random a) => Int -> Benchmark
pureRandomBench = let !stdGen = mkStdGen 1337 in pureBench @a (genManyRandom @a stdGen)

pureUniformBench :: forall a. (Typeable a, Uniform a) => Int -> Benchmark
pureUniformBench = let !stdGen = mkStdGen 1337 in pureBench @a (genManyUniform @a stdGen)

pureBench :: forall a. (Typeable a) => (Int -> ()) -> Int -> Benchmark
pureBench f sz = bench (showsTypeRep (typeRep (Proxy :: Proxy a)) "") $ nf f sz

genManyRandom :: forall a g. (Random a, RandomGen g) => g -> Int -> ()
genManyRandom = genMany (random @a)

genManyUniform :: forall a g. (Uniform a, RandomGen g) => g -> Int -> ()
genManyUniform = genMany (uniform @g @a)

genMany :: (g -> (a, g)) -> g -> Int -> ()
genMany f g0 n = go g0 0
  where
    go g i
      | i < n =
        case f g of
          (x, g') -> x `seq` go g' (i + 1)
      | otherwise = g `seq` ()
