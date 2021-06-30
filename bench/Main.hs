{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Monad
import Control.Monad.State.Strict
import Data.Int
import Data.Proxy
import Data.Typeable
import Data.Word
import Foreign.C.Types
import Numeric.Natural (Natural)
import System.Random.SplitMix as SM
import Test.Tasty.Bench
import Control.Monad.Primitive
import Data.Primitive.PrimArray
import Data.Primitive.Types

import System.Random.Stateful

seed :: Int
seed = 1337

main :: IO ()
main = do
  let !sz = 100000
      genLengths =
        -- create 5000 small lengths that are needed for ShortByteString generation
        runStateGen (mkStdGen 2020) $ \g -> replicateM 5000 (uniformRM (16 + 1, 16 + 7) g)
  setStdGen $ mkStdGen seed
  defaultMain
    [ bgroup "baseline"
      [ env (pure $ SM.mkSMGen $ fromIntegral seed) $ \smGen ->
          bench "nextWord32" $ whnf (genMany SM.nextWord32 smGen) sz
      , env (pure $ SM.mkSMGen $ fromIntegral seed) $ \smGen ->
          bench "nextWord64" $ whnf (genMany SM.nextWord64 smGen) sz
      , env (pure $ SM.mkSMGen $ fromIntegral seed) $ \smGen ->
          bench "nextInt" $ whnf (genMany SM.nextInt smGen) sz
      , env (pure $ SM.mkSMGen $ fromIntegral seed) $ \smGen ->
          bench "split" $ whnf (genMany SM.splitSMGen smGen) sz
      ]
    , bgroup "pure"
      [ bgroup "random"
        [ pureBench random sz (Proxy :: Proxy Word8)
        , pureBench random sz (Proxy :: Proxy Word16)
        , pureBench random sz (Proxy :: Proxy Word32)
        , pureBench random sz (Proxy :: Proxy Word64)
        , pureBench random sz (Proxy :: Proxy Int8)
        , pureBench random sz (Proxy :: Proxy Int16)
        , pureBench random sz (Proxy :: Proxy Int32)
        , pureBench random sz (Proxy :: Proxy Int64)
        , pureBench random sz (Proxy :: Proxy Bool)
        , pureBench random sz (Proxy :: Proxy Char)
        , pureBench random sz (Proxy :: Proxy Float)
        , pureBench random sz (Proxy :: Proxy Double)
        , pureBench random sz (Proxy :: Proxy Integer)
        ]
      , bgroup "uniform"
        [ pureBench uniform sz (Proxy :: Proxy Word8)
        , pureBench uniform sz (Proxy :: Proxy Word16)
        , pureBench uniform sz (Proxy :: Proxy Word32)
        , pureBench uniform sz (Proxy :: Proxy Word64)
        , pureBench uniform sz (Proxy :: Proxy Int8)
        , pureBench uniform sz (Proxy :: Proxy Int16)
        , pureBench uniform sz (Proxy :: Proxy Int32)
        , pureBench uniform sz (Proxy :: Proxy Int64)
        , pureBench uniform sz (Proxy :: Proxy Bool)
        , pureBench uniform sz (Proxy :: Proxy Char)
        , pureBench uniform sz (Proxy :: Proxy CChar)
        , pureBench uniform sz (Proxy :: Proxy CSChar)
        , pureBench uniform sz (Proxy :: Proxy CUChar)
        , pureBench uniform sz (Proxy :: Proxy CShort)
        , pureBench uniform sz (Proxy :: Proxy CUShort)
        , pureBench uniform sz (Proxy :: Proxy CInt)
        , pureBench uniform sz (Proxy :: Proxy CUInt)
        , pureBench uniform sz (Proxy :: Proxy CLong)
        , pureBench uniform sz (Proxy :: Proxy CULong)
        , pureBench uniform sz (Proxy :: Proxy CPtrdiff)
        , pureBench uniform sz (Proxy :: Proxy CSize)
        , pureBench uniform sz (Proxy :: Proxy CWchar)
        , pureBench uniform sz (Proxy :: Proxy CSigAtomic)
        , pureBench uniform sz (Proxy :: Proxy CLLong)
        , pureBench uniform sz (Proxy :: Proxy CULLong)
        , pureBench uniform sz (Proxy :: Proxy CIntPtr)
        , pureBench uniform sz (Proxy :: Proxy CUIntPtr)
        , pureBench uniform sz (Proxy :: Proxy CIntMax)
        , pureBench uniform sz (Proxy :: Proxy CUIntMax)
        ]
      , bgroup "uniformR"
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
          , let !n = (10 :: Natural) ^ (100 :: Natural)
                !range = (1, n - 1)
            in pureUniformRBench (Proxy :: Proxy Natural) range sz
          ]
        , bgroup "floating"
          [ bgroup "IO"
            [ env ((,) <$> getStdGen <*> newAlignedPinnedPrimArray sz) $ \ ~(gen, ma) ->
                bench "uniformFloat01M" $
                nfIO (runStateGenT gen (fillMutablePrimArrayM uniformFloat01M ma))
            , env ((,) <$> getStdGen <*> newAlignedPinnedPrimArray sz) $ \ ~(gen, ma) ->
                bench "uniformFloatPositive01M" $
                nfIO (runStateGenT gen (fillMutablePrimArrayM uniformFloatPositive01M ma))
            , env ((,) <$> getStdGen <*> newAlignedPinnedPrimArray sz) $ \ ~(gen, ma) ->
                bench "uniformDouble01M" $
                nfIO (runStateGenT gen (fillMutablePrimArrayM uniformDouble01M ma))
            , env ((,) <$> getStdGen <*> newAlignedPinnedPrimArray sz) $ \ ~(gen, ma) ->
                bench "uniformDoublePositive01M" $
                nfIO (runStateGenT gen (fillMutablePrimArrayM uniformDoublePositive01M ma))
            ]
          , bgroup "State"
            [ env getStdGen $
                bench "uniformFloat01M" . nf (`runStateGen` (replicateM_ sz . uniformFloat01M))
            , env getStdGen $
                bench "uniformFloatPositive01M" .
                nf (`runStateGen` (replicateM_ sz . uniformFloatPositive01M))
            , env getStdGen $
                bench "uniformDouble01M" . nf (`runStateGen` (replicateM_ sz . uniformDouble01M))
            , env getStdGen $
                bench "uniformDoublePositive01M" .
                nf (`runStateGen` (replicateM_ sz . uniformDoublePositive01M))
            ]
          , bgroup "pure"
            [ env getStdGen $ \gen ->
                bench "uniformFloat01M" $ nf
                (genMany (runState $ uniformFloat01M (StateGenM :: StateGenM StdGen)) gen)
                sz
            , env getStdGen $ \gen ->
                bench "uniformFloatPositive01M" $ nf
                (genMany (runState $ uniformFloatPositive01M (StateGenM :: StateGenM StdGen)) gen)
                sz
            , env getStdGen $ \gen ->
                bench "uniformDouble01M" $ nf
                (genMany (runState $ uniformDouble01M (StateGenM :: StateGenM StdGen)) gen)
                sz
            , env getStdGen $ \gen ->
                bench "uniformDoublePositive01M" $ nf
                (genMany (runState $ uniformDoublePositive01M (StateGenM :: StateGenM StdGen)) gen)
                sz
            ]
          ]
        , bgroup "ShortByteString"
          [ env (pure genLengths) $ \ ~(ns, gen) ->
              bench "genShortByteString" $
              nfIO $ runStateGenT gen $ \g -> mapM (`uniformShortByteString` g) ns
          ]
        ]
      ]
    ]

pureUniformRFullBench ::
     forall a. (Typeable a, UniformRange a, Bounded a)
  => Proxy a
  -> Int
  -> Benchmark
pureUniformRFullBench px =
  let range = (minBound :: a, maxBound :: a)
   in pureUniformRBench px range
{-# INLINE pureUniformRFullBench #-}

pureUniformRExcludeMaxBench ::
     forall a. (Typeable a, UniformRange a, Bounded a, Enum a)
  => Proxy a
  -> Int
  -> Benchmark
pureUniformRExcludeMaxBench px =
  let range = (minBound :: a, pred (maxBound :: a))
   in pureUniformRBench px range
{-# INLINE pureUniformRExcludeMaxBench #-}

pureUniformRIncludeHalfBench ::
     forall a. (Typeable a, UniformRange a, Bounded a, Integral a)
  => Proxy a
  -> Int
  -> Benchmark
pureUniformRIncludeHalfBench px =
  let range = ((minBound :: a) + 1, ((maxBound :: a) `div` 2) + 1)
  in pureUniformRBench px range
{-# INLINE pureUniformRIncludeHalfBench #-}

pureUniformRIncludeHalfEnumBench ::
     forall a. (Typeable a, UniformRange a, Bounded a, Enum a)
  => Proxy a
  -> Int
  -> Benchmark
pureUniformRIncludeHalfEnumBench px =
  let range = (succ (minBound :: a), toEnum ((fromEnum (maxBound :: a) `div` 2) + 1))
  in pureUniformRBench px range
{-# INLINE pureUniformRIncludeHalfEnumBench #-}

pureUniformRBench ::
     forall a. (Typeable a, UniformRange a)
  => Proxy a
  -> (a, a)
  -> Int
  -> Benchmark
pureUniformRBench px range@(!_, !_) sz = pureBench (uniformR range) sz px
{-# INLINE pureUniformRBench #-}

pureBench ::
     forall a. Typeable a
  => (StdGen -> (a, StdGen))
  -> Int
  -> Proxy a
  -> Benchmark
pureBench f sz px =
  env getStdGen $ \gen ->
    bench (showsTypeRep (typeRep px) "") $ whnf (genMany f gen) sz
{-# INLINE pureBench #-}


genMany :: (g -> (a, g)) -> g -> Int -> a
genMany f g0 n = go 0 $ f g0
  where
    go i (!y, !g)
      | i < n = go (i + 1) $ f g
      | otherwise = y


fillMutablePrimArrayM ::
     (Prim a, PrimMonad m)
  => (gen -> m a)
  -> MutablePrimArray (PrimState m) a
  -> gen
  -> m (PrimArray a)
fillMutablePrimArrayM f ma g = do
  n <- getSizeofMutablePrimArray ma
  let go i
        | i < n = f g >>= writePrimArray ma i >> go (i + 1)
        | otherwise = pure ()
  go 0
  unsafeFreezePrimArray ma
