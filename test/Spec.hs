{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import Data.ByteString.Short as SBS
import Data.Coerce
import Data.Int
import Data.Typeable
import Data.Word
import Foreign.C.Types
import Numeric.Natural (Natural)
import System.Random
import Test.SmallCheck.Series as SC
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck as SC

#include "HsBaseConfig.h"

import qualified Spec.Range as Range
import qualified Spec.Run as Run

main :: IO ()
main =
  defaultMain $
  testGroup
    "Spec"
    [ floatingSpec @Double
    , floatingSpec @Float
    , floatingSpec @CDouble
    , floatingSpec @CFloat
    , integralSpec @Word8
    , integralSpec @Word16
    , integralSpec @Word32
    , integralSpec @Word64
    , integralSpec @Word
    , integralSpec @Int8
    , integralSpec @Int16
    , integralSpec @Int32
    , integralSpec @Int64
    , integralSpec @Int
    , integralSpec @Char
    , integralSpec @Bool
    , integralSpec @CBool
    , integralSpec @CChar
    , integralSpec @CSChar
    , integralSpec @CUChar
    , integralSpec @CShort
    , integralSpec @CUShort
    , integralSpec @CInt
    , integralSpec @CUInt
    , integralSpec @CLong
    , integralSpec @CULong
    , integralSpec @CPtrdiff
    , integralSpec @CSize
    , integralSpec @CWchar
    , integralSpec @CSigAtomic
    , integralSpec @CLLong
    , integralSpec @CULLong
    , integralSpec @CIntPtr
    , integralSpec @CUIntPtr
    , integralSpec @CIntMax
    , integralSpec @CUIntMax
    , integralSpec @Integer
    , integralSpec @Natural
    -- , bitmaskSpec @Word8
    -- , bitmaskSpec @Word16
    -- , bitmaskSpec @Word32
    -- , bitmaskSpec @Word64
    -- , bitmaskSpec @Word
    , runSpec
    , floatTests
    , byteStringSpec
    ]

floatTests :: TestTree
floatTests = testGroup "(Float)"
  [ -- Check that https://github.com/haskell/random/issues/53 does not regress

    testCase "Subnormal generation not above upper bound" $
    [] @?= filter (>4.0e-45) (take 100000 $ randomRs (0, 4.0e-45::Float) $ mkStdGen 0)

  , testCase "Subnormal generation includes upper bound" $
    1.0e-45 `elem` take 100 (randomRs (0, 1.0e-45::Float) $ mkStdGen 0) @?
    "Does not contain 1.0e-45"
  ]

showsType :: forall t . Typeable t => ShowS
showsType = showsTypeRep (typeRep (Proxy :: Proxy t))

byteStringSpec :: TestTree
byteStringSpec =
  testGroup
    "ByteString"
    [ SC.testProperty "genShortByteString" $ \(seed, n8) ->
        let n = fromIntegral (n8 :: Word8) -- no need to generate huge collection of bytes
         in SBS.length (fst (seeded (genShortByteString n) seed)) == n
    , SC.testProperty "genByteString" $ \(seed, n8) ->
        let n = fromIntegral (n8 :: Word8)
         in SBS.toShort (fst (seeded (genByteString n) seed)) ==
            fst (seeded (genShortByteString n) seed)
    ]


rangeSpec ::
     forall a.
     (SC.Serial IO a, Typeable a, Ord a, UniformRange a, Show a)
  => TestTree
rangeSpec =
  testGroup ("Range (" ++ showsType @a ")")
  [ SC.testProperty "uniformR" $ seeded $ Range.uniformRangeWithin @_ @a
  ]

integralSpec ::
     forall a.
     (SC.Serial IO a, Typeable a, Ord a, UniformRange a, Show a)
  => TestTree
integralSpec  =
  testGroup ("(" ++ showsType @a ")")
  [ SC.testProperty "symmetric" $ seeded $ Range.symmetric @_ @a
  , SC.testProperty "bounded" $ seeded $ Range.bounded @_ @a
  , SC.testProperty "singleton" $ seeded $ Range.singleton @_ @a
  , rangeSpec @a
  -- TODO: Add more tests
  ]

floatingSpec ::
     forall a.
     (SC.Serial IO a, Typeable a, Num a, Ord a, Random a, UniformRange a, Show a)
  => TestTree
floatingSpec  =
  testGroup ("(" ++ showsType @a ")")
  [ SC.testProperty "uniformR" $ seeded $ Range.uniformRangeWithin @_ @a
  -- TODO: Add more tests
  ]

runSpec :: TestTree
runSpec = testGroup "runGenState_ and runPrimGenIO_"
    [ SC.testProperty "equal outputs" $ seeded $ \g -> monadic $ Run.runsEqual g ]

-- | Create a StdGen instance from an Int and pass it to the given function.
seeded :: (StdGen -> a) -> Int -> a
seeded f = f . mkStdGen


instance Monad m => Serial m CFloat where
  series = coerce <$> (series :: Series m Float)
instance Monad m => Serial m CDouble where
  series = coerce <$> (series :: Series m Double)
instance Monad m => Serial m CBool where
  series = coerce <$> (series :: Series m HTYPE_BOOL)
instance Monad m => Serial m CChar where
  series = coerce <$> (series :: Series m HTYPE_CHAR)
instance Monad m => Serial m CSChar where
  series = coerce <$> (series :: Series m HTYPE_SIGNED_CHAR)
instance Monad m => Serial m CUChar where
  series = coerce <$> (series :: Series m HTYPE_UNSIGNED_CHAR)
instance Monad m => Serial m CShort where
  series = coerce <$> (series :: Series m HTYPE_SHORT)
instance Monad m => Serial m CUShort where
  series = coerce <$> (series :: Series m HTYPE_UNSIGNED_SHORT)
instance Monad m => Serial m CInt where
  series = coerce <$> (series :: Series m HTYPE_INT)
instance Monad m => Serial m CUInt where
  series = coerce <$> (series :: Series m HTYPE_UNSIGNED_INT)
instance Monad m => Serial m CLong where
  series = coerce <$> (series :: Series m HTYPE_LONG)
instance Monad m => Serial m CULong where
  series = coerce <$> (series :: Series m HTYPE_UNSIGNED_LONG)
instance Monad m => Serial m CPtrdiff where
  series = coerce <$> (series :: Series m HTYPE_PTRDIFF_T)
instance Monad m => Serial m CSize where
  series = coerce <$> (series :: Series m HTYPE_SIZE_T)
instance Monad m => Serial m CWchar where
  series = coerce <$> (series :: Series m HTYPE_WCHAR_T)
instance Monad m => Serial m CSigAtomic where
  series = coerce <$> (series :: Series m HTYPE_SIG_ATOMIC_T)
instance Monad m => Serial m CLLong where
  series = coerce <$> (series :: Series m HTYPE_LONG_LONG)
instance Monad m => Serial m CULLong where
  series = coerce <$> (series :: Series m HTYPE_UNSIGNED_LONG_LONG)
instance Monad m => Serial m CIntPtr where
  series = coerce <$> (series :: Series m HTYPE_INTPTR_T)
instance Monad m => Serial m CUIntPtr where
  series = coerce <$> (series :: Series m HTYPE_UINTPTR_T)
instance Monad m => Serial m CIntMax where
  series = coerce <$> (series :: Series m HTYPE_INTMAX_T)
instance Monad m => Serial m CUIntMax where
  series = coerce <$> (series :: Series m HTYPE_UINTMAX_T)
