{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import Data.Coerce
import Data.Word
import Data.Int
import System.Random
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.SmallCheck.Series as SC
import Data.Typeable
import Foreign.C.Types

--import qualified Spec.Bitmask as Bitmask
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
    , rangeSpec @Char
    , rangeSpec @Bool
    -- , bitmaskSpec @Word8
    -- , bitmaskSpec @Word16
    -- , bitmaskSpec @Word32
    -- , bitmaskSpec @Word64
    -- , bitmaskSpec @Word
    , runSpec
    ]

showsType :: forall t . Typeable t => ShowS
showsType = showsTypeRep (typeRep (Proxy :: Proxy t))

-- bitmaskSpec ::
--      forall a.
--      (SC.Serial IO a, Typeable a, Num a, Ord a, Random a, FiniteBits a, Show a)
--   => TestTree
-- bitmaskSpec =
--   testGroup ("bitmaskWithRejection (" ++ showsType @a ")")
--   [ SC.testProperty "symmetric" $ seeded $ Bitmask.symmetric @_ @a
--   , SC.testProperty "bounded" $ seeded $ Bitmask.bounded @_ @a
--   , SC.testProperty "singleton" $ seeded $ Bitmask.singleton @_ @a
--   ]

rangeSpec ::
     forall a.
     (SC.Serial IO a, Typeable a, Ord a, Random a, UniformRange a, Show a)
  => TestTree
rangeSpec =
  testGroup ("Range (" ++ showsType @a ")")
  [ SC.testProperty "uniformR" $ seeded $ Range.uniformRangeWithin @_ @a
  ]

integralSpec ::
     forall a.
     (SC.Serial IO a, Typeable a, Ord a, Random a, UniformRange a, Show a)
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
  [ SC.testProperty "uniformR" $ seeded $ Range.uniformRangeWithinExcluded @_ @a
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
instance Monad m => Serial m CChar where
  series = coerce <$> (series :: Series m Int8)
instance Monad m => Serial m CSChar where
  series = coerce <$> (series :: Series m Int8)
instance Monad m => Serial m CUChar where
  series = coerce <$> (series :: Series m Word8)
instance Monad m => Serial m CShort where
  series = coerce <$> (series :: Series m Int16)
instance Monad m => Serial m CUShort where
  series = coerce <$> (series :: Series m Word16)
instance Monad m => Serial m CInt where
  series = coerce <$> (series :: Series m Int32)
instance Monad m => Serial m CUInt where
  series = coerce <$> (series :: Series m Word32)
instance Monad m => Serial m CLong where
  series = coerce <$> (series :: Series m Int64)
instance Monad m => Serial m CULong where
  series = coerce <$> (series :: Series m Word64)
instance Monad m => Serial m CPtrdiff where
  series = coerce <$> (series :: Series m Int64)
instance Monad m => Serial m CSize where
  series = coerce <$> (series :: Series m Word64)
instance Monad m => Serial m CWchar where
  series = coerce <$> (series :: Series m Int32)
instance Monad m => Serial m CSigAtomic where
  series = coerce <$> (series :: Series m Int32)
instance Monad m => Serial m CLLong where
  series = coerce <$> (series :: Series m Int64)
instance Monad m => Serial m CULLong where
  series = coerce <$> (series :: Series m Word64)
instance Monad m => Serial m CIntPtr where
  series = coerce <$> (series :: Series m Int64)
instance Monad m => Serial m CUIntPtr where
  series = coerce <$> (series :: Series m Word64)
instance Monad m => Serial m CIntMax where
  series = coerce <$> (series :: Series m Int64)
instance Monad m => Serial m CUIntMax where
  series = coerce <$> (series :: Series m Word64)
