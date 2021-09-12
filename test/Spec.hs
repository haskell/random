{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.Int
import Data.Typeable
import Data.Void
import Data.Word
import Foreign.C.Types
import GHC.Generics
import Numeric.Natural (Natural)
import System.Random
import System.Random.Stateful
import Test.SmallCheck.Series as SC
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck as SC

import qualified Spec.Range as Range
import qualified Spec.Run as Run
import qualified Spec.Stateful as Stateful

main :: IO ()
main =
  defaultMain $
  testGroup
    "Spec"
    [ floatingSpec (Proxy :: Proxy Double)
    , floatingSpec (Proxy :: Proxy Float)
    , floatingSpec (Proxy :: Proxy CDouble)
    , floatingSpec (Proxy :: Proxy CFloat)
    , integralSpec (Proxy :: Proxy Word8)
    , integralSpec (Proxy :: Proxy Word16)
    , integralSpec (Proxy :: Proxy Word32)
    , integralSpec (Proxy :: Proxy Word64)
    , integralSpec (Proxy :: Proxy Word)
    , integralSpec (Proxy :: Proxy Int8)
    , integralSpec (Proxy :: Proxy Int16)
    , integralSpec (Proxy :: Proxy Int32)
    , integralSpec (Proxy :: Proxy Int64)
    , integralSpec (Proxy :: Proxy Int)
    , integralSpec (Proxy :: Proxy Char)
    , integralSpec (Proxy :: Proxy Bool)
#if __GLASGOW_HASKELL__ >= 802
    , integralSpec (Proxy :: Proxy CBool)
#endif
    , integralSpec (Proxy :: Proxy CChar)
    , integralSpec (Proxy :: Proxy CSChar)
    , integralSpec (Proxy :: Proxy CUChar)
    , integralSpec (Proxy :: Proxy CShort)
    , integralSpec (Proxy :: Proxy CUShort)
    , integralSpec (Proxy :: Proxy CInt)
    , integralSpec (Proxy :: Proxy CUInt)
    , integralSpec (Proxy :: Proxy CLong)
    , integralSpec (Proxy :: Proxy CULong)
    , integralSpec (Proxy :: Proxy CPtrdiff)
    , integralSpec (Proxy :: Proxy CSize)
    , integralSpec (Proxy :: Proxy CWchar)
    , integralSpec (Proxy :: Proxy CSigAtomic)
    , integralSpec (Proxy :: Proxy CLLong)
    , integralSpec (Proxy :: Proxy CULLong)
    , integralSpec (Proxy :: Proxy CIntPtr)
    , integralSpec (Proxy :: Proxy CUIntPtr)
    , integralSpec (Proxy :: Proxy CIntMax)
    , integralSpec (Proxy :: Proxy CUIntMax)
    , integralSpec (Proxy :: Proxy Integer)
    , integralSpec (Proxy :: Proxy Natural)
    , enumSpec     (Proxy :: Proxy Colors)
    , enumSpec     (Proxy :: Proxy (Int, Int))
    , enumSpec     (Proxy :: Proxy (Bool, Bool, Bool))
    , enumSpec     (Proxy :: Proxy ((), Int, Bool, Word))
    , runSpec
    , floatTests
    , byteStringSpec
    , SC.testProperty "uniformRangeWithinExcludedF" $ seeded Range.uniformRangeWithinExcludedF
    , SC.testProperty "uniformRangeWithinExcludedD" $ seeded Range.uniformRangeWithinExcludedD
    , Stateful.statefulSpec
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

showsType :: forall t . Typeable t => Proxy t -> ShowS
showsType px = showsTypeRep (typeRep px)

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
    , testCase "genByteString/ShortByteString consistency" $ do
        let g = mkStdGen 2021
            bs = [78,232,117,189,13,237,63,84,228,82,19,36,191,5,128,192] :: [Word8]
        forM_ [0 .. length bs - 1] $ \ n -> do
          xs <- SBS.unpack <$> runStateGenT_ g (uniformShortByteString n)
          xs @?= take n bs
          ys <- BS.unpack <$> runStateGenT_ g (uniformByteStringM n)
          ys @?= xs
    ]


rangeSpec ::
     forall a.
     (SC.Serial IO a, Typeable a, Ord a, UniformRange a, Show a)
  => Proxy a -> TestTree
rangeSpec px =
  testGroup ("Range (" ++ showsType px ")")
  [ SC.testProperty "uniformR" $ seeded $ Range.uniformRangeWithin px
  ]

integralSpec ::
     forall a.
     (SC.Serial IO a, Typeable a, Ord a, UniformRange a, Show a)
  => Proxy a -> TestTree
integralSpec px =
  testGroup ("(" ++ showsType px ")")
  [ SC.testProperty "symmetric" $ seeded $ Range.symmetric px
  , SC.testProperty "bounded" $ seeded $ Range.bounded px
  , SC.testProperty "singleton" $ seeded $ Range.singleton px
  , rangeSpec px
  -- TODO: Add more tests
  ]

enumSpec ::
     forall a.
     (SC.Serial IO a, Typeable a, Ord a, UniformRange a, Show a)
  => Proxy a -> TestTree
enumSpec = integralSpec

floatingSpec ::
     forall a.
     (SC.Serial IO a, Typeable a, Num a, Ord a, Random a, UniformRange a, Read a, Show a)
  => Proxy a -> TestTree
floatingSpec px =
  testGroup ("(" ++ showsType px ")")
  [ SC.testProperty "uniformR" $ seeded $ Range.uniformRangeWithin px
  , testCase "r = +inf, x = 0" $ positiveInf @?= fst (uniformR (0, positiveInf) (ConstGen 0))
  , testCase "r = +inf, x = 1" $ positiveInf @?= fst (uniformR (0, positiveInf) (ConstGen 1))
  , testCase "l = -inf, x = 0" $ negativeInf @?= fst (uniformR (negativeInf, 0) (ConstGen 0))
  , testCase "l = -inf, x = 1" $ negativeInf @?= fst (uniformR (negativeInf, 0) (ConstGen 1))
  -- TODO: Add more tests
  ]
  where
    positiveInf, negativeInf :: a
    positiveInf = read "Infinity"
    negativeInf = read "-Infinity"

runSpec :: TestTree
runSpec = testGroup "runStateGen_ and runPrimGenIO_"
    [ SC.testProperty "equal outputs" $ seeded $ \g -> monadic $ Run.runsEqual g ]

-- | Create a StdGen instance from an Int and pass it to the given function.
seeded :: (StdGen -> a) -> Int -> a
seeded f = f . mkStdGen

data MyBool = MyTrue | MyFalse
  deriving (Eq, Ord, Show, Generic, Finite, Uniform)
instance Monad m => Serial m MyBool

data MyAction = Code (Maybe MyBool) | Never Void | Eat (Bool, Bool) | Sleep ()
  deriving (Eq, Ord, Show, Generic, Finite)
instance Monad m => Serial m MyAction
instance Uniform MyAction

data Foo
  = Quux Char
  | Bar   Int   | Baz Word
  | Bar8  Int8  | Baz8 Word8
  | Bar16 Int16 | Baz16 Word16
  | Bar32 Int32 | Baz32 Word32
  | Bar64 Int64 | Baz64 Word64
  | Final ()
  deriving (Eq, Ord, Show, Generic, Finite, Uniform)
instance Monad m => Serial m Foo

newtype ConstGen = ConstGen Word64

instance RandomGen ConstGen where
  genWord64 g@(ConstGen c) = (c, g)
  split g = (g, g)

data Colors = Red | Green | Blue | Purple | Yellow | Black | White | Orange
  deriving (Eq, Ord, Show, Generic, Enum, Bounded)
instance Monad m => Serial m Colors

instance Uniform Colors where
  uniformM = uniformEnumM

instance UniformRange Colors where
  uniformRM = uniformEnumRM
  isInRange (lo, hi) x = isInRange (fromEnum lo, fromEnum hi) (fromEnum x)

