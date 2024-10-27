{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Main (main) where

import Control.Monad (replicateM, forM_)
import Control.Monad.ST (runST)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.Int
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Typeable
import Data.Void
import Data.Word
import Foreign.C.Types
import GHC.Generics
import GHC.Exts (fromList)
import Numeric.Natural (Natural)
import System.Random.Stateful
import System.Random.Internal (newMutableByteArray, freezeMutableByteArray, writeWord8)
import Test.SmallCheck.Series as SC
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck as SC
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid ((<>))
#endif

import qualified Spec.Range as Range
import qualified Spec.Run as Run
import qualified Spec.Seed as Seed
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
    , fillMutableByteArraySpec
    , SC.testProperty "uniformRangeWithinExcludedF" $ seeded Range.uniformRangeWithinExcludedF
    , SC.testProperty "uniformRangeWithinExcludedD" $ seeded Range.uniformRangeWithinExcludedD
    , randomSpec (Proxy :: Proxy (CFloat, CDouble))
    , randomSpec (Proxy :: Proxy (Int8, Int16, Int32))
    , randomSpec (Proxy :: Proxy (Int8, Int16, Int32, Int64))
    , randomSpec (Proxy :: Proxy (Word8, Word16, Word32, Word64, Word))
    , randomSpec (Proxy :: Proxy (Int8, Word8, Word16, Word32, Word64, Word))
    , randomSpec (Proxy :: Proxy (Int8, Int16, Word8, Word16, Word32, Word64, Word))
    , uniformSpec (Proxy :: Proxy (Int, Bool))
    , uniformSpec (Proxy :: Proxy (Int8, Int16, Int32))
    , uniformSpec (Proxy :: Proxy (Int8, Int16, Int32, Int64))
    , uniformSpec (Proxy :: Proxy (Word8, Word16, Word32, Word64, Word))
    , uniformSpec (Proxy :: Proxy (Int8, Word8, Word16, Word32, Word64, Word))
    , uniformSpec (Proxy :: Proxy (Int8, Int16, Word8, Word16, Word32, Word64, Word))
    , Stateful.statefulGenSpec
    , Seed.spec
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

showType :: forall t . Typeable t => Proxy t -> String
showType px = show (typeRep px)

byteStringSpec :: TestTree
byteStringSpec =
  testGroup
    "ByteString"
    [ SC.testProperty "genShortByteString" $
      seededWithLen $ \n g -> SBS.length (fst (genShortByteString n g)) == n
    , SC.testProperty "genByteString" $
      seededWithLen $ \n g ->
        SBS.toShort (fst (genByteString n g)) == fst (genShortByteString n g)
    , testCase "genByteString/ShortByteString consistency" $ do
        let g = mkStdGen 2021
            bs = [78,232,117,189,13,237,63,84,228,82,19,36,191,5,128,192] :: [Word8]
        forM_ [0 .. length bs - 1] $ \ n -> do
          xs <- SBS.unpack <$> runStateGenT_ g (uniformShortByteStringM n)
          xs @?= take n bs
          ys <- BS.unpack <$> runStateGenT_ g (uniformByteStringM n)
          ys @?= xs
    ]

fillMutableByteArraySpec :: TestTree
fillMutableByteArraySpec =
  testGroup
    "MutableByteArray"
    [ SC.testProperty "Same as uniformByteArray" $
        forAll $ \isPinned -> seededWithLen $ \n g ->
          let baFilled = runST $ do
                mba <- newMutableByteArray n
                g' <- uniformFillMutableByteArray mba 0 n g
                ba <- freezeMutableByteArray mba
                pure (ba, g')
          in baFilled == uniformByteArray isPinned n g
    , SC.testProperty "Safe uniformFillMutableByteArray" $
        forAll $ \isPinned offset count -> seededWithLen $ \sz g ->
          let (baFilled, gf) = runST $ do
                mba <- newMutableByteArray sz
                forM_ [0 .. sz - 1] (\i -> writeWord8 mba i 0)
                g' <- uniformFillMutableByteArray mba offset count g
                ba <- freezeMutableByteArray mba
                pure (ba, g')
              (baGen, gu) = uniformByteArray isPinned count' g
              offset' = min sz (max 0 offset)
              count' = min (sz - offset') (max 0 count)
              prefix = replicate offset' 0
              suffix = replicate (sz - (count' + offset')) 0
          in gf == gu && baFilled == fromList prefix <> baGen <> fromList suffix
    ]

rangeSpec ::
     forall a.
     (SC.Serial IO a, Typeable a, Ord a, UniformRange a, Show a)
  => Proxy a -> TestTree
rangeSpec px =
  testGroup ("Range " ++ showType px)
  [ SC.testProperty "uniformR" $ seeded $ Range.uniformRangeWithin px
  ]

integralSpec ::
     forall a.
     (SC.Serial IO a, Typeable a, Ord a, UniformRange a, Show a)
  => Proxy a -> TestTree
integralSpec px =
  testGroup (showType px)
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
  testGroup (showType px)
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

randomSpec ::
     forall a.
     (Typeable a, Eq a, Random a, Show a)
  => Proxy a -> TestTree
randomSpec px =
  testGroup
    ("Random " ++ showType px)
    [ SC.testProperty "randoms" $
      seededWithLen $ \len g ->
        take len (randoms g :: [a]) == runStateGen_ g (replicateM len . randomM)
    , SC.testProperty "randomRs" $
      seededWithLen $ \len g ->
        case random g of
          (range, g') ->
             take len (randomRs range g' :: [a]) ==
               runStateGen_ g' (replicateM len . randomRM range)
    ]

uniformSpec ::
     forall a.
     (Typeable a, Eq a, Random a, Uniform a, UniformRange a, Show a)
  => Proxy a -> TestTree
uniformSpec px =
  testGroup
    ("Uniform " ++ showType px)
    [ SC.testProperty "uniformList" $
      seededWithLen $ \len g ->
        take len (randoms g :: [a]) == fst (uniformList len g)
    , SC.testProperty "uniformListR" $
      seededWithLen $ \len g ->
        case uniform g of
          (range, g') ->
            take len (randomRs range g' :: [a]) == fst (uniformListR len range g')
    , SC.testProperty "shuffleList" $
      seededWithLen $ \len g ->
        case uniformList len g of
          (xs, g') ->
            let xs' = zip [0 :: Int ..] (xs :: [a])
            in sortOn fst (fst (shuffleList xs' g')) == xs'
    , SC.testProperty "uniforms" $
      seededWithLen $ \len g ->
        take len (randoms g :: [a]) == take len (uniforms g)
    , SC.testProperty "uniformRs" $
      seededWithLen $ \len g ->
        case uniform g of
          (range, g') ->
            take len (randomRs range g' :: [a]) == take len (uniformRs range g')
    ]

runSpec :: TestTree
runSpec = testGroup "runStateGen_ and runPrimGenIO_"
    [ SC.testProperty "equal outputs" $ seeded $ \g -> monadic $ Run.runsEqual g ]

-- | Create a StdGen instance from an Int and pass it to the given function.
seeded :: (StdGen -> a) -> Int -> a
seeded f = f . mkStdGen

-- | Same as `seeded`, but also produces a length in range 0-65535 suitable for generating
-- lists and such
seededWithLen :: (Int -> StdGen -> a) -> Word16 -> Int -> a
seededWithLen f w16 = seeded (f (fromIntegral w16))

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

instance SeedGen ConstGen where
  type SeedSize ConstGen = 8
  seedGen64 (w :| _) = ConstGen w
  unseedGen64 (ConstGen w) = pure w

instance RandomGen ConstGen where
  genWord64 g@(ConstGen c) = (c, g)
instance SplitGen ConstGen where
  splitGen g = (g, g)

data Colors = Red | Green | Blue | Purple | Yellow | Black | White | Orange
  deriving (Eq, Ord, Show, Generic, Enum, Bounded)
instance Monad m => Serial m Colors

instance Uniform Colors where
  uniformM = uniformEnumM

instance UniformRange Colors where
  uniformRM = uniformEnumRM
  isInRange (lo, hi) x = isInRange (fromEnum lo, fromEnum hi) (fromEnum x)
