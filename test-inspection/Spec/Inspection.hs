{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-missing-signatures -O -dsuppress-all -dno-suppress-type-signatures -fplugin=Test.Tasty.Inspection.Plugin #-}

module Spec.Inspection (inspectionTests) where

import Data.Int
import Data.Void
import Data.Word
import GHC.Generics
import System.Random
import System.Random.Stateful
import Test.Tasty
import Test.Tasty.Inspection

uniform' :: Uniform a => (a, StdGen)
uniform' = uniform (mkStdGen 42)

uniform_Word8 = uniform' @Word8
uniform_Int8  = uniform' @Int8
uniform_Char  = uniform' @Char

data MyAction = Code (Maybe Bool) | Never Void | Eat (Bool, Bool) | Sleep ()
  deriving (Eq, Ord, Show, Generic, Finite)
instance Uniform MyAction

uniform_MyAction = uniform' @MyAction

uniformR' :: (Bounded a, UniformRange a) => (a, StdGen)
uniformR' = uniformR (minBound, maxBound) (mkStdGen 42)

uniformR_Word8 = uniformR' @Word8
uniformR_Int8  = uniformR' @Int8
uniformR_Char  = uniformR' @Char

uniformR_Double = uniformR (0 :: Double, 1) (mkStdGen 42)

inspectionTests :: TestTree
inspectionTests = testGroup "Inspection" $
  [ $(inspectObligations [(`doesNotUse` 'StateGenM), hasNoGenerics, hasNoTypeClasses] 'uniform_Word8)
  , $(inspectObligations [(`doesNotUse` 'StateGenM), hasNoGenerics, hasNoTypeClasses] 'uniform_Int8)
  , $(inspectObligations [(`doesNotUse` 'StateGenM), hasNoGenerics, hasNoTypeClasses] 'uniform_Char)
  , $(inspectObligations [(`doesNotUse` 'StateGenM), hasNoGenerics, hasNoTypeClasses] 'uniform_MyAction)

  , $(inspectObligations [(`doesNotUse` 'StateGenM), hasNoGenerics, hasNoTypeClasses] 'uniformR_Word8)
  , $(inspectObligations [(`doesNotUse` 'StateGenM), hasNoGenerics, hasNoTypeClasses] 'uniformR_Int8)
  , $(inspectObligations [(`doesNotUse` 'StateGenM), hasNoGenerics, hasNoTypeClasses] 'uniformR_Char)
  , $(inspectObligations [(`doesNotUse` 'StateGenM), hasNoGenerics, hasNoTypeClasses] 'uniformR_Double)
  ]
