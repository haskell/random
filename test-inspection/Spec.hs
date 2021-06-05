{-# LANGUAGE CPP #-}
module Main (main) where
#if __GLASGOW_HASKELL__ >= 800

import qualified Spec.Inspection as Inspection
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "InspectionSpec"
      [ Inspection.inspectionTests
      ]

#else

main :: IO ()
main = putStrLn "\nInspection testing is not supported for pre ghc-8.0 versions\n"

#endif


