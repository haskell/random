
module Main where


import Data.Random.Utils
import Data.Word(Word32,Word64)
import Test.Tasty
import Test.Tasty.HUnit


main :: IO ()
main = defaultMain $ testGroup "zero Rep tests"
          [testGroup "single precision"
                    [testCase "signed zeros representation" $
                        assertBool "impossible, binary rep of +/-0 cannot agree"
                        (castFloatToWord32 (negate 0)  /= castFloatToWord32 0)

                    ,testCase  "zero word32 is +zero" $
                          assertBool "word32 zero is +zero, but this failure says nope"
                            (castWord32ToFloat (0 :: Word32) == (0 :: Float ))]
          ,testGroup "double  precision zero test "
                    [testCase "signed zeros" $
                      assertBool "zeros agree"
                      (castDoubleToWord64 (negate 0)  /= castDoubleToWord64 0)

                    ,testCase "zero word64 is +zero" $
                      assertBool "word64 zero is +zero"
                        (castWord64ToDouble (0 :: Word64) == (0 :: Double ))
                        ]]


