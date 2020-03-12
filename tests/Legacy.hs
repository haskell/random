module Main (main) where

import qualified Legacy.Random1283 as Random1283
import qualified Legacy.RangeTest as RangeTest
import qualified Legacy.T7936 as T7936
import qualified Legacy.TestRandomIOs as TestRandomIOs
-- FIXME Implement 'instance UniformRange Integer', then uncomment this import.
-- import qualified Legacy.TestRandomRs as TestRandomRs

main :: IO ()
main = do
    Random1283.main
    RangeTest.main
    T7936.main
    TestRandomIOs.main
    -- FIXME Implement 'instance UniformRange Integer', then uncomment TestRandomRs.
    -- TestRandomRs.main
