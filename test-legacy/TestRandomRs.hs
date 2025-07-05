-- | Test for ticket #4218 (TestRandomRs):
-- https://ghc.haskell.org/trac/ghc/ticket/4218
--
-- Fixed together with ticket #8704
-- https://ghc.haskell.org/trac/ghc/ticket/8704
-- Commit 4695ffa366f659940369f05e419a4f2249c3a776
--
-- Used to fail with:
--
-- @
--   $ cabal build
--   $ cabal exec -- ghc -O1 -fforce-recomp -rtsopts -main-is TestRandomRs test-legacy/TestRandomRs.hs -o test-legacy/test
--   $ test-legacy/test +RTS -M1M -A1M -RTS
--   test: Heap exhausted;
--   test: Current maximum heap size is 1048576 bytes (1 MB).
--   test: Use `+RTS -M<size>' to increase it.
-- @
module TestRandomRs where

import Control.Monad (liftM)
import System.Random (getStdGen, randomRs)

-- Return the five-thousandth random number:
-- Should run in constant space (< 1Mb heap).
main :: IO ()
main = do
  n <- (last . take 5000 . randomRs (0, 1000000)) `liftM` getStdGen
  print (n :: Int)
