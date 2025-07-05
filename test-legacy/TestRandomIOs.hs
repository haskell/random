-- | Test for ticket #4218 (TestRandomIOs):
-- https://ghc.haskell.org/trac/ghc/ticket/4218
--
-- Used to fail with:
--
-- @
--   $ cabal build
--   $ cabal exec -- ghc -O1 -fforce-recomp -rtsopts -main-is TestRandomIOs test-legacy/TestRandomIOs.hs -o test-legacy/test
--   $ test-legacy/test +RTS -M1M -A1M -RTS
--   test: Heap exhausted;
--   test: Current maximum heap size is 1048576 bytes (1 MB).
--   test: Use `+RTS -M<size>' to increase it.
-- @
module TestRandomIOs where

import Control.Monad (replicateM)
import System.Random (randomIO)

-- Build a list of 5000 random ints in memory (IO Monad is strict), and print
-- the last one.
-- Should use less than 1Mb of heap space, or we are generating a list of
-- unevaluated thunks.
main :: IO ()
main = do
  rs <- replicateM 5000 randomIO :: IO [Int]
  print $ last rs
