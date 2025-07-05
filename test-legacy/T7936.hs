-- | Test for ticket #7936:
-- https://ghc.haskell.org/trac/ghc/ticket/7936
--
-- Used to fail with:
--
-- @
--   $ cabal build
--   $ cabal exec -- ghc -O1 -fforce-recomp -rtsopts -main-is T7936 test-legacy/T7936.hs -o test-legacy/test
--   $ test-legacy/test +RTS -M1M -A1M -RTS
--   test: Heap exhausted;
--   test: Current maximum heap size is 1048576 bytes (1 MB).
--   test: Use `+RTS -M<size>' to increase it.
-- @
module T7936 where

import Control.Monad (replicateM_)
import System.Random (newStdGen)

main :: IO ()
main = replicateM_ 100000 newStdGen
