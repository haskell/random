-- | Test for ticket #7936:
-- https://ghc.haskell.org/trac/ghc/ticket/7936
--
-- Used to fail with:
--
-- > $ cabal test T7936 --test-options="+RTS -M1M -RTS"
--
-- T7936: Heap exhausted;
module T7936 where

import Control.Monad (replicateM_)
import System.Random (newStdGen)

main :: IO ()
main = replicateM_ 100000 newStdGen
