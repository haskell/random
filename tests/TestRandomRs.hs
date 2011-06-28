
-- Test from ticket #4218:
-- http://hackage.haskell.org/trac/ghc/ticket/4218

module Main where

import Control.Monad
import System.Random
import Data.List

force = foldr (\x r -> x `seq` (x:r)) []

-- Ten million random numbers:
blowsTheHeap :: IO Integer
blowsTheHeap = (last . take 10000000 . randomRs (0, 1000000)) `liftM` getStdGen

works :: IO Integer
works = (last . take 10000000 . force . randomRs (0, 1000000)) `liftM` getStdGen


main = 
 do n <- blowsTheHeap
    print n

