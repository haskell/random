module Spec.Run (runsEqual) where

import Data.Word (Word64)
import System.Random.Monad

runsEqual :: RandomGen g => g -> IO Bool
runsEqual g = do
  let pureResult = runStateGen_ g uniformM :: Word64
      stResult = runSTGen_ g uniformM
  ioResult <- runGenM_ (IOGen g) uniformM
  atomicResult <- runGenM_ (AtomicGen g) uniformM
  return $ all (pureResult ==) [stResult, ioResult, atomicResult]
