module Spec.Run (runsEqual) where

import Data.Word (Word64)
import System.Random

runsEqual :: RandomGen g => g -> IO Bool
runsEqual g = do
  let pureResult = runGenState_ g uniform :: Word64
      stResult = runSTGen_ g uniform
  ioResult <- runGenM_ (IOGen g) uniform
  atomicResult <- runGenM_ (AtomicGen g) uniform
  return $ all (pureResult ==) [stResult, ioResult, atomicResult]
