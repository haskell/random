{-# LANGUAGE ScopedTypeVariables #-}

module Spec.Run (runsEqual) where

import Data.Word (Word64)
import System.Random

runsEqual :: RandomGen g => g -> IO Bool
runsEqual g = do
    let (pureResult :: Word64) = runGenState_ g uniform
    (genResult :: Word64) <- runPrimGenIO_ g uniform
    return $ pureResult == genResult
