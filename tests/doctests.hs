module Main where

import Build_doctests (flags, pkgs, module_sources)
import Data.Foldable (traverse_)
import Test.DocTest (doctest)

main :: IO ()
main = do
    traverse_ putStrLn args
    doctest args
  where
    -- '-fobject-code' is required to get the doctests to build without
    -- tripping over the Cmm bits.
    args = ["-fobject-code"] ++ flags ++ pkgs ++ module_sources
