{-# LANGUAGE CPP #-}
module Main where

#if __GLASGOW_HASKELL__ >= 802 && __GLASGOW_HASKELL__ < 810

import Test.DocTest (doctest)

main :: IO ()
main = doctest ["src"]

#else

-- Also disabled in cabal file.
-- TODO: fix doctest support
main :: IO ()
main = putStrLn "\nDoctests are not supported for older ghc version\n"

#endif
