{-# LANGUAGE CPP #-}
module Main where

#if __GLASGOW_HASKELL__ >= 802

import Test.DocTest (doctest)

main :: IO ()
main =
  doctest
    [ "-XBangPatterns"
    , "-XCPP"
    , "-XDefaultSignatures"
    , "-XFlexibleContexts"
    , "-XFlexibleInstances"
    , "-XMultiParamTypeClasses"
    , "-XGHCForeignImportPrim"
    , "-XGeneralizedNewtypeDeriving"
    , "-XMagicHash"
    , "-XRankNTypes"
    , "-XScopedTypeVariables"
    , "-XTrustworthy"
    , "-XTypeOperators"
    , "-XUnboxedTuples"
    , "-XUndecidableInstances"
    , "-XUnliftedFFITypes"
    , "-XTypeFamilyDependencies"
    , "-XHaskell2010"
    , "src"
    ]

#else

-- Also disabled in cabal file.
-- TODO: fix doctest support
main :: IO ()
main = putStrLn "\nDoctests are not supported for older ghc version\n"

#endif
