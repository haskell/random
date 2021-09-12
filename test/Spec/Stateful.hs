{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Spec.Stateful where

import Control.Concurrent.STM
import Control.Monad.ST
import Control.Monad.Trans.State.Strict
import Data.Proxy
import Data.Typeable
import System.Random.Stateful
import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.SmallCheck as SC

instance Monad m => Serial m StdGen where
  series = mkStdGen <$> series

instance (Monad m, Serial m g) => Serial m (AtomicGen g) where
  series = AtomicGen <$> series

instance (Monad m, Serial m g) => Serial m (IOGen g) where
  series = IOGen <$> series

instance (Monad m, Serial m g) => Serial m (STGen g) where
  series = STGen <$> series

instance (Monad m, Serial m g) => Serial m (TGen g) where
  series = TGen <$> series

instance (Monad m, Serial m g) => Serial m (StateGen g) where
  series = StateGen <$> series


matchRandomGenSpec ::
     forall b f m. (FrozenGen f m, Eq f, Show f, Eq b)
  => (forall a. m a -> IO a)
  -> (MutableGen f m -> m b)
  -> (StdGen -> (b, StdGen))
  -> (f -> StdGen)
  -> f
  -> Property IO
matchRandomGenSpec toIO genM gen toStdGen frozen =
  monadic $ do
    (x1, fg1) <- toIO $ withMutableGen frozen genM
    let (x2, g2) = gen $ toStdGen frozen
    pure $ x1 == x2 && toStdGen fg1 == g2

withMutableGenSpec ::
     forall f m. (FrozenGen f m, Eq f, Show f)
  => (forall a. m a -> IO a)
  -> f
  -> Property IO
withMutableGenSpec toIO frozen =
  forAll $ \n -> monadic $ do
    let gen = uniformListM n
    x :: ([Word], f) <- toIO $ withMutableGen frozen gen
    y <- toIO $ withMutableGen frozen gen
    pure $ x == y


statefulSpecFor ::
     forall f m. (FrozenGen f m, Eq f, Show f, Serial IO f, Typeable f)
  => (forall a. m a -> IO a)
  -> (f -> StdGen)
  -> TestTree
statefulSpecFor toIO toStdGen =
  testGroup
    (showsTypeRep (typeRep (Proxy :: Proxy f)) "")
    [ testProperty "withMutableGen" $
      forAll $ \(f :: f) -> withMutableGenSpec toIO f
    , testGroup
        "matchRandomGenSpec"
        [ testProperty "uniformWord8/genWord8" $
          forAll $ \(f :: f) ->
            matchRandomGenSpec toIO uniformWord8 genWord8 toStdGen f
        , testProperty "uniformWord16/genWord16" $
          forAll $ \(f :: f) ->
            matchRandomGenSpec toIO uniformWord16 genWord16 toStdGen f
        , testProperty "uniformWord32/genWord32" $
          forAll $ \(f :: f) ->
            matchRandomGenSpec toIO uniformWord32 genWord32 toStdGen f
        , testProperty "uniformWord64/genWord64" $
          forAll $ \(f :: f) ->
            matchRandomGenSpec toIO uniformWord64 genWord64 toStdGen f
        , testProperty "uniformWord32R/genWord32R" $
          forAll $ \(w32, f :: f) ->
            matchRandomGenSpec toIO (uniformWord32R w32) (genWord32R w32) toStdGen f
        , testProperty "uniformWord64R/genWord64R" $
          forAll $ \(w64, f :: f) ->
            matchRandomGenSpec toIO (uniformWord64R w64) (genWord64R w64) toStdGen f
        , testProperty "uniformShortByteString/genShortByteString" $
          forAll $ \(n', f :: f) ->
            let n = abs n' `mod` 1000 -- Ensure it is not too big
            in matchRandomGenSpec toIO (uniformShortByteString n) (genShortByteString n) toStdGen f
        ]
    ]


statefulSpec :: TestTree
statefulSpec =
  testGroup
    "Stateful"
    [ statefulSpecFor id unIOGen
    , statefulSpecFor id unAtomicGen
    , statefulSpecFor stToIO unSTGen
    , statefulSpecFor atomically unTGen
    , statefulSpecFor (`evalStateT` mkStdGen 0) unStateGen
    ]

