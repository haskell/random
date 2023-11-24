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
     forall b f m. (RandomGen f, FrozenGen f m, Eq f, Show f, Eq b)
  => (forall a. m a -> IO a)
  -> (MutableGen f m -> m b)
  -> (forall g. RandomGen g => g -> (b, g))
  -> (f -> StdGen)
  -> f
  -> Property IO
matchRandomGenSpec toIO genM gen toStdGen frozen =
  monadic $ do
    (x1, fg1) <- toIO $ withMutableGen frozen genM
    (x2, fg2) <- toIO $ withMutableGen frozen (`modifyGen` gen)
    let (x3, g3) = gen $ toStdGen frozen
    let (x4, g4) = toStdGen <$> gen frozen
    pure $ and [x1 == x2, x2 == x3, x3 == x4, fg1 == fg2, toStdGen fg1 == g3, g3 == g4]

withMutableGenSpec ::
     forall f m. (FrozenGen f m, Eq f, Show f)
  => (forall a. m a -> IO a)
  -> f
  -> Property IO
withMutableGenSpec toIO frozen =
  forAll $ \n -> monadic $ toIO $ do
    let action = uniformListM n
    x@(_, _) :: ([Word], f) <- withMutableGen frozen action
    y@(r, _) <- withMutableGen frozen action
    r' <- withMutableGen_ frozen action
    pure $ x == y && r == r'

overwriteMutableGenSpec ::
     forall f m. (FrozenGen f m, Eq f, Show f)
  => (forall a. m a -> IO a)
  -> f
  -> Property IO
overwriteMutableGenSpec toIO frozen =
  forAll $ \n -> monadic $ toIO $ do
    let action = uniformListM (abs n + 1) -- Non-empty
    ((r1, r2), frozen') :: ((String, String), f) <- withMutableGen frozen $ \mutable -> do
      r1 <- action mutable
      overwriteGen mutable frozen
      r2 <- action mutable
      modifyGen mutable (const ((), frozen))
      pure (r1, r2)
    pure $ r1 == r2 && frozen == frozen'

splitMutableGenSpec ::
     forall f m. (RandomGen f, FrozenGen f m, Eq f, Show f)
  => (forall a. m a -> IO a)
  -> f
  -> Property IO
splitMutableGenSpec toIO frozen =
  monadic $ toIO $ do
    (sfg1, fg1) <- withMutableGen frozen splitGen
    (smg2, fg2) <- withMutableGen frozen splitMutableGen
    sfg3 <- freezeGen smg2
    pure $ fg1 == fg2 && sfg1 == sfg3

statefulSpecFor ::
     forall f m. (RandomGen f, FrozenGen f m, Eq f, Show f, Serial IO f, Typeable f)
  => (forall a. m a -> IO a)
  -> (f -> StdGen)
  -> TestTree
statefulSpecFor toIO toStdGen =
  testGroup
    (showsTypeRep (typeRep (Proxy :: Proxy f)) "")
    [ testProperty "withMutableGen" $
      forAll $ \(f :: f) -> withMutableGenSpec toIO f
    , testProperty "overwriteGen" $
      forAll $ \(f :: f) -> overwriteMutableGenSpec toIO f
    , testProperty "splitGen" $
      forAll $ \(f :: f) -> splitMutableGenSpec toIO f
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

