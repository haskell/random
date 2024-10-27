{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Spec.Stateful where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.ST
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
     forall f a sg m. (StatefulGen sg m, RandomGen f, Eq f, Show f, Eq a)
  => (forall g n. StatefulGen g n => g -> n a)
  -> (forall g. RandomGen g => g -> (a, g))
  -> (StdGen -> f)
  -> (f -> StdGen)
  -> (f -> (sg -> m a) -> IO (a, f))
  -> Property IO
matchRandomGenSpec genM gen fromStdGen toStdGen runStatefulGen =
  forAll $ \seed -> monadic $ do
    let stdGen = mkStdGen seed
        g = fromStdGen stdGen
        (x1, g1) = gen stdGen
        (x2, g2) = gen g
    (x3, g3) <- runStatefulGen g genM
    pure $ and [x1 == x2, x2 == x3, g1 == toStdGen g2, g1 == toStdGen g3, g2 == g3]

withMutableGenSpec ::
     forall f m. (ThawedGen f m, Eq f, Show f)
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
     forall f m. (ThawedGen f m, Eq f, Show f)
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

indepMutableGenSpec ::
     forall f m. (RandomGen f, ThawedGen f m, Eq f, Show f)
  => (forall a. m a -> IO a) -> [f] -> Property IO
indepMutableGenSpec toIO fgs =
  monadic $ toIO $ do
    (fgs ==) <$> (mapM freezeGen =<< mapM thawGen fgs)

immutableFrozenGenSpec ::
     forall f m. (RandomGen f, ThawedGen f m, Eq f, Show f)
  => (forall a. m a -> IO a) -> f -> Property IO
immutableFrozenGenSpec toIO frozen =
  forAll $ \n -> monadic $ toIO $ do
    let action = do
          mg <- thawGen frozen
          (,) <$> uniformWord8 mg <*> freezeGen mg
    x <- action
    xs <- replicateM n action
    pure $ all (x ==) xs

splitMutableGenSpec ::
     forall f m. (SplitGen f, ThawedGen f m, Eq f, Show f)
  => (forall a. m a -> IO a)
  -> f
  -> Property IO
splitMutableGenSpec toIO frozen =
  monadic $ toIO $ do
    (sfg1, fg1) <- withMutableGen frozen splitGenM
    (smg2, fg2) <- withMutableGen frozen splitMutableGenM
    sfg3 <- freezeGen smg2
    pure $ fg1 == fg2 && sfg1 == sfg3

thawedGenSpecFor ::
     forall f m. (SplitGen f, ThawedGen f m, Eq f, Show f, Serial IO f, Typeable f)
  => (forall a. m a -> IO a)
  -> Proxy f
  -> TestTree
thawedGenSpecFor toIO px =
  testGroup
    (showsTypeRep (typeRep px) "")
    [ testProperty "withMutableGen" $
      forAll $ \(f :: f) -> withMutableGenSpec toIO f
    , testProperty "overwriteGen" $
      forAll $ \(f :: f) -> overwriteMutableGenSpec toIO f
    , testProperty "independent mutable generators" $
      forAll $ \(fs :: [f]) -> indepMutableGenSpec toIO fs
    , testProperty "immutable frozen generators" $
      forAll $ \(f :: f) -> immutableFrozenGenSpec toIO f
    , testProperty "splitGen" $
      forAll $ \(f :: f) -> splitMutableGenSpec toIO f
    ]

frozenGenSpecFor ::
     forall f sg m. (RandomGen f, StatefulGen sg m, Eq f, Show f, Typeable f)
  => (StdGen -> f)
  -> (f -> StdGen)
  -> (forall a. f -> (sg -> m a) -> IO (a, f))
  -> TestTree
frozenGenSpecFor fromStdGen toStdGen runStatefulGen =
    testGroup (showsTypeRep (typeRep (Proxy :: Proxy f)) "")
    [ testGroup "matchRandomGenSpec"
      [ testProperty "uniformWord8/genWord8" $
          matchRandomGenSpec uniformWord8 genWord8 fromStdGen toStdGen runStatefulGen
      , testProperty "uniformWord16/genWord16" $
          matchRandomGenSpec uniformWord16 genWord16 fromStdGen toStdGen runStatefulGen
      , testProperty "uniformWord32/genWord32" $
          matchRandomGenSpec uniformWord32 genWord32 fromStdGen toStdGen runStatefulGen
      , testProperty "uniformWord64/genWord64" $
          matchRandomGenSpec uniformWord64 genWord64 fromStdGen toStdGen runStatefulGen
      , testProperty "uniformWord32R/genWord32R" $
        forAll $ \w32 ->
          matchRandomGenSpec (uniformWord32R w32) (genWord32R w32) fromStdGen toStdGen runStatefulGen
      , testProperty "uniformWord64R/genWord64R" $
        forAll $ \w64 ->
          matchRandomGenSpec (uniformWord64R w64) (genWord64R w64) fromStdGen toStdGen runStatefulGen
      , testProperty "uniformShortByteStringM/genShortByteString" $
        forAll $ \(NonNegative n') ->
          let n = n' `mod` 100000 -- Ensure it is not too big
          in matchRandomGenSpec
               (uniformShortByteStringM n)
               (genShortByteString n)
               fromStdGen
               toStdGen
               runStatefulGen
      , testProperty "uniformByteStringM/uniformByteString" $
        forAll $ \(NonNegative n') ->
          let n = n' `mod` 100000 -- Ensure it is not too big
          in matchRandomGenSpec
               (uniformByteStringM n)
               (uniformByteString n)
               fromStdGen
               toStdGen
               runStatefulGen
      , testProperty "uniformByteArrayM/genByteArray" $
        forAll $ \(NonNegative n', isPinned1 :: Bool, isPinned2 :: Bool) ->
          let n = n' `mod` 100000 -- Ensure it is not too big
          in matchRandomGenSpec
               (uniformByteArrayM isPinned1 n)
               (uniformByteArray isPinned2 n)
               fromStdGen
               toStdGen
               runStatefulGen
      ]
    ]


statefulGenSpec :: TestTree
statefulGenSpec =
  testGroup
    "StatefulGen"
    [ testGroup "ThawedGen"
        [ thawedGenSpecFor id (Proxy :: Proxy (IOGen StdGen))
        , thawedGenSpecFor id (Proxy :: Proxy (AtomicGen StdGen))
        , thawedGenSpecFor stToIO (Proxy :: Proxy (STGen StdGen))
        , thawedGenSpecFor atomically (Proxy :: Proxy (TGen StdGen))
        ]
    , testGroup "FrozenGen"
        [ frozenGenSpecFor StateGen unStateGen runStateGenT
        , frozenGenSpecFor IOGen unIOGen $ \g action -> do
            mg <- newIOGenM (unIOGen g)
            res <- action mg
            g' <- freezeGen mg
            pure (res, g')
        , frozenGenSpecFor AtomicGen unAtomicGen $ \g action -> do
            mg <- newAtomicGenM (unAtomicGen g)
            res <- action mg
            g' <- freezeGen mg
            pure (res, g')
        , frozenGenSpecFor STGen unSTGen $ \g action -> stToIO $ do
            mg <- newSTGenM (unSTGen g)
            res <- action mg
            g' <- freezeGen mg
            pure (res, g')
        , frozenGenSpecFor TGen unTGen $ \g action -> atomically $ do
            mg <- newTGenM (unTGen g)
            res <- action mg
            g' <- freezeGen mg
            pure (res, g')
        ]
    ]
