{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE Trustworthy #-}
-- |
-- Module      :  System.Random.Seed
-- Copyright   :  (c) Alexey Kuleshevich 2024
-- License     :  BSD-style (see the file LICENSE in the 'random' repository)
-- Maintainer  :  libraries@haskell.org
--

module System.Random.Seed
  ( SeedGen(..)
  , -- ** Seed
    Seed
  , seedSize
  , mkSeed
  , unSeed
  , mkSeedFromByteString
  , unSeedToByteString
  , withSeedFile
  , nonEmptyToSeed
  , nonEmptyFromSeed
  ) where

import Control.Monad (unless)
import Control.Monad.IO.Class
import Control.Monad.ST
import Control.Monad.State.Strict (get, put, runStateT)
import Data.Array.Byte (ByteArray(..))
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS (fromShort, toShort)
import Data.Coerce
import Data.List.NonEmpty as NE (NonEmpty(..), nonEmpty, toList)
import Data.Typeable
import Data.Word
import GHC.TypeLits (Nat, KnownNat, natVal, type (<=))
import System.Random.Internal
import qualified System.Random.SplitMix as SM
import qualified System.Random.SplitMix32 as SM32


-- | Interface for coverting a pure pseudo-random number generator to and from non-empty
-- sequence of bytes/words. Seeds are stored in Little-Endian order regardless of the platform
-- it is being used on, which provides inter-platform compatibility, while providing
-- optimal performance for most common platforms.
--
-- Conversion to and from a `Seed` serves as a building block for implementing
-- serialization for any pure or frozen pseudo-random number generator
--
-- @since 1.3.0
class (KnownNat (SeedSize g), 1 <= SeedSize g, Typeable g) => SeedGen g where
  type SeedSize g :: Nat
  {-# MINIMAL (seedGen, unseedGen)|(seedGen64, unseedGen64) #-}

  -- | Convert from a binary representation to a pseudo-random number generator
  --
  -- @since 1.3.0
  seedGen :: Seed g -> g
  seedGen = seedGen64 . nonEmptyFromSeed

  -- |
  --
  -- @since 1.3.0
  unseedGen :: g -> Seed g
  unseedGen = nonEmptyToSeed . unseedGen64

  -- |
  --
  -- @since 1.3.0
  seedGen64 :: NonEmpty Word64 -> g
  seedGen64 = seedGen . nonEmptyToSeed

  -- |
  --
  -- @since 1.3.0
  unseedGen64 :: g -> NonEmpty Word64
  unseedGen64 = nonEmptyFromSeed . unseedGen

instance SeedGen StdGen where
  type SeedSize StdGen = SeedSize SM.SMGen
  seedGen  = seedGen . coerce
  unseedGen = coerce . unseedGen

instance SeedGen g => SeedGen (StateGen g) where
  type SeedSize (StateGen g) = SeedSize g
  seedGen = seedGen . coerce
  unseedGen = coerce . unseedGen

instance SeedGen SM.SMGen where
  type SeedSize SM.SMGen = 16
  seedGen (Seed ba) =
    SM.seedSMGen (indexWord64LE ba 0) (indexWord64LE ba 8)
  unseedGen g =
    case SM.unseedSMGen g of
      (seed, gamma) -> Seed $ runST $ do
        mba <- newMutableByteArray 16
        writeWord64LE mba 0 seed
        writeWord64LE mba 8 gamma
        freezeMutableByteArray mba

instance SeedGen SM32.SMGen where
  type SeedSize SM32.SMGen = 8
  seedGen (Seed ba) =
    let x = indexWord64LE ba 0
        seed, gamma :: Word32
        seed = fromIntegral (shiftR x 32)
        gamma = fromIntegral x
    in SM32.seedSMGen seed gamma
  unseedGen g =
    let seed, gamma :: Word32
        (seed, gamma) = SM32.unseedSMGen g
    in Seed $ runST $ do
        mba <- newMutableByteArray 8
        let w64 :: Word64
            w64 = shiftL (fromIntegral seed) 32 .|. fromIntegral gamma
        writeWord64LE mba 0 w64
        freezeMutableByteArray mba


-- | Get the expected size in bytes of the `Seed`
--
-- @since 1.3.0
seedSize :: forall g. SeedGen g => Int
seedSize = fromIntegral $ natVal (Proxy :: Proxy (SeedSize g))

-- | Construct a `Seed` from a `ByteArray` of expected length. Whenever `ByteArray` does
-- not match the `SeedSize` specified by the pseudo-random generator, this function will
-- return `Nothing`
--
-- @since 1.3.0
mkSeed :: forall g m. (SeedGen g, MonadFail m) => ByteArray -> m (Seed g)
mkSeed ba = do
  unless (sizeOfByteArray ba == seedSize @g) $ do
    fail $ "Unexpected number of bytes: "
        <> show (sizeOfByteArray ba)
        <> ". Exactly "
        <> show (seedSize @g)
        <> " bytes is required by the "
        <> show (genTypeName @g)
  pure $ Seed ba


-- | This is a function that shows the name of the generator type, which is useful for
-- error reporting.
genTypeName :: forall g. SeedGen g => String
genTypeName = show (typeOf (Proxy @g))


-- | Just like `mkSeed`, but uses `ByteString` as argument. Results in a memcopy of the seed.
--
-- @since 1.3.0
mkSeedFromByteString :: (SeedGen g, MonadFail m) => BS.ByteString -> m (Seed g)
mkSeedFromByteString = mkSeed . shortByteStringToByteArray . SBS.toShort

-- | Unwrap the `Seed` and get the underlying `ByteArray`
--
-- @since 1.3.0
unSeed :: Seed g -> ByteArray
unSeed (Seed ba) = ba

-- | Just like `unSeed`, but produced a `ByteString`. Results in a memcopy of the seed.
--
-- @since 1.3.0
unSeedToByteString :: Seed g -> BS.ByteString
unSeedToByteString = SBS.fromShort . byteArrayToShortByteString . unSeed


-- | Read the seed from a file and use it for constructing a pseudo-random number
-- generator. After supplied action has been applied to the constructed generator, the
-- resulting generator will be converted back to a seed and written to the same file.
--
-- @since 1.3.0
withSeedFile :: (SeedGen g, MonadIO m) => FilePath -> (g -> m (a, g)) -> m a
withSeedFile fileName f = do
  bs <- liftIO $ BS.readFile fileName
  seed <- liftIO $ mkSeedFromByteString bs
  (res, gen) <- f $ seedGen seed
  liftIO $ BS.writeFile fileName $ unSeedToByteString $ unseedGen gen
  pure res


nonEmptyToSeed :: forall g. SeedGen g => NonEmpty Word64 -> Seed g
nonEmptyToSeed xs = Seed $ runST $ do
  let n = seedSize @g
  mba <- newMutableByteArray n
  _ <- flip runStateT (NE.toList xs) $ do
    defaultUnsafeFillMutableByteArrayT mba 0 n $ do
      get >>= \case
        [] -> pure 0
        w:ws -> w <$ put ws
  freezeMutableByteArray mba

nonEmptyFromSeed :: forall g. SeedGen g => Seed g -> NonEmpty Word64
nonEmptyFromSeed (Seed ba) =
  case nonEmpty $ reverse $ goWord64 0 [] of
    Just ne -> ne
    Nothing -> -- Seed is at least 1 byte in size, so it can't be empty
      error $ "Impossible: Seed must be at least: "
           ++ show (seedSize @g)
           ++ " bytes, but got "
           ++ show n
  where
    n = sizeOfByteArray ba
    n8 = 8 * (n `quot` 8)
    goWord64 i !acc
      | i < n8 = goWord64 (i + 8) (indexWord64LE ba i : acc)
      | i == n = acc
      | otherwise = indexByteSliceWord64LE ba i n : acc