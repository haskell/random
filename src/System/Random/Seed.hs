{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
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
  , withSeed
  , withSeedM
  , withSeedFile
  , seedGenTypeName
  , nonEmptyToSeed
  , nonEmptyFromSeed
  ) where

import Control.Monad (unless)
import qualified Control.Monad.Fail as F
import Control.Monad.IO.Class
import Control.Monad.ST
import Control.Monad.State.Strict (get, put, runStateT)
import Data.Array.Byte (ByteArray(..))
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short.Internal as SBS (fromShort, toShort)
import Data.Coerce
import Data.Functor.Identity (runIdentity)
import Data.List.NonEmpty as NE (NonEmpty(..), nonEmpty, toList)
import Data.Typeable
import Data.Word
import GHC.TypeLits (Nat, KnownNat, natVal, type (<=))
import System.Random.Internal
import qualified System.Random.SplitMix as SM
import qualified System.Random.SplitMix32 as SM32


-- | Interface for converting a pure pseudo-random number generator to and from non-empty
-- sequence of bytes. Seeds are stored in Little-Endian order regardless of the platform
-- it is being used on, which provides cross-platform compatibility, while providing
-- optimal performance for the most common platform type.
--
-- Conversion to and from a `Seed` serves as a building block for implementing
-- serialization for any pure or frozen pseudo-random number generator.
--
-- It is not trivial to implement platform independence. For this reason this type class
-- has two alternative ways of creating an instance for this class. The easiest way for
-- constructing a platform indepent seed is by converting the inner state of a generator
-- to and from a list of 64 bit words using `unseedGen64` and `seedGen64` respectively. In
-- that case cross-platform support will be handled automaticaly.
--
-- >>> :set -XDataKinds -XTypeFamilies
-- >>> import Data.Word (Word8, Word32)
-- >>> import Data.Bits ((.|.), shiftR, shiftL)
-- >>> import Data.List.NonEmpty (NonEmpty ((:|)))
-- >>> data FiveByteGen = FiveByteGen Word8 Word32 deriving Show
-- >>> :{
-- instance SeedGen FiveByteGen where
--   type SeedSize FiveByteGen = 5
--   seedGen64 (w64 :| _) =
--     FiveByteGen (fromIntegral (w64 `shiftR` 32)) (fromIntegral w64)
--   unseedGen64 (FiveByteGen x1 x4) =
--     let w64 = (fromIntegral x1 `shiftL` 32) .|. fromIntegral x4
--      in (w64 :| [])
-- :}
--
-- >>> FiveByteGen 0x80 0x01020304
-- FiveByteGen 128 16909060
-- >>> seedGen (unseedGen (FiveByteGen 0x80 0x01020304))
-- FiveByteGen 128 16909060
-- >>> unseedGen (FiveByteGen 0x80 0x01020304)
-- Seed [0x04, 0x03, 0x02, 0x01, 0x80]
-- >>> unseedGen64 (FiveByteGen 0x80 0x01020304)
-- 549772722948 :| []
--
-- However, when performance is of utmost importance or default handling of cross platform
-- independence is not sufficient, then an adventurous developer can try implementing
-- conversion into bytes directly with `unseedGen` and `seedGen`.
--
-- Properties that must hold:
--
-- @
-- > seedGen (unseedGen gen) == gen
-- @
--
-- @
-- > seedGen64 (unseedGen64 gen) == gen
-- @
--
-- Note, that there is no requirement for every `Seed` to roundtrip, eg. this proprty does
-- not even hold for `StdGen`:
--
-- >>> let seed = nonEmptyToSeed (0xab :| [0xff00]) :: Seed StdGen
-- >>> seed == unseedGen (seedGen seed)
-- False
--
-- @since 1.3.0
class (KnownNat (SeedSize g), 1 <= SeedSize g, Typeable g) => SeedGen g where
  -- | Number of bytes that is required for storing the full state of a pseudo-random
  -- number generator. It should be big enough to satisfy the roundtrip property:
  --
  -- @
  -- > seedGen (unseedGen gen) == gen
  -- @
  --
  type SeedSize g :: Nat
  {-# MINIMAL (seedGen, unseedGen)|(seedGen64, unseedGen64) #-}

  -- | Convert from a binary representation to a pseudo-random number generator
  --
  -- @since 1.3.0
  seedGen :: Seed g -> g
  seedGen = seedGen64 . nonEmptyFromSeed

  -- | Convert to a binary representation of a pseudo-random number generator
  --
  -- @since 1.3.0
  unseedGen :: g -> Seed g
  unseedGen = nonEmptyToSeed . unseedGen64

  -- | Construct pseudo-random number generator from a list of words. Whenever list does
  -- not have enough bytes to satisfy the `SeedSize` requirement, it will be padded with
  -- zeros. On the other hand when it has more than necessary, extra bytes will be dropped.
  --
  -- For example if `SeedSize` is set to 2, then only the lower 16 bits of the first
  -- element in the list will be used.
  --
  -- @since 1.3.0
  seedGen64 :: NonEmpty Word64 -> g
  seedGen64 = seedGen . nonEmptyToSeed

  -- | Convert pseudo-random number generator to a list of words
  --
  -- In case when `SeedSize` is not a multiple of 8, then the upper bits of the last word
  -- in the list will be set to zero.
  --
  -- @since 1.3.0
  unseedGen64 :: g -> NonEmpty Word64
  unseedGen64 = nonEmptyFromSeed . unseedGen

instance SeedGen StdGen where
  type SeedSize StdGen = SeedSize SM.SMGen
  seedGen = coerce (seedGen :: Seed SM.SMGen -> SM.SMGen)
  unseedGen = coerce (unseedGen :: SM.SMGen -> Seed SM.SMGen)

instance SeedGen g => SeedGen (StateGen g) where
  type SeedSize (StateGen g) = SeedSize g
  seedGen = coerce (seedGen :: Seed g -> g)
  unseedGen = coerce (unseedGen :: g -> Seed g)

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


-- | Get the expected size of the `Seed` in number bytes
--
-- @since 1.3.0
seedSize :: forall g. SeedGen g => Int
seedSize = fromIntegral $ natVal (Proxy :: Proxy (SeedSize g))

-- | Construct a `Seed` from a `ByteArray` of expected length. Whenever `ByteArray` does
-- not match the `SeedSize` specified by the pseudo-random generator, this function will
-- `F.fail`.
--
-- @since 1.3.0
mkSeed :: forall g m. (SeedGen g, F.MonadFail m) => ByteArray -> m (Seed g)
mkSeed ba = do
  unless (sizeOfByteArray ba == seedSize @g) $ do
    F.fail $ "Unexpected number of bytes: "
        ++ show (sizeOfByteArray ba)
        ++ ". Exactly "
        ++ show (seedSize @g)
        ++ " bytes is required by the "
        ++ show (seedGenTypeName @g)
  pure $ Seed ba

-- | Helper function that allows for operating directly on the `Seed`, while supplying a
-- function that uses the pseudo-random number generator that is constructed from that
-- `Seed`.
--
-- ====__Example__
--
-- >>> :set -XTypeApplications
-- >>> withSeed (nonEmptyToSeed (pure 2024) :: Seed StdGen) (random @Int)
-- (1039666877624726199,Seed [0xe9, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])
--
-- @since 1.3.0
withSeed :: SeedGen g => Seed g -> (g -> (a, g)) -> (a, Seed g)
withSeed seed f = runIdentity (withSeedM seed (pure . f))

-- | Same as `withSeed`, except it is useful with monadic computation and frozen generators.
--
-- See `System.Random.Stateful.withMutableSeedGen` for a helper that also handles seeds
-- for mutable pseduo-random number generators.
--
-- @since 1.3.0
withSeedM :: (SeedGen g, Functor f) => Seed g -> (g -> f (a, g)) -> f (a, Seed g)
withSeedM seed f = fmap unseedGen <$> f (seedGen seed)

-- | This is a function that shows the name of the generator type, which is useful for
-- error reporting.
--
-- @since 1.3.0
seedGenTypeName :: forall g. SeedGen g => String
seedGenTypeName = show (typeOf (Proxy @g))


-- | Just like `mkSeed`, but uses `ByteString` as argument. Results in a memcopy of the seed.
--
-- @since 1.3.0
mkSeedFromByteString :: (SeedGen g, F.MonadFail m) => BS.ByteString -> m (Seed g)
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
  (res, seed') <- withSeedM seed f
  liftIO $ BS.writeFile fileName $ unSeedToByteString seed'
  pure res

-- | Construct a seed from a list of 64-bit words. At most `SeedSize` many bytes will be used.
--
-- @since 1.3.0
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

-- | Convert a `Seed` to a list of 64bit words.
--
-- @since 1.3.0
nonEmptyFromSeed :: forall g. SeedGen g => Seed g -> NonEmpty Word64
nonEmptyFromSeed (Seed ba) =
  case nonEmpty $ reverse $ goWord64 0 [] of
    Just ne -> ne
    Nothing -> -- Seed is at least 1 byte in size, so it can't be empty
      error $ "Impossible: Seed for "
           ++ seedGenTypeName @g
           ++ " must be at least: "
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
