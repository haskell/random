{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      :  System.Random.Array
-- Copyright   :  (c) Alexey Kuleshevich 2024
-- License     :  BSD-style (see the file LICENSE in the 'random' repository)
-- Maintainer  :  libraries@haskell.org
--
module System.Random.Array
  ( -- * Helper array functionality
    ioToST
  , wordSizeInBits
    -- ** MutableByteArray
  , newMutableByteArray
  , newPinnedMutableByteArray
  , freezeMutableByteArray
  , writeWord8
  , writeWord64LE
  , writeByteSliceWord64LE
  , indexWord8
  , indexWord64LE
  , indexByteSliceWord64LE
  , sizeOfByteArray
  , shortByteStringToByteArray
  , byteArrayToShortByteString
  , getSizeOfMutableByteArray
  , shortByteStringToByteString
  ) where

import Control.Monad (when)
import Control.Monad.ST
import Data.Array.Byte (ByteArray(..), MutableByteArray(..))
import Data.Bits
import Data.ByteString.Short.Internal (ShortByteString(SBS))
import qualified Data.ByteString.Short.Internal as SBS (fromShort)
import Data.Word
import GHC.Exts
import GHC.IO (IO(..))
import GHC.ST (ST(..))
import GHC.Word
#if __GLASGOW_HASKELL__ >= 802
import Data.ByteString.Internal (ByteString(PS))
import GHC.ForeignPtr
#else
import Data.ByteString (ByteString)
#endif

-- Needed for WORDS_BIGENDIAN
#include "MachDeps.h"

wordSizeInBits :: Int
wordSizeInBits = finiteBitSize (0 :: Word)

-- Architecture independent helpers:

sizeOfByteArray :: ByteArray -> Int
sizeOfByteArray (ByteArray ba#) = I# (sizeofByteArray# ba#)

st_ :: (State# s -> State# s) -> ST s ()
st_ m# = ST $ \s# -> (# m# s#, () #)
{-# INLINE st_ #-}

ioToST :: IO a -> ST RealWorld a
ioToST (IO m#) = ST m#
{-# INLINE ioToST #-}

newMutableByteArray :: Int -> ST s (MutableByteArray s)
newMutableByteArray (I# n#) =
  ST $ \s# ->
    case newByteArray# n# s# of
      (# s'#, mba# #) -> (# s'#, MutableByteArray mba# #)
{-# INLINE newMutableByteArray #-}

newPinnedMutableByteArray :: Int -> ST s (MutableByteArray s)
newPinnedMutableByteArray (I# n#) =
  ST $ \s# ->
    case newPinnedByteArray# n# s# of
      (# s'#, mba# #) -> (# s'#, MutableByteArray mba# #)
{-# INLINE newPinnedMutableByteArray #-}

freezeMutableByteArray :: MutableByteArray s -> ST s ByteArray
freezeMutableByteArray (MutableByteArray mba#) =
  ST $ \s# ->
    case unsafeFreezeByteArray# mba# s# of
      (# s'#, ba# #) -> (# s'#, ByteArray ba# #)

writeWord8 :: MutableByteArray s -> Int -> Word8 -> ST s ()
writeWord8 (MutableByteArray mba#) (I# i#) (W8# w#) = st_ (writeWord8Array# mba# i# w#)
{-# INLINE writeWord8 #-}

writeByteSliceWord64LE :: MutableByteArray s -> Int -> Int -> Word64 -> ST s ()
writeByteSliceWord64LE mba fromByteIx toByteIx = go fromByteIx
  where
    go !i !z =
      when (i < toByteIx) $ do
        writeWord8 mba i (fromIntegral z :: Word8)
        go (i + 1) (z `shiftR` 8)
{-# INLINE writeByteSliceWord64LE #-}

indexWord8 ::
     ByteArray
  -> Int -- ^ Offset into immutable byte array in number of bytes
  -> Word8
indexWord8 (ByteArray ba#) (I# i#) =
  W8# (indexWord8Array# ba# i#)
{-# INLINE indexWord8 #-}

indexWord64LE ::
     ByteArray
  -> Int -- ^ Offset into immutable byte array in number of bytes
  -> Word64
#if defined WORDS_BIGENDIAN || !(__GLASGOW_HASKELL__ >= 806)
indexWord64LE ba i = indexByteSliceWord64LE ba i (i + 8)
#else
indexWord64LE (ByteArray ba#) (I# i#)
  | wordSizeInBits == 64 = W64# (indexWord8ArrayAsWord64# ba# i#)
  | otherwise =
    let !w32l = W32# (indexWord8ArrayAsWord32# ba# i#)
        !w32u = W32# (indexWord8ArrayAsWord32# ba# (i# +# 4#))
    in (fromIntegral w32u `shiftL` 32) .|. fromIntegral w32l
#endif
{-# INLINE indexWord64LE #-}

indexByteSliceWord64LE ::
     ByteArray
  -> Int -- ^ Starting offset in number of bytes
  -> Int -- ^ Ending offset in number of bytes
  -> Word64
indexByteSliceWord64LE ba fromByteIx toByteIx = goWord8 fromByteIx 0
  where
    r = (toByteIx - fromByteIx) `rem` 8
    nPadBits = if r == 0 then 0 else 8 * (8 - r)
    goWord8 i !w64
      | i < toByteIx = goWord8 (i + 1) (shiftL w64 8 .|. fromIntegral (indexWord8 ba i))
      | otherwise = byteSwap64 (shiftL w64 nPadBits)
{-# INLINE indexByteSliceWord64LE #-}

-- On big endian machines we need to write one byte at a time for consistency with little
-- endian machines. Also for GHC versions prior to 8.6 we don't have primops that can
-- write with byte offset, eg. writeWord8ArrayAsWord64# and writeWord8ArrayAsWord32#, so we
-- also must fallback to writing one byte a time. Such fallback results in about 3 times
-- slow down, which is not the end of the world.
writeWord64LE ::
     MutableByteArray s
  -> Int -- ^ Offset into mutable byte array in number of bytes
  -> Word64 -- ^ 8 bytes that will be written into the supplied array
  -> ST s ()
#if defined WORDS_BIGENDIAN || !(__GLASGOW_HASKELL__ >= 806)
writeWord64LE mba i w64 =
  writeByteSliceWord64LE mba i (i + 8) w64
#else
writeWord64LE (MutableByteArray mba#) (I# i#) w64@(W64# w64#)
  | wordSizeInBits == 64 = st_ (writeWord8ArrayAsWord64# mba# i# w64#)
  | otherwise = do
    let !(W32# w32l#) = fromIntegral w64
        !(W32# w32u#) = fromIntegral (w64 `shiftR` 32)
    st_ (writeWord8ArrayAsWord32# mba# i# w32l#)
    st_ (writeWord8ArrayAsWord32# mba# (i# +# 4#) w32u#)
#endif
{-# INLINE writeWord64LE #-}

getSizeOfMutableByteArray :: MutableByteArray s -> ST s Int
getSizeOfMutableByteArray (MutableByteArray mba#) =
#if __GLASGOW_HASKELL__ >=802
  ST $ \s ->
    case getSizeofMutableByteArray# mba# s of
      (# s', n# #) -> (# s', I# n# #)
#else
  pure $! I# (sizeofMutableByteArray# mba#)
#endif
{-# INLINE getSizeOfMutableByteArray #-}

shortByteStringToByteArray :: ShortByteString -> ByteArray
shortByteStringToByteArray (SBS ba#) = ByteArray ba#
{-# INLINE shortByteStringToByteArray #-}

byteArrayToShortByteString :: ByteArray -> ShortByteString
byteArrayToShortByteString (ByteArray ba#) = SBS ba#
{-# INLINE byteArrayToShortByteString #-}

-- | Convert a ShortByteString to ByteString by casting, whenever memory is pinned,
-- otherwise make a copy into a new pinned ByteString
shortByteStringToByteString :: ShortByteString -> ByteString
shortByteStringToByteString ba =
#if __GLASGOW_HASKELL__ < 802
  SBS.fromShort ba
#else
  let !(SBS ba#) = ba in
  if isTrue# (isByteArrayPinned# ba#)
    then pinnedByteArrayToByteString ba#
    else SBS.fromShort ba
{-# INLINE shortByteStringToByteString #-}

pinnedByteArrayToByteString :: ByteArray# -> ByteString
pinnedByteArrayToByteString ba# =
  PS (pinnedByteArrayToForeignPtr ba#) 0 (I# (sizeofByteArray# ba#))
{-# INLINE pinnedByteArrayToByteString #-}

pinnedByteArrayToForeignPtr :: ByteArray# -> ForeignPtr a
pinnedByteArrayToForeignPtr ba# =
  ForeignPtr (byteArrayContents# ba#) (PlainPtr (unsafeCoerce# ba#))
{-# INLINE pinnedByteArrayToForeignPtr #-}
#endif
