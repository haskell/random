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
module System.Random.Array (
  -- * Helper array functionality
  ioToST,
  wordSizeInBits,

  -- ** MutableByteArray
  newMutableByteArray,
  newPinnedMutableByteArray,
  freezeMutableByteArray,
  writeWord8,
  writeWord64LE,
  writeByteSliceWord64LE,
  indexWord8,
  indexWord64LE,
  indexByteSliceWord64LE,
  sizeOfByteArray,
  shortByteStringToByteArray,
  byteArrayToShortByteString,
  getSizeOfMutableByteArray,
  shortByteStringToByteString,

  -- ** MutableArray
  Array (..),
  MutableArray (..),
  newMutableArray,
  freezeMutableArray,
  writeArray,
  shuffleListM,
  shuffleListST,
) where

import Control.Monad (when)
import Control.Monad.ST
import Control.Monad.Trans (MonadTrans, lift)
#if !defined(__MHS__)
import Data.Array.Byte (ByteArray (..), MutableByteArray (..))
import Data.Bits
import Data.ByteString.Short.Internal (ShortByteString (SBS))
import qualified Data.ByteString.Short.Internal as SBS (fromShort)
import Data.Word
import GHC.Exts
import GHC.IO (IO (..))
import GHC.ST (ST (..))
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

----------------
-- Byte Array --
----------------

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

indexWord8 ::
  ByteArray ->
  -- | Offset into immutable byte array in number of bytes
  Int ->
  Word8
indexWord8 (ByteArray ba#) (I# i#) =
  W8# (indexWord8Array# ba# i#)
{-# INLINE indexWord8 #-}

indexWord64LE ::
  ByteArray ->
  -- | Offset into immutable byte array in number of bytes
  Int ->
  Word64
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

-- On big endian machines we need to write one byte at a time for consistency with little
-- endian machines. Also for GHC versions prior to 8.6 we don't have primops that can
-- write with byte offset, eg. writeWord8ArrayAsWord64# and writeWord8ArrayAsWord32#, so we
-- also must fallback to writing one byte a time. Such fallback results in about 3 times
-- slow down, which is not the end of the world.
writeWord64LE ::
  MutableByteArray s ->
  -- | Offset into mutable byte array in number of bytes
  Int ->
  -- | 8 bytes that will be written into the supplied array
  Word64 ->
  ST s ()
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
#if __GLASGOW_HASKELL__ >=802
getSizeOfMutableByteArray (MutableByteArray mba#) =
  ST $ \s ->
    case getSizeofMutableByteArray# mba# s of
      (# s', n# #) -> (# s', I# n# #)
#else
getSizeOfMutableByteArray (MutableByteArray mba#) =
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
#if __GLASGOW_HASKELL__ < 802
shortByteStringToByteString ba = SBS.fromShort ba
#else
shortByteStringToByteString ba =
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

-----------------
-- Boxed Array --
-----------------

data Array a = Array (Array# a)

data MutableArray s a = MutableArray (MutableArray# s a)

newMutableArray :: Int -> a -> ST s (MutableArray s a)
newMutableArray (I# n#) a =
  ST $ \s# ->
    case newArray# n# a s# of
      (# s'#, ma# #) -> (# s'#, MutableArray ma# #)
{-# INLINE newMutableArray #-}

freezeMutableArray :: MutableArray s a -> ST s (Array a)
freezeMutableArray (MutableArray ma#) =
  ST $ \s# ->
    case unsafeFreezeArray# ma# s# of
      (# s'#, a# #) -> (# s'#, Array a# #)
{-# INLINE freezeMutableArray #-}

sizeOfMutableArray :: MutableArray s a -> Int
sizeOfMutableArray (MutableArray ma#) = I# (sizeofMutableArray# ma#)
{-# INLINE sizeOfMutableArray #-}

readArray :: MutableArray s a -> Int -> ST s a
readArray (MutableArray ma#) (I# i#) = ST (readArray# ma# i#)
{-# INLINE readArray #-}

writeArray :: MutableArray s a -> Int -> a -> ST s ()
writeArray (MutableArray ma#) (I# i#) a = st_ (writeArray# ma# i# a)
{-# INLINE writeArray #-}

#else /* !defined(__MHS__) */
import Data.Array.Byte
import Data.Bits
import Data.ByteString(ByteString)
import Data.ByteString.Short.Internal
import Data.Word
import GHC.Exts(unsafeIOToST)

wordSizeInBits :: Int
wordSizeInBits = _wordSize

ioToST :: IO a -> ST s a
ioToST = unsafeIOToST

-- newMutableArray

newPinnedMutableByteArray :: Int -> ST s (MutableByteArray s)
newPinnedMutableByteArray = newMutableByteArray

-- freezeMutableArray
-- writeWord8
-- indexWord8

indexWord64LE :: ByteArray -> Int -> Word64
indexWord64LE ba i = indexByteSliceWord64LE ba i (i+8)

writeWord64LE :: MutableByteArray s -> Int -> Word64 -> ST s ()
writeWord64LE ba i w = writeByteSliceWord64LE ba i (i+8) w

getSizeOfMutableByteArray :: MutableByteArray s -> ST s Int
getSizeOfMutableByteArray = sizeOfMutableByteArray

shortByteStringToByteArray :: ShortByteString -> ByteArray
shortByteStringToByteArray = byteStringToByteArray . fromShort
byteArrayToShortByteString :: ByteArray -> ShortByteString
byteArrayToShortByteString = toShort . byteArrayToByteString
shortByteStringToByteString :: ShortByteString -> ByteString
shortByteStringToByteString = fromShort

--------

data Array a
data MutableArray s a

newMutableArray :: Int -> a -> ST s (MutableArray s a)
newMutableArray = undefined

freezeMutableArray :: MutableArray s a -> ST s (Array a)
freezeMutableArray = undefined

sizeOfMutableArray :: MutableArray s a -> Int
sizeOfMutableArray = undefined

readArray :: MutableArray s a -> Int -> ST s a
readArray = undefined

writeArray :: MutableArray s a -> Int -> a -> ST s ()
writeArray = undefined

#endif /* !defined(__MHS__) */

writeByteSliceWord64LE :: MutableByteArray s -> Int -> Int -> Word64 -> ST s ()
writeByteSliceWord64LE mba fromByteIx toByteIx = go fromByteIx
  where
    go !i !z =
      when (i < toByteIx) $ do
        writeWord8 mba i (fromIntegral z :: Word8)
        go (i + 1) (z `shiftR` 8)
{-# INLINE writeByteSliceWord64LE #-}

indexByteSliceWord64LE ::
  ByteArray ->
  -- | Starting offset in number of bytes
  Int ->
  -- | Ending offset in number of bytes
  Int ->
  Word64
indexByteSliceWord64LE ba fromByteIx toByteIx = goWord8 fromByteIx 0
  where
    r = (toByteIx - fromByteIx) `rem` 8
    nPadBits = if r == 0 then 0 else 8 * (8 - r)
    goWord8 i !w64
      | i < toByteIx = goWord8 (i + 1) (shiftL w64 8 .|. fromIntegral (indexWord8 ba i))
      | otherwise = byteSwap64 (shiftL w64 nPadBits)
{-# INLINE indexByteSliceWord64LE #-}

swapArray :: MutableArray s a -> Int -> Int -> ST s ()
swapArray ma i j = do
  x <- readArray ma i
  y <- readArray ma j
  writeArray ma j x
  writeArray ma i y
{-# INLINE swapArray #-}

-- | Write contents of the list into the mutable array. Make sure that array is big
-- enough or segfault will happen.
fillMutableArrayFromList :: MutableArray s a -> [a] -> ST s ()
fillMutableArrayFromList ma = go 0
  where
    go _ [] = pure ()
    go i (x : xs) = writeArray ma i x >> go (i + 1) xs
{-# INLINE fillMutableArrayFromList #-}

readListFromMutableArray :: MutableArray s a -> ST s [a]
readListFromMutableArray ma = go (len - 1) []
  where
    len = sizeOfMutableArray ma
    go i !acc
      | i >= 0 = do
          x <- readArray ma i
          go (i - 1) (x : acc)
      | otherwise = pure acc
{-# INLINE readListFromMutableArray #-}

-- | Generate a list of indices that will be used for swapping elements in uniform shuffling:
--
-- @
-- [ (0, n - 1)
-- , (0, n - 2)
-- , (0, n - 3)
-- , ...
-- , (0, 3)
-- , (0, 2)
-- , (0, 1)
-- ]
-- @
genSwapIndices ::
  Monad m =>
  -- | Action that generates a Word in the supplied range.
  (Word -> m Word) ->
  -- | Number of index swaps to generate.
  Word ->
  m [Int]
genSwapIndices genWordR n = go 1 []
  where
    go i !acc
      | i >= n = pure acc
      | otherwise = do
          x <- genWordR i
          let !xi = fromIntegral x
          go (i + 1) (xi : acc)
{-# INLINE genSwapIndices #-}

-- | Implementation of mutable version of Fisher-Yates shuffle. Unfortunately, we cannot generally
-- interleave pseudo-random number generation and mutation of `ST` monad, therefore we have to
-- pre-generate all of the index swaps with `genSwapIndices` and store them in a list before we can
-- perform the actual swaps.
shuffleListM :: Monad m => (Word -> m Word) -> [a] -> m [a]
shuffleListM genWordR ls
  | len <= 1 = pure ls
  | otherwise = do
      swapIxs <- genSwapIndices genWordR (fromIntegral len)
      pure $ runST $ do
        ma <- newMutableArray len $ error "Impossible: shuffleListM"
        fillMutableArrayFromList ma ls

        -- Shuffle elements of the mutable array according to the uniformly generated index swap list
        let goSwap _ [] = pure ()
            goSwap i (j : js) = swapArray ma i j >> goSwap (i - 1) js
        goSwap (len - 1) swapIxs

        readListFromMutableArray ma
  where
    len = length ls
{-# INLINE shuffleListM #-}

-- | This is a ~x2-x3 more efficient version of `shuffleListM`. It is more efficient because it does
-- not need to pregenerate a list of indices and instead generates them on demand. Because of this the
-- result that will be produced will differ for the same generator, since the order in which index
-- swaps are generated is reversed.
--
-- Unfortunately, most stateful generator monads can't handle `MonadTrans`, so this version is only
-- used for implementing the pure shuffle.
shuffleListST :: (Monad (t (ST s)), MonadTrans t) => (Word -> t (ST s) Word) -> [a] -> t (ST s) [a]
shuffleListST genWordR ls
  | len <= 1 = pure ls
  | otherwise = do
      ma <- lift $ newMutableArray len $ error "Impossible: shuffleListST"
      lift $ fillMutableArrayFromList ma ls

      -- Shuffle elements of the mutable array according to the uniformly generated index swap
      let goSwap i =
            when (i > 0) $ do
              j <- genWordR $ (fromIntegral :: Int -> Word) i
              lift $ swapArray ma i ((fromIntegral :: Word -> Int) j)
              goSwap (i - 1)
      goSwap (len - 1)

      lift $ readListFromMutableArray ma
  where
    len = length ls
{-# INLINE shuffleListST #-}
