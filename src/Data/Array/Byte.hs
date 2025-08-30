module Data.Array.Byte where
import Control.Monad.ST(ST)
import Control.Monad.Trans
import Data.Word
import Data.ByteString
import Data.ByteString.Short.Internal
import GHC.Exts

data ByteArray
data MutableByteArray s

instance Eq ByteArray
instance Ord ByteArray
instance Show ByteArray

ioToST :: IO a -> ST RealWorld a
ioToST = undefined

wordSizeInBits :: Int
wordSizeInBits = _wordSize

newMutableByteArray :: Int -> ST s (MutableByteArray s)
newMutableByteArray = undefined

newPinnedMutableByteArray :: Int -> ST s (MutableByteArray s)
newPinnedMutableByteArray = undefined

freezeMutableByteArray :: MutableByteArray s -> ST s ByteArray
freezeMutableByteArray = undefined

writeWord8 :: MutableByteArray s -> Int -> Word8 -> ST s ()
writeWord8 = undefined

writeWord64LE :: MutableByteArray s -> Int -> Word64 -> ST s ()
writeWord64LE = undefined

writeByteSliceWord64LE :: MutableByteArray s -> Int -> Int -> Word64 -> ST s ()
writeByteSliceWord64LE = undefined

indexWord8 :: ByteArray -> Int -> Word8
indexWord8 = undefined

indexWord64LE :: ByteArray -> Int -> Word64
indexWord64LE = undefined

indexByteSliceWord64LE :: ByteArray -> Int -> Int -> Word64
indexByteSliceWord64LE = undefined

sizeOfByteArray :: ByteArray -> Int
sizeOfByteArray = undefined

shortByteStringToByteArray :: ShortByteString -> ByteArray
shortByteStringToByteArray = undefined

byteArrayToShortByteString :: ByteArray -> ShortByteString
byteArrayToShortByteString = undefined

getSizeOfMutableByteArray :: MutableByteArray s -> ST s Int
getSizeOfMutableByteArray = undefined

shortByteStringToByteString :: ShortByteString -> ByteString
shortByteStringToByteString = undefined

-----------------
-- Boxed Array --
-----------------

data Array a

data MutableArray s a

newMutableArray :: Int -> a -> ST s (MutableArray s a)
newMutableArray = undefined

freezeMutableArray :: MutableArray s a -> ST s (Array a)
freezeMutableArray = undefined

writeArray :: MutableArray s a -> Int -> a -> ST s ()
writeArray = undefined

shuffleListM :: Monad m => (Word -> m Word) -> [a] -> m [a]
shuffleListM = undefined

shuffleListST :: (Monad (t (ST s)), MonadTrans t) => (Word -> t (ST s) Word) -> [a] -> t (ST s) [a]
shuffleListST = undefined
