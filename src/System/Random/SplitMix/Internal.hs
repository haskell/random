{-# LANGUAGE ScopedTypeVariables, BangPatterns, UnboxedTuples, MagicHash, GADTs #-}
{-# LANGUAGE DeriveFunctor, DeriveDataTypeable #-}


module System.Random.SplitMix.Internal(
  nextSeedSplitMix
  ,splitGeneratorSplitMix
  ,nextWord64SplitMix
  ,splitGeneratorSplitMix#
  ,nextWord64SplitMix#
  ,SplitMix64(..)
  ,Random(..)
  ,sampleWord64Random
  ,RandomT(..)
  ,sampleWord64RandomT
  ) where

import qualified  Data.Bits  as DB
import Data.Bits (xor,(.|.))
import Data.Word(Word64)
import Data.Functor.Identity
import Data.Data(Data(),Typeable())

{-
splitmix constants follows
https://github.com/dmlloyd/openjdk/blob/67672eec97164de10a9ca83ddbcef6b42816ed04/src/java.base/share/classes/java/util/SplittableRandom.java

see also
http://hg.openjdk.java.net/jdk/jdk10/file/bffcbf07ea88/src/java.base/share/classes/java/util/SplittableRandom.java

ie the variant found in JDK >=8

see also discussion on the melissa o'neil pcg blog about
splitmix
http://www.pcg-random.org/posts/bugs-in-splitmix.html
-}

{-# SPECIALIZE popCount :: Word64 -> Word64 #-}
{-# SPECIALIZE popCount :: Int -> Word64 #-}
{-# SPECIALIZE popCount :: Word -> Word64 #-}
popCount :: DB.FiniteBits b => b -> Word64
popCount  = \ w ->  fromIntegral $ DB.popCount w


{-# SPECIALIZE xorShiftR :: Int -> Word64 -> Word64 #-}
xorShiftR :: DB.FiniteBits  b => Int -> b ->  b
xorShiftR = \ shift val  ->  val `xor` ( val `DB.unsafeShiftR` shift)


xorShiftR33 :: Word64 -> Word64
xorShiftR33 = \ w -> xorShiftR 33 w


firstRoundMix64 :: Word64 -> Word64
firstRoundMix64 = \ w ->  xorShiftR33 w * 0xff51afd7ed558ccd

secondRoundMix64 :: Word64 -> Word64
secondRoundMix64 = \ w -> xorShiftR33 w * 0xc4ceb9fe1a85ec53



mix64variant13 :: Word64 -> Word64
mix64variant13 = \ w -> xorShiftR 31 $ secondRoundMix64Variant13 $ firstRoundMix64Variant13 w

firstRoundMix64Variant13 :: Word64 -> Word64
firstRoundMix64Variant13 = \ w -> xorShiftR 30 w * 0xbf58476d1ce4e5b9

secondRoundMix64Variant13 :: Word64 -> Word64
secondRoundMix64Variant13 = \ w -> xorShiftR 27 w * 0x94d049bb133111eb

mix64 :: Word64 -> Word64
mix64 = \ w -> xorShiftR33 $  secondRoundMix64 $ firstRoundMix64 w

mixGamma :: Word64 -> Word64
mixGamma = \ w -> runIdentity $!
  do
    !mixedGamma <- return $! (mix64variant13 w .|. 1)
    !bitCount <- return $! popCount $ xorShiftR 1 mixedGamma
    if bitCount >= 24
      then return (mixedGamma `xor` 0xaaaaaaaaaaaaaaaa)
      else return mixedGamma

{-

theres a few different alternatives we could do for the RNG state

-- this isn't quite expressible
type SplitMix64 = (# Word64# , Word64# #)
-}

data SplitMix64 = SplitMix64 { sm64seed :: {-# UNPACK #-} !Word64
                              ,sm64Gamma :: {-# UNPACK #-} !Word64 }
   deriving (Eq,Ord,Read,Show,Data,Typeable)


advanceSplitMix :: SplitMix64 -> SplitMix64
advanceSplitMix (SplitMix64 sd gamma) = SplitMix64 (sd + gamma) gamma

nextSeedSplitMix :: SplitMix64 -> (# Word64, SplitMix64 #)
nextSeedSplitMix gen@(SplitMix64 result _) =  newgen `seq` (# result,newgen #)
  where
    newgen = advanceSplitMix gen


newtype Random  a =  Random# {unRandom# ::  SplitMix64 -> (# a , SplitMix64 #)}

instance  Functor Random where
  fmap = \ f (Random# mf) ->
              Random# $  \ seed ->
                       let  (# !a , !s'  #) = mf seed
                            !b =  f a
                          in  (# b , s' #)

instance Applicative Random where
  pure = \ x ->  Random# $  \ s  -> (#  x , s  #)
  (<*>)  = \ (Random# frmb) (Random# rma) ->  Random# $ \ s ->
                    let (# fseed, maseed #) = splitGeneratorSplitMix# s
                        (# f , _boringSeed #) = frmb fseed
                        (# a , newSeed #) = rma  maseed
                        in (#  f a , newSeed  #)

instance Monad Random where
  (>>=) =
    \(Random# ma) f ->
      Random# $ \ s ->
        let (# splitSeed , nextSeed #) = splitGeneratorSplitMix# s
            (# a, _boringSeed #) = ma splitSeed
            in  unRandom# (f a) nextSeed

sampleWord64Random :: Random Word64
sampleWord64Random = Random# nextWord64SplitMix#

newtype RandomT m a = RandomT# { unRandomT# :: (SplitMix64 ->  m (a , SplitMix64) ) }

instance Functor m => Functor (RandomT m) where
  fmap = \ f (RandomT# mf) ->
              RandomT# $  \ seed ->
                              fmap (\(a,s) -> (f a, s)  )   $ mf   seed

instance Applicative m => Applicative (RandomT m) where
  pure = \ x ->  RandomT# $  \ s  ->  pure  ( x , s  )
  (<*>)  = \ (RandomT# frmb) (RandomT# rma) ->  RandomT# $ \ s ->
                    let (# !fseed, !maseed #) = splitGeneratorSplitMix# s
                        mfOldSeed= frmb fseed
                        mArgNewSeed = rma  maseed
                        in (fmap (\(f,_s)-> \(x,newSeed)-> (f x, newSeed) ) mfOldSeed)
                            <*> mArgNewSeed

instance Monad m => Monad (RandomT m) where

  (>>=) = \ (RandomT#  ma) f -> RandomT# $  \ s ->
      let (# fseed, nextSeed #) = splitGeneratorSplitMix# s
       in
          do
            (a,_boring) <- ma fseed
            unRandomT# (f a) nextSeed

sampleWord64RandomT :: Applicative m => RandomT m Word64
sampleWord64RandomT = RandomT#  $ \ s ->
                        let (# !w, !ngen #) = nextWord64SplitMix# s
                           in  pure (w, ngen)

--instance PrimMonad m => PrimMonad (RandomT m) where
--  primitive = \ m ->
--  {-# INLINE #-}

nextWord64SplitMix# :: SplitMix64 -> (# Word64 , SplitMix64 #)
nextWord64SplitMix# gen = mixedRes `seq` (# mixedRes , newgen #)
  where
    mixedRes = mix64 premixres
    (#  premixres , newgen  #) = nextSeedSplitMix  gen

{-# INLINE nextWord64SplitMix #-}
nextWord64SplitMix :: SplitMix64 -> ( Word64 , SplitMix64 )
nextWord64SplitMix gen = (mixedRes, newgen)
  where
    (# mixedRes,newgen #) = nextWord64SplitMix# gen


splitGeneratorSplitMix# :: SplitMix64 -> (# SplitMix64 , SplitMix64 #)
splitGeneratorSplitMix# gen = splitGen `seq`( nextNextGen `seq` (# splitGen , nextNextGen #))
  where
    (# splitSeed , nextGen  #) = nextWord64SplitMix# gen
    (# splitPreMixGamma , nextNextGen #) = nextSeedSplitMix nextGen
    !splitGenGamma = mixGamma splitPreMixGamma
    !splitGen = SplitMix64 splitSeed splitGenGamma

{-# INLINE splitGeneratorSplitMix #-}
splitGeneratorSplitMix :: SplitMix64 -> (SplitMix64 , SplitMix64)
splitGeneratorSplitMix gen =  (splitGen, nextNextGen)
    where
      (# splitGen, nextNextGen #) = splitGeneratorSplitMix# gen
