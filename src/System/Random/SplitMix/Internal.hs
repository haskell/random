{-# LANGUAGE ScopedTypeVariables, BangPatterns, UnboxedTuples #-}

module System.Random.SplitMix.Internal(
  --mix32,
  xorShiftR
  ) where

import qualified  Data.Bits  as DB
import Data.Bits (xor,(.|.))
import Data.Word(Word64(..))
import Data.Functor.Identity

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



data SplitMix64 = SplitMix64 { sm64seed :: {-# UNPACK #-} !Word64
                              ,sm64Gamma :: {-# UNPACK #-} !Word64 }


advanceSplitMix :: SplitMix64 -> SplitMix64
advanceSplitMix (SplitMix64 sd gamma) = SplitMix64 (sd + gamma) gamma

nextSeedSplitMix :: SplitMix64 -> (# Word64, SplitMix64 #)
nextSeedSplitMix gen@(SplitMix64 result _) =  newgen `seq` (# result,newgen #)
  where
    newgen = advanceSplitMix gen


newtype RandomM  a =  RandomM (SplitMix64 -> (# a , SplitMix64 #))

nextWord64SplitMix :: SplitMix64 -> (# Word64 , SplitMix64 #)
nextWord64SplitMix gen = mixedRes `seq` (# mixedRes , newgen #)
  where
    mixedRes = mix64 premixres
    (#  premixres , newgen  #) = nextSeedSplitMix  gen

splitGeneratorSplitMix :: SplitMix64 -> (# SplitMix64 , SplitMix64 #)
splitGeneratorSplitMix gen = splitGen `seq`( nextNextGen `seq` (# splitGen , nextNextGen #))
  where
    (# splitSeed , nextGen  #) = nextWord64SplitMix gen
    (# splitPreMixGamma , nextNextGen #) = nextSeedSplitMix nextGen
    !splitGenGamma = mixGamma splitPreMixGamma
    !splitGen = SplitMix64 splitSeed splitGenGamma

{-

struct SplitMix64* split_generator(struct SplitMix64* generator) {
  struct SplitMix64* new_generator = (struct SplitMix64*) malloc(sizeof(struct SplitMix64));
  new_generator->seed = next_int64(generator);
  new_generator->gamma = mix_gamma(next_seed(generator));
  return new_generator;
}

inline void advance(struct SplitMix64* generator);
inline uint64_t next_seed(struct SplitMix64* generator);

inline void advance(struct SplitMix64* generator) {
  generator->seed += generator->gamma;
}

inline uint64_t next_seed(struct SplitMix64* generator) {
  uint64_t result = generator->seed;
  advance(generator);
  return result;
}


uint64_t next_int64(struct SplitMix64* generator) {
  return mix64(next_seed(generator));
}

uint64_t next_bounded_int64(struct SplitMix64* generator, uint64_t bound) {
  uint64_t threshold = -bound % bound;
  while (1) {
    uint64_t r = next_int64(generator);
    if (r >= threshold) {
      return r % bound;
    }
  }
}



struct SplitMix64 {
  uint64_t seed;
  uint64_t gamma;
};
uint64_t mix_gamma(uint64_t value) {
  uint64_t mixed_gamma = mix64variant13(value) | 1;
  int bit_count = pop_count(xor_shift(1, mixed_gamma));
  if (bit_count >= 24) {
    return mixed_gamma ^ 0xaaaaaaaaaaaaaaaa;
  }
  return mixed_gamma;
}

uint64_t mix64(uint64_t value) {
  return xor_shift33(second_round_mix64(first_round_mix64(value)));

inline uint64_t mix64variant13(uint64_t value) {
  return xor_shift(31, second_round_mix64_variant13(first_round_mix64_variant13(value)));



inline uint64_t first_round_mix64_variant13(uint64_t value) {
  return xor_shift(30, value) * 0xbf58476d1ce4e5b9;
}

inline uint64_t second_round_mix64_variant13(uint64_t value) {
  return xor_shift(27, value) * 0x94d049bb133111eb;
-}
