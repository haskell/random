-- |
-- Module      :  System.Random.GFiniteRange
-- Copyright   :  (c) Andrew Lelechenko 2021
-- License     :  BSD-style (see the file LICENSE in the 'random' repository)
-- Maintainer  :  libraries@haskell.org
--

{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}

module System.Random.GFiniteRange
  ( FiniteRange(..)
  , GFiniteRange(..)
  ) where

import Data.Int
import Data.Void
import Data.Word
import GHC.Exts (Proxy#, proxy#)
import GHC.Generics
import Numeric.Natural (Natural)
import System.Random.GFinite (Finite, GFinite(..))

-- | A type class for data with a finite number of inhabitants in ranges,
-- a generalization of 'Finite'.
-- This type class is used
-- in the default implementation of 'System.Random.Stateful.UniformRange'.
--
-- Note that 'Integer' and 'Natural' cannot be 'Finite',
-- but are 'GFiniteRange'.
--
-- Users are not supposed to write instances of 'FiniteRange' manually.
-- There is a default implementation in terms of 'Generic' instead,
-- which assumes that ranges for product types are a Cartesian product
-- of ranges for respective components, and ranges for sum types
-- follow the order of constructors.
--
-- >>> :set -XDeriveGeneric -XDeriveAnyClass
-- >>> import GHC.Generics (Generic)
-- >>> data MyBool = MyTrue | MyFalse deriving (Generic, Finite, FiniteRange)
-- >>> data Action = Code MyBool MyBool | Eat (Maybe Bool) | Sleep deriving (Generic, Finite, FiniteRange)
--
class FiniteRange a where
  rangeCardinality :: (a, a) -> Integer
  isInFiniteRange  :: (a, a) -> a -> Bool
  toFiniteRange    :: (a, a) -> Integer -> a
  fromFiniteRange  :: (a, a) -> a -> Integer

  default rangeCardinality :: (Generic a, GFiniteRange (Rep a)) => (a, a) -> Integer
  rangeCardinality (lo, hi) = grangeCardinality (from lo, from hi)

  default isInFiniteRange :: (Generic a, GFiniteRange (Rep a)) => (a, a) -> a -> Bool
  isInFiniteRange (lo, hi) = isInGFiniteRange (from lo, from hi) . from

  default toFiniteRange :: (Generic a, GFiniteRange (Rep a)) => (a, a) -> Integer -> a
  toFiniteRange (lo, hi) = to . toGFiniteRange (from lo, from hi)

  default fromFiniteRange :: (Generic a, GFiniteRange (Rep a)) => (a, a) -> a -> Integer
  fromFiniteRange (lo, hi) = fromGFiniteRange (from lo, from hi) . from

class GFiniteRange f where
  grangeCardinality :: (f a, f a) -> Integer
  isInGFiniteRange  :: (f a, f a) -> f a -> Bool
  toGFiniteRange    :: (f a, f a) -> Integer -> f a
  fromGFiniteRange  :: (f a, f a) -> f a -> Integer

instance GFiniteRange V1 where
  grangeCardinality = const 0
  {-# INLINE grangeCardinality #-}
  isInGFiniteRange = const $ error "GFiniteRange: V1 has no inhabitants"
  {-# INLINE isInGFiniteRange #-}
  toGFiniteRange = const $ const $ error "GFiniteRange: V1 has no inhabitants"
  {-# INLINE toGFiniteRange #-}
  fromGFiniteRange = const $ const $ error "GFiniteRange: V1 has no inhabitants"
  {-# INLINE fromGFiniteRange #-}

instance GFiniteRange U1 where
  grangeCardinality = const 1
  {-# INLINE grangeCardinality #-}
  isInGFiniteRange = const $ const True
  {-# INLINE isInGFiniteRange #-}
  toGFiniteRange = const $ const U1
  {-# INLINE toGFiniteRange #-}
  fromGFiniteRange = const $ const 0
  {-# INLINE fromGFiniteRange #-}

instance FiniteRange a => GFiniteRange (K1 _x a) where
  grangeCardinality (K1 lo, K1 hi) = rangeCardinality (lo, hi)
  {-# INLINE grangeCardinality #-}
  isInGFiniteRange (K1 lo, K1 hi) = isInFiniteRange (lo, hi) . unK1
  {-# INLINE isInGFiniteRange #-}
  toGFiniteRange (K1 lo, K1 hi) = K1 . toFiniteRange (lo, hi)
  {-# INLINE toGFiniteRange #-}
  fromGFiniteRange (K1 lo, K1 hi) = fromFiniteRange (lo, hi) . unK1
  {-# INLINE fromGFiniteRange #-}

instance GFiniteRange a => GFiniteRange (M1 _x _y a) where
  grangeCardinality (M1 lo, M1 hi) = grangeCardinality (lo, hi)
  {-# INLINE grangeCardinality #-}
  isInGFiniteRange (M1 lo, M1 hi) = isInGFiniteRange (lo, hi) . unM1
  {-# INLINE isInGFiniteRange #-}
  toGFiniteRange (M1 lo, M1 hi) = M1 . toGFiniteRange (lo, hi)
  {-# INLINE toGFiniteRange #-}
  fromGFiniteRange (M1 lo, M1 hi) = fromGFiniteRange (lo, hi) . unM1
  {-# INLINE fromGFiniteRange #-}

instance (GFiniteRange a, GFiniteRange b) => GFiniteRange (a :*: b) where
  grangeCardinality (loA :*: loB, hiA :*: hiB) =
    grangeCardinality (loA, hiA) * grangeCardinality (loB, hiB)
  {-# INLINE grangeCardinality #-}

  isInGFiniteRange (loA :*: loB, hiA :*: hiB) (a :*: b) =
    isInGFiniteRange (loA, hiA) a && isInGFiniteRange (loB, hiB) b
  {-# INLINE isInGFiniteRange #-}

  toGFiniteRange (loA :*: loB, hiA :*: hiB) n =
    toGFiniteRange (loA, hiA) q :*: toGFiniteRange (loB, hiB) r
    where
      cardB = grangeCardinality (loB, hiB)
      (q, r) = n `quotRem` cardB
  {-# INLINE toGFiniteRange #-}

  fromGFiniteRange (loA :*: loB, hiA :*: hiB) (q :*: r) =
    grangeCardinality (loB, hiB) * fromGFiniteRange (loA, hiA) q + fromGFiniteRange (loB, hiB) r
  {-# INLINE fromGFiniteRange #-}

instance (GFinite a, GFiniteRange a, GFinite b, GFiniteRange b) => GFiniteRange (a :+: b) where
  grangeCardinality (L1 lo, L1 hi) = grangeCardinality (lo, hi)
  grangeCardinality (R1 lo, R1 hi) = grangeCardinality (lo, hi)
  grangeCardinality (L1 lo, R1 hi) = grangeCardinalityLR (lo, hi)
  grangeCardinality (R1 hi, L1 lo) = grangeCardinalityLR (lo, hi)
  {-# INLINE grangeCardinality #-}

  isInGFiniteRange (L1 lo, L1 hi) = \case
    L1 x -> isInGFiniteRange (lo, hi) x
    R1{} -> False
  isInGFiniteRange (R1 lo, R1 hi) = \case
    L1{} -> False
    R1 x -> isInGFiniteRange (lo, hi) x
  isInGFiniteRange (L1 lo, R1 hi) = isInGFiniteRangeLR (lo, hi)
  isInGFiniteRange (R1 hi, L1 lo) = isInGFiniteRangeLR (lo, hi)
  {-# INLINE isInGFiniteRange #-}

  toGFiniteRange (L1 lo, L1 hi) = L1 . toGFiniteRange (lo, hi)
  toGFiniteRange (R1 lo, R1 hi) = R1 . toGFiniteRange (lo, hi)
  toGFiniteRange (L1 lo, R1 hi) = toGFiniteRangeLR (lo, hi)
  toGFiniteRange (R1 hi, L1 lo) = toGFiniteRangeLR (lo, hi)
  {-# INLINE toGFiniteRange #-}

  fromGFiniteRange (L1 lo, L1 hi) = \case
    L1 x -> fromGFiniteRange (lo, hi) x
    R1{} -> error "GFiniteRange: R1 is out of (L1, L1) range"
  fromGFiniteRange (R1 lo, R1 hi) = \case
    L1{} -> error "GFiniteRange: L1 is out of (R1, R1) range"
    R1 x -> fromGFiniteRange (lo, hi) x
  fromGFiniteRange (L1 lo, R1 hi) = fromGFiniteRangeLR (lo, hi)
  fromGFiniteRange (R1 hi, L1 lo) = fromGFiniteRangeLR (lo, hi)
  {-# INLINE fromGFiniteRange #-}

grangeCardinalityLR
  :: forall a b x. (GFinite a, GFiniteRange a, GFinite b, GFiniteRange b)
  => (a x, b x) -> Integer
grangeCardinalityLR (lo, hi) =
  grangeCardinality (lo, maxBoundA) + grangeCardinality (minBoundB, hi)
  where
    maxBoundA = toGFinite (toInteger (gcardinality (proxy# :: Proxy# a) - 1))
    minBoundB = toGFinite 0
{-# INLINE grangeCardinalityLR #-}

isInGFiniteRangeLR
  :: forall a b x. (GFinite a, GFiniteRange a, GFinite b, GFiniteRange b)
  => (a x, b x) -> (a :+: b) x -> Bool
isInGFiniteRangeLR (lo, hi) = \case
  L1 x -> isInGFiniteRange (lo, maxBoundA) x
  R1 x -> isInGFiniteRange (minBoundB, hi) x
  where
    maxBoundA = toGFinite (toInteger (gcardinality (proxy# :: Proxy# a) - 1))
    minBoundB = toGFinite 0
{-# INLINE isInGFiniteRangeLR #-}

toGFiniteRangeLR
  :: forall a b x. (GFinite a, GFiniteRange a, GFinite b, GFiniteRange b)
  => (a x, b x) -> Integer -> (a :+: b) x
toGFiniteRangeLR (lo, hi) n
  | n < cardA = L1 (toGFiniteRange (lo, maxBoundA) n)
  | otherwise = R1 (toGFiniteRange (minBoundB, hi) (n - cardA))
  where
    maxBoundA = toGFinite (toInteger (gcardinality (proxy# :: Proxy# a) - 1))
    minBoundB = toGFinite 0
    cardA = grangeCardinality (lo, maxBoundA)
{-# INLINE toGFiniteRangeLR #-}

fromGFiniteRangeLR
  :: forall a b x. (GFinite a, GFiniteRange a, GFinite b, GFiniteRange b)
  => (a x, b x) -> (a :+: b) x -> Integer
fromGFiniteRangeLR (lo, hi) = \case
  L1 x -> fromGFiniteRange (lo, maxBoundA) x
  R1 x -> fromGFiniteRange (minBoundB, hi) x + cardA
  where
    maxBoundA = toGFinite (toInteger (gcardinality (proxy# :: Proxy# a) - 1))
    minBoundB = toGFinite 0
    cardA = grangeCardinality (lo, maxBoundA)
{-# INLINE fromGFiniteRangeLR #-}

instance FiniteRange Void
instance FiniteRange ()
instance FiniteRange Bool
instance FiniteRange Ordering

instance FiniteRange Char where
  rangeCardinality (lo, hi) = rangeCardinality (fromEnum lo, fromEnum hi)
  {-# INLINE rangeCardinality #-}
  isInFiniteRange = isInFiniteRangeDef
  {-# INLINE isInFiniteRange #-}
  toFiniteRange (lo, hi) = toEnum . toFiniteRange (fromEnum lo, fromEnum hi)
  {-# INLINE toFiniteRange #-}
  fromFiniteRange (lo, hi) = fromFiniteRange (fromEnum lo, fromEnum hi) . fromEnum
  {-# INLINE fromFiniteRange #-}

rangeCardinalityDef :: Integral a => (a, a) -> Integer
rangeCardinalityDef (lo, hi) = abs (toInteger hi - toInteger lo) + 1

isInFiniteRangeDef :: Ord a => (a, a) -> a -> Bool
isInFiniteRangeDef (lo, hi) x = min lo hi <= x && x <= max lo hi

toFiniteRangeDef :: (Ord a, Num a) => (a, a) -> Integer -> a
toFiniteRangeDef (a, b) n = fromInteger n + min a b

fromFiniteRangeDef :: Integral a => (a, a) -> a -> Integer
fromFiniteRangeDef (a, b) n = toInteger (n - min a b)

instance FiniteRange Word8 where
  rangeCardinality = rangeCardinalityDef
  {-# INLINE rangeCardinality #-}
  isInFiniteRange = isInFiniteRangeDef
  {-# INLINE isInFiniteRange #-}
  toFiniteRange = toFiniteRangeDef
  {-# INLINE toFiniteRange #-}
  fromFiniteRange = fromFiniteRangeDef
  {-# INLINE fromFiniteRange #-}
instance FiniteRange Word16 where
  rangeCardinality = rangeCardinalityDef
  {-# INLINE rangeCardinality #-}
  isInFiniteRange = isInFiniteRangeDef
  {-# INLINE isInFiniteRange #-}
  toFiniteRange = toFiniteRangeDef
  {-# INLINE toFiniteRange #-}
  fromFiniteRange = fromFiniteRangeDef
  {-# INLINE fromFiniteRange #-}
instance FiniteRange Word32 where
  rangeCardinality = rangeCardinalityDef
  {-# INLINE rangeCardinality #-}
  isInFiniteRange = isInFiniteRangeDef
  {-# INLINE isInFiniteRange #-}
  toFiniteRange = toFiniteRangeDef
  {-# INLINE toFiniteRange #-}
  fromFiniteRange = fromFiniteRangeDef
  {-# INLINE fromFiniteRange #-}
instance FiniteRange Word64 where
  rangeCardinality = rangeCardinalityDef
  {-# INLINE rangeCardinality #-}
  isInFiniteRange = isInFiniteRangeDef
  {-# INLINE isInFiniteRange #-}
  toFiniteRange = toFiniteRangeDef
  {-# INLINE toFiniteRange #-}
  fromFiniteRange = fromFiniteRangeDef
  {-# INLINE fromFiniteRange #-}
instance FiniteRange Word where
  rangeCardinality = rangeCardinalityDef
  {-# INLINE rangeCardinality #-}
  isInFiniteRange = isInFiniteRangeDef
  {-# INLINE isInFiniteRange #-}
  toFiniteRange = toFiniteRangeDef
  {-# INLINE toFiniteRange #-}
  fromFiniteRange = fromFiniteRangeDef
  {-# INLINE fromFiniteRange #-}
instance FiniteRange Natural where
  rangeCardinality = rangeCardinalityDef
  {-# INLINE rangeCardinality #-}
  isInFiniteRange = isInFiniteRangeDef
  {-# INLINE isInFiniteRange #-}
  toFiniteRange = toFiniteRangeDef
  {-# INLINE toFiniteRange #-}
  fromFiniteRange = fromFiniteRangeDef
  {-# INLINE fromFiniteRange #-}
instance FiniteRange Int8 where
  rangeCardinality = rangeCardinalityDef
  {-# INLINE rangeCardinality #-}
  isInFiniteRange = isInFiniteRangeDef
  {-# INLINE isInFiniteRange #-}
  toFiniteRange = toFiniteRangeDef
  {-# INLINE toFiniteRange #-}
  fromFiniteRange = fromFiniteRangeDef
  {-# INLINE fromFiniteRange #-}
instance FiniteRange Int16 where
  rangeCardinality = rangeCardinalityDef
  {-# INLINE rangeCardinality #-}
  isInFiniteRange = isInFiniteRangeDef
  {-# INLINE isInFiniteRange #-}
  toFiniteRange = toFiniteRangeDef
  {-# INLINE toFiniteRange #-}
  fromFiniteRange = fromFiniteRangeDef
  {-# INLINE fromFiniteRange #-}
instance FiniteRange Int32 where
  rangeCardinality = rangeCardinalityDef
  {-# INLINE rangeCardinality #-}
  isInFiniteRange = isInFiniteRangeDef
  {-# INLINE isInFiniteRange #-}
  toFiniteRange = toFiniteRangeDef
  {-# INLINE toFiniteRange #-}
  fromFiniteRange = fromFiniteRangeDef
  {-# INLINE fromFiniteRange #-}
instance FiniteRange Int64 where
  rangeCardinality = rangeCardinalityDef
  {-# INLINE rangeCardinality #-}
  isInFiniteRange = isInFiniteRangeDef
  {-# INLINE isInFiniteRange #-}
  toFiniteRange = toFiniteRangeDef
  {-# INLINE toFiniteRange #-}
  fromFiniteRange = fromFiniteRangeDef
  {-# INLINE fromFiniteRange #-}
instance FiniteRange Int where
  rangeCardinality = rangeCardinalityDef
  {-# INLINE rangeCardinality #-}
  isInFiniteRange = isInFiniteRangeDef
  {-# INLINE isInFiniteRange #-}
  toFiniteRange = toFiniteRangeDef
  {-# INLINE toFiniteRange #-}
  fromFiniteRange = fromFiniteRangeDef
  {-# INLINE fromFiniteRange #-}
instance FiniteRange Integer where
  rangeCardinality = rangeCardinalityDef
  {-# INLINE rangeCardinality #-}
  isInFiniteRange = isInFiniteRangeDef
  {-# INLINE isInFiniteRange #-}
  toFiniteRange = toFiniteRangeDef
  {-# INLINE toFiniteRange #-}
  fromFiniteRange = fromFiniteRangeDef
  {-# INLINE fromFiniteRange #-}

instance (Finite a, FiniteRange a) => FiniteRange (Maybe a)
instance (Finite a, FiniteRange a, Finite b, FiniteRange b) => FiniteRange (Either a b)
instance (FiniteRange a, FiniteRange b) => FiniteRange (a, b)
instance (FiniteRange a, FiniteRange b, FiniteRange c) => FiniteRange (a, b, c)
instance (FiniteRange a, FiniteRange b, FiniteRange c, FiniteRange d) => FiniteRange (a, b, c, d)
instance (FiniteRange a, FiniteRange b, FiniteRange c, FiniteRange d, FiniteRange e) => FiniteRange (a, b, c, d, e)
instance (FiniteRange a, FiniteRange b, FiniteRange c, FiniteRange d, FiniteRange e, FiniteRange f) => FiniteRange (a, b, c, d, e, f)
instance (FiniteRange a, FiniteRange b, FiniteRange c, FiniteRange d, FiniteRange e, FiniteRange f, FiniteRange g) => FiniteRange (a, b, c, d, e, f, g)
