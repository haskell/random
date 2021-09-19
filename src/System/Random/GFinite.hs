-- |
-- Module      :  System.Random.GFinite
-- Copyright   :  (c) Andrew Lelechenko 2020
-- License     :  BSD-style (see the file LICENSE in the 'random' repository)
-- Maintainer  :  libraries@haskell.org
--

{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}

module System.Random.GFinite
  ( Cardinality(..)
  , Finite(..)
  , GFinite(..)
  ) where

import Data.Bits
import Data.Int
import Data.Void
import Data.Word
import GHC.Exts (Proxy#, proxy#)
import GHC.Generics

-- | Cardinality of a set.
data Cardinality
  = Shift !Int -- ^ Shift n is equivalent to Card (bit n)
  | Card  !Integer
  deriving (Eq, Ord, Show)

-- | This is needed only as a superclass of 'Integral'.
instance Enum Cardinality where
  toEnum = fromIntegral
  fromEnum = fromIntegral
  succ = (+ 1)
  pred = subtract 1
  enumFrom x           = map fromInteger (enumFrom (toInteger x))
  enumFromThen x y     = map fromInteger (enumFromThen (toInteger x) (toInteger y))
  enumFromTo x y       = map fromInteger (enumFromTo (toInteger x) (toInteger y))
  enumFromThenTo x y z = map fromInteger (enumFromThenTo (toInteger x) (toInteger y) (toInteger z))

instance Num Cardinality where
  fromInteger 1 = Shift 0  -- ()
  fromInteger 2 = Shift 1  -- Bool
  fromInteger n = Card n
  {-# INLINE fromInteger #-}

  x + y = fromInteger (toInteger x + toInteger y)
  {-# INLINE (+) #-}

  Shift x * Shift y = Shift (x + y)
  Shift x * Card  y = Card (y `shiftL` x)
  Card  x * Shift y = Card (x `shiftL` y)
  Card  x * Card  y = Card (x * y)
  {-# INLINE (*) #-}

  abs    = Card . abs    . toInteger
  signum = Card . signum . toInteger
  negate = Card . negate . toInteger

-- | This is needed only as a superclass of 'Integral'.
instance Real Cardinality where
  toRational = fromIntegral

instance Integral Cardinality where
  toInteger = \case
    Shift n -> bit n
    Card  n -> n
  {-# INLINE toInteger #-}

  quotRem x' = \case
    Shift n -> (Card (x `shiftR` n), Card (x .&. (bit n - 1)))
    Card  n -> let (q, r) = x `quotRem` n in (Card q, Card r)
    where
      x = toInteger x'
  {-# INLINE quotRem #-}

-- | A type class for data with a finite number of inhabitants.
-- This type class is used
-- in default implementations of 'System.Random.Stateful.Uniform'.
--
-- Users are not supposed to write instances of 'Finite' manually.
-- There is a default implementation in terms of 'Generic' instead.
--
-- >>> :set -XDeriveGeneric -XDeriveAnyClass
-- >>> import GHC.Generics (Generic)
-- >>> data MyBool = MyTrue | MyFalse deriving (Generic, Finite)
-- >>> data Action = Code MyBool | Eat (Maybe Bool) | Sleep deriving (Generic, Finite)
--
class Finite a where
  cardinality :: Proxy# a -> Cardinality
  toFinite :: Integer -> a
  fromFinite :: a -> Integer

  default cardinality :: (Generic a, GFinite (Rep a)) => Proxy# a -> Cardinality
  cardinality _ = gcardinality (proxy# :: Proxy# (Rep a))
  {-# INLINE cardinality #-}

  default toFinite :: (Generic a, GFinite (Rep a)) => Integer -> a
  toFinite = to . toGFinite
  {-# INLINE toFinite #-}

  default fromFinite :: (Generic a, GFinite (Rep a)) => a -> Integer
  fromFinite = fromGFinite . from
  {-# INLINE fromFinite #-}

class GFinite f where
  gcardinality :: Proxy# f -> Cardinality
  toGFinite :: Integer -> f a
  fromGFinite :: f a -> Integer

instance GFinite V1 where
  gcardinality _ = 0
  {-# INLINE gcardinality #-}
  toGFinite = const $ error "GFinite: V1 has no inhabitants"
  {-# INLINE toGFinite #-}
  fromGFinite = const $ error "GFinite: V1 has no inhabitants"
  {-# INLINE fromGFinite #-}

instance GFinite U1 where
  gcardinality _ = 1
  {-# INLINE gcardinality #-}
  toGFinite = const U1
  {-# INLINE toGFinite #-}
  fromGFinite = const 0
  {-# INLINE fromGFinite #-}

instance Finite a => GFinite (K1 _x a) where
  gcardinality _ = cardinality (proxy# :: Proxy# a)
  {-# INLINE gcardinality #-}
  toGFinite = K1 . toFinite
  {-# INLINE toGFinite #-}
  fromGFinite = fromFinite . unK1
  {-# INLINE fromGFinite #-}

instance GFinite a => GFinite (M1 _x _y a) where
  gcardinality _ = gcardinality (proxy# :: Proxy# a)
  {-# INLINE gcardinality #-}
  toGFinite = M1 . toGFinite
  {-# INLINE toGFinite #-}
  fromGFinite = fromGFinite . unM1
  {-# INLINE fromGFinite #-}

instance (GFinite a, GFinite b) => GFinite (a :+: b) where
  gcardinality _ =
    gcardinality (proxy# :: Proxy# a) + gcardinality (proxy# :: Proxy# b)
  {-# INLINE gcardinality #-}

  toGFinite n
    | n < cardA = L1 $ toGFinite n
    | otherwise = R1 $ toGFinite (n - cardA)
    where
      cardA = toInteger (gcardinality (proxy# :: Proxy# a))
  {-# INLINE toGFinite #-}

  fromGFinite = \case
     L1 x -> fromGFinite x
     R1 x -> fromGFinite x + toInteger (gcardinality (proxy# :: Proxy# a))
  {-# INLINE fromGFinite #-}

instance (GFinite a, GFinite b) => GFinite (a :*: b) where
  gcardinality _ =
    gcardinality (proxy# :: Proxy# a) * gcardinality (proxy# :: Proxy# b)
  {-# INLINE gcardinality #-}

  toGFinite n = toGFinite (toInteger q) :*: toGFinite (toInteger r)
    where
      cardB = gcardinality (proxy# :: Proxy# b)
      (q, r) = Card n `quotRem` cardB
  {-# INLINE toGFinite #-}

  fromGFinite (q :*: r) =
    toInteger (gcardinality (proxy# :: Proxy# b) * Card (fromGFinite q)) + fromGFinite r
  {-# INLINE fromGFinite #-}

instance Finite Void
instance Finite ()
instance Finite Bool
instance Finite Ordering

instance Finite Char where
  cardinality _ = Card $ toInteger (fromEnum (maxBound :: Char)) + 1
  {-# INLINE cardinality #-}
  toFinite = toEnum . fromInteger
  {-# INLINE toFinite #-}
  fromFinite = toInteger . fromEnum
  {-# INLINE fromFinite #-}

cardinalityDef :: forall a. (Num a, FiniteBits a) => Proxy# a -> Cardinality
cardinalityDef _ = Shift (finiteBitSize (0 :: a))

toFiniteDef :: forall a. (Num a, FiniteBits a) => Integer -> a
toFiniteDef n
    | isSigned (0 :: a) = fromInteger (n - bit (finiteBitSize (0 :: a) - 1))
    | otherwise = fromInteger n

fromFiniteDef :: (Integral a, FiniteBits a) => a -> Integer
fromFiniteDef x
    | isSigned x = toInteger x + bit (finiteBitSize x - 1)
    | otherwise = toInteger x

instance Finite Word8 where
  cardinality = cardinalityDef
  {-# INLINE cardinality #-}
  toFinite = toFiniteDef
  {-# INLINE toFinite #-}
  fromFinite = fromFiniteDef
  {-# INLINE fromFinite #-}
instance Finite Word16 where
  cardinality = cardinalityDef
  {-# INLINE cardinality #-}
  toFinite = toFiniteDef
  {-# INLINE toFinite #-}
  fromFinite = fromFiniteDef
  {-# INLINE fromFinite #-}
instance Finite Word32 where
  cardinality = cardinalityDef
  {-# INLINE cardinality #-}
  toFinite = toFiniteDef
  {-# INLINE toFinite #-}
  fromFinite = fromFiniteDef
  {-# INLINE fromFinite #-}
instance Finite Word64 where
  cardinality = cardinalityDef
  {-# INLINE cardinality #-}
  toFinite = toFiniteDef
  {-# INLINE toFinite #-}
  fromFinite = fromFiniteDef
  {-# INLINE fromFinite #-}
instance Finite Word where
  cardinality = cardinalityDef
  {-# INLINE cardinality #-}
  toFinite = toFiniteDef
  {-# INLINE toFinite #-}
  fromFinite = fromFiniteDef
  {-# INLINE fromFinite #-}
instance Finite Int8 where
  cardinality = cardinalityDef
  {-# INLINE cardinality #-}
  toFinite = toFiniteDef
  {-# INLINE toFinite #-}
  fromFinite = fromFiniteDef
  {-# INLINE fromFinite #-}
instance Finite Int16 where
  cardinality = cardinalityDef
  {-# INLINE cardinality #-}
  toFinite = toFiniteDef
  {-# INLINE toFinite #-}
  fromFinite = fromFiniteDef
  {-# INLINE fromFinite #-}
instance Finite Int32 where
  cardinality = cardinalityDef
  {-# INLINE cardinality #-}
  toFinite = toFiniteDef
  {-# INLINE toFinite #-}
  fromFinite = fromFiniteDef
  {-# INLINE fromFinite #-}
instance Finite Int64 where
  cardinality = cardinalityDef
  {-# INLINE cardinality #-}
  toFinite = toFiniteDef
  {-# INLINE toFinite #-}
  fromFinite = fromFiniteDef
  {-# INLINE fromFinite #-}
instance Finite Int where
  cardinality = cardinalityDef
  {-# INLINE cardinality #-}
  toFinite = toFiniteDef
  {-# INLINE toFinite #-}
  fromFinite = fromFiniteDef
  {-# INLINE fromFinite #-}

instance Finite a => Finite (Maybe a)
instance (Finite a, Finite b) => Finite (Either a b)
instance (Finite a, Finite b) => Finite (a, b)
instance (Finite a, Finite b, Finite c) => Finite (a, b, c)
instance (Finite a, Finite b, Finite c, Finite d) => Finite (a, b, c, d)
instance (Finite a, Finite b, Finite c, Finite d, Finite e) => Finite (a, b, c, d, e)
instance (Finite a, Finite b, Finite c, Finite d, Finite e, Finite f) => Finite (a, b, c, d, e, f)
