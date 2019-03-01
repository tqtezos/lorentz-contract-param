{-# LANGUAGE DeriveDataTypeable, DerivingStrategies #-}

-- | Core primitive Tezos types.

module Tezos.Core
  (
    -- * Mutez
    Mutez (unMutez)
  , mkMutez
  , unsafeMkMutez
  , addMutez
  , subMutez
  , mulMutez
  , divModMutez
  , divModMutezInt

    -- * Timestamp
  , Timestamp (..)
  ) where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Data (Data(..))

----------------------------------------------------------------------------
-- Mutez
----------------------------------------------------------------------------

-- | Mutez is a wrapper over integer data type. 1 mutez is 1 token (μTz).
newtype Mutez = Mutez
  { unMutez :: Word64
  } deriving stock (Show, Eq, Ord, Data, Generic)
    deriving newtype (Enum)

instance Bounded Mutez where
  minBound = Mutez 0
  -- 2⁶³ - 1
  -- This value was checked against the reference implementation.
  maxBound = Mutez 9223372036854775807

-- | Safely create 'Mutez' checking for overflow.
mkMutez :: Word64 -> Maybe Mutez
mkMutez n
  | n <= unMutez maxBound = Just (Mutez n)
  | otherwise = Nothing
{-# INLINE mkMutez #-}

-- | Partial function for 'Mutez' creation, it's pre-condition is that
-- the argument must not exceed the maximal 'Mutez' value.
unsafeMkMutez :: Word64 -> Mutez
unsafeMkMutez n =
  fromMaybe (error $ "mkMutez: overflow (" <> show n) (mkMutez n)
{-# INLINE unsafeMkMutez #-}

-- | Addition of 'Mutez' values. Returns 'Nothing' in case of overflow.
addMutez :: Mutez -> Mutez -> Maybe Mutez
addMutez (unMutez -> a) (unMutez -> b) =
  mkMutez (a + b) -- (a + b) can't overflow if 'Mutez' values are valid
{-# INLINE addMutez #-}

-- | Subtraction of 'Mutez' values. Returns 'Nothing' when the
-- subtrahend is greater than the minuend, and 'Just' otherwise.
subMutez :: Mutez -> Mutez -> Maybe Mutez
subMutez (unMutez -> a) (unMutez -> b)
  | a >= b = Just (Mutez (a - b))
  | otherwise = Nothing
{-# INLINE subMutez #-}

-- | Multiplication of 'Mutez' and an integral number. Returns
-- 'Nothing' in case of overflow.
mulMutez :: Integral a => Mutez -> a -> Maybe Mutez
mulMutez (unMutez -> a) b
    | res <= toInteger (unMutez maxBound) = Just (Mutez (fromInteger res))
    | otherwise = Nothing
  where
    res = toInteger a * toInteger b
{-# INLINE mulMutez #-}

-- | Euclidian division of two 'Mutez' values.
divModMutez :: Mutez -> Mutez -> Maybe (Word64, Mutez)
divModMutez a (unMutez -> b) = first unMutez <$> divModMutezInt a b

-- | Euclidian division of  'Mutez' and a number.
divModMutezInt :: Integral a => Mutez -> a -> Maybe (Mutez, Mutez)
divModMutezInt (toInteger . unMutez -> a) (toInteger -> b)
  | b <= 0 = Nothing
  | otherwise = Just $ bimap toMutez toMutez (a `divMod` b)
  where
    toMutez :: Integer -> Mutez
    toMutez = Mutez . fromInteger

----------------------------------------------------------------------------
-- Timestamp
----------------------------------------------------------------------------

-- | Dates in the real world. It's Unix time in seconds.
--
-- TODO: perhaps, we should use some better representation.
newtype Timestamp = Timestamp
  { unTimestamp :: Word64
  } deriving stock (Show, Eq, Ord, Data, Generic)

deriveJSON defaultOptions ''Mutez
deriveJSON defaultOptions ''Timestamp
