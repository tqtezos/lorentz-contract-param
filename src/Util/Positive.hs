{-# LANGUAGE DeriveDataTypeable, DerivingStrategies #-}

-- | Definition of 'Positive' type and related utilities.
module Util.Positive
  ( Positive (..)
  , mkPositive
  , lengthNE
  , replicateNE
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Fmt (Buildable, pretty)

import Util.Instances ()

-- | Integer values starting from 1.
--
-- We define our own datatype in order to have 'Data' instance for it,
-- which can not be derived for third-party types without exported constructor.
newtype Positive = PositiveUnsafe { unPositive :: Natural }
  deriving stock (Eq, Ord, Data)
  deriving newtype (Show, Buildable, ToJSON, FromJSON)

mkPositive :: (Integral i, Buildable i) => i -> Either Text Positive
mkPositive a
  | a > 0     = Right $ PositiveUnsafe (fromIntegral a)
  | otherwise = Left $ "Number is not positive: " <> pretty a

-- | Count length of non-empty list.
lengthNE :: NonEmpty a -> Positive
lengthNE = PositiveUnsafe . fromIntegral . length

-- | Produce a non empty list consisting of the given value.
replicateNE :: Positive -> a -> NonEmpty a
replicateNE (PositiveUnsafe i) a = a :| replicate (fromIntegral i - 1) a
