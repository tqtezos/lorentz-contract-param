{-# LANGUAGE DeriveDataTypeable, DerivingStrategies #-}

-- | Core primitive Tezos types.

module Tezos.Core
  ( Mutez (..)
  , Timestamp (..)
  ) where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Data (Data(..))

-- | Mutez is a wrapper over integer data type. 1 mutez is 1 toke (μTz).
newtype Mutez = Mutez
  { unMutez :: Word64
  } deriving stock (Show, Eq, Ord, Data, Generic)
    deriving newtype (Num, Integral, Real, Enum, Bounded)

-- | Dates in the real world. It's Unix time in seconds.
--
-- TODO: perhaps, we should use some better representation.
newtype Timestamp = Timestamp
  { unTimestamp :: Word64
  } deriving stock (Show, Eq, Ord, Data, Generic)

deriveJSON defaultOptions ''Mutez
deriveJSON defaultOptions ''Timestamp
