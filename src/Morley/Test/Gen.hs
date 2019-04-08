{-# OPTIONS_GHC -Wno-orphans #-}

-- | Utilities for arbitrary data generation in property tests.

module Morley.Test.Gen
  ( minTimestamp
  , maxTimestamp
  , midTimestamp
  ) where

import Data.Time.Calendar (Day, addDays, diffDays)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Test.QuickCheck (Arbitrary(..), choose)

import Michelson.Typed (CT(..), CValue(..), T(..), Value'(..))
import Tezos.Core
  (Mutez(..), Timestamp, timestampFromSeconds, timestampFromUTCTime, timestampToSeconds,
  unsafeMkMutez)

instance Arbitrary (CValue 'CKeyHash) where
  arbitrary = CvKeyHash <$> arbitrary
instance Arbitrary (CValue 'CMutez) where
  arbitrary = CvMutez <$> arbitrary
instance Arbitrary (CValue 'CInt) where
  arbitrary = CvInt <$> arbitrary
instance Arbitrary (CValue a) => Arbitrary (Value' instr ('Tc a)) where
  arbitrary = VC <$> arbitrary
instance Arbitrary (Value' instr a) => Arbitrary (Value' instr ('TList a)) where
  arbitrary = VList <$> arbitrary
instance Arbitrary (Value' instr 'TUnit) where
  arbitrary = pure VUnit
instance (Arbitrary (Value' instr a), Arbitrary (Value' instr b))
    => Arbitrary (Value' instr ('TPair a b)) where
  arbitrary = VPair ... (,) <$> arbitrary <*> arbitrary

minDay :: Day
minDay = fromMaybe (error "failed to parse day 2008-11-01") $
            parseTimeM True defaultTimeLocale "%Y-%-m-%-d" "2008-11-01"

maxDay :: Day
maxDay = fromMaybe (error "failed to parse day 2024-11-01") $
            parseTimeM True defaultTimeLocale "%Y-%-m-%-d" "2024-11-01"

minSec :: Integer
minSec = 0

maxSec :: Integer
maxSec = 86399

-- | Minimal (earliest) timestamp used for @Arbitrary (CValue 'CTimestamp)@
minTimestamp :: Timestamp
minTimestamp = timestampFromUTCTime $ UTCTime minDay (fromInteger minSec)

-- | Maximal (latest) timestamp used for @Arbitrary (CValue 'CTimestamp)@
maxTimestamp :: Timestamp
maxTimestamp = timestampFromUTCTime $ UTCTime maxDay (fromInteger maxSec)

-- | Median of 'minTimestamp' and 'maxTimestamp'.
-- Useful for testing (exactly half of generated dates will be before and after
-- this date).
midTimestamp :: Timestamp
midTimestamp = timestampFromUTCTime $
  UTCTime ( ((maxDay `diffDays` minDay) `div` 2) `addDays` minDay)
          (fromInteger $ (maxSec - minSec) `div` 2)

instance Arbitrary (CValue 'CTimestamp) where
  arbitrary = CvTimestamp <$> arbitrary

instance Arbitrary Mutez where
  arbitrary = unsafeMkMutez <$> choose (unMutez minBound, unMutez maxBound)

instance Arbitrary Timestamp where
  arbitrary =
    timestampFromSeconds @Int <$>
    choose (timestampToSeconds minTimestamp, timestampToSeconds maxTimestamp)
