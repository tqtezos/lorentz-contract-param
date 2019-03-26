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

import Michelson.Typed (CT(..), CVal(..), T(..), Val(..))
import Tezos.Core
  (Mutez(..), Timestamp, timestampFromSeconds, timestampFromUTCTime, timestampToSeconds,
  unsafeMkMutez)

instance Arbitrary (CVal 'CKeyHash) where
  arbitrary = CvKeyHash <$> arbitrary
instance Arbitrary (CVal 'CMutez) where
  arbitrary = CvMutez <$> arbitrary
instance Arbitrary (CVal 'CInt) where
  arbitrary = CvInt <$> arbitrary
instance Arbitrary (CVal a) => Arbitrary (Val instr ('Tc a)) where
  arbitrary = VC <$> arbitrary
instance Arbitrary (Val instr a) => Arbitrary (Val instr ('TList a)) where
  arbitrary = VList <$> arbitrary
instance Arbitrary (Val instr 'TUnit) where
  arbitrary = pure VUnit
instance (Arbitrary (Val instr a), Arbitrary (Val instr b))
    => Arbitrary (Val instr ('TPair a b)) where
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

-- | Minimal (earliest) timestamp used for @Arbitrary (CVal 'CTimestamp)@
minTimestamp :: Timestamp
minTimestamp = timestampFromUTCTime $ UTCTime minDay (fromInteger minSec)

-- | Maximal (latest) timestamp used for @Arbitrary (CVal 'CTimestamp)@
maxTimestamp :: Timestamp
maxTimestamp = timestampFromUTCTime $ UTCTime maxDay (fromInteger maxSec)

-- | Median of 'minTimestamp' and 'maxTimestamp'.
-- Useful for testing (exactly half of generated dates will be before and after
-- this date).
midTimestamp :: Timestamp
midTimestamp = timestampFromUTCTime $
  UTCTime ( ((maxDay `diffDays` minDay) `div` 2) `addDays` minDay)
          (fromInteger $ (maxSec - minSec) `div` 2)

instance Arbitrary (CVal 'CTimestamp) where
  arbitrary = CvTimestamp <$> arbitrary

instance Arbitrary Mutez where
  arbitrary = unsafeMkMutez <$> choose (unMutez minBound, unMutez maxBound)

instance Arbitrary Timestamp where
  arbitrary =
    timestampFromSeconds @Int <$>
    choose (timestampToSeconds minTimestamp, timestampToSeconds maxTimestamp)
