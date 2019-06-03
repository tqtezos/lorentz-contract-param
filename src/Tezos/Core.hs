{-# LANGUAGE DeriveDataTypeable, DerivingStrategies #-}

-- | Core primitive Tezos types.

module Tezos.Core
  (
    -- * Mutez
    Mutez (unMutez)
  , mkMutez
  , unsafeMkMutez
  , toMutez
  , addMutez
  , unsafeAddMutez
  , subMutez
  , unsafeSubMutez
  , mulMutez
  , divModMutez
  , divModMutezInt

    -- * Timestamp
  , Timestamp (..)
  , timestampToSeconds
  , timestampFromSeconds
  , timestampFromUTCTime
  , timestampPlusSeconds
  , formatTimestamp
  , parseTimestamp
  , getCurrentTime
  ) where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Data (Data(..))
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.LocalTime (utc, utcToZonedTime, zonedTimeToUTC)
import Data.Time.RFC3339 (formatTimeRFC3339, parseTimeRFC3339)
import Formatting.Buildable (Buildable(build))

----------------------------------------------------------------------------
-- Mutez
----------------------------------------------------------------------------

-- | Mutez is a wrapper over integer data type. 1 mutez is 1 token (μTz).
newtype Mutez = Mutez
  { unMutez :: Word64
  } deriving stock (Show, Eq, Ord, Data, Generic)
    deriving newtype (Enum, Buildable)

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
unsafeMkMutez :: HasCallStack => Word64 -> Mutez
unsafeMkMutez n =
  fromMaybe (error $ "mkMutez: overflow (" <> show n <> ")") (mkMutez n)
{-# INLINE unsafeMkMutez #-}

-- | Safely create 'Mutez'.
--
-- This is recommended way to create @Mutez@ from a numeric literal;
-- you can't construct all valid @Mutez@ values using this function
-- but for small values it works neat.
--
-- Warnings displayed when trying to construct invalid 'Natural' or 'Word'
-- literal are hardcoded for these types in GHC implementation, so we can only
-- exploit these existing rules.
toMutez :: Word32 -> Mutez
toMutez = unsafeMkMutez . fromIntegral
{-# INLINE toMutez #-}

-- | Addition of 'Mutez' values. Returns 'Nothing' in case of overflow.
addMutez :: Mutez -> Mutez -> Maybe Mutez
addMutez (unMutez -> a) (unMutez -> b) =
  mkMutez (a + b) -- (a + b) can't overflow if 'Mutez' values are valid
{-# INLINE addMutez #-}

-- | Partial addition of 'Mutez', should be used only if you're
-- sure there'll be no overflow.
unsafeAddMutez :: HasCallStack => Mutez -> Mutez -> Mutez
unsafeAddMutez = fromMaybe (error "unsafeAddMutez: overflow") ... addMutez

-- | Subtraction of 'Mutez' values. Returns 'Nothing' when the
-- subtrahend is greater than the minuend, and 'Just' otherwise.
subMutez :: Mutez -> Mutez -> Maybe Mutez
subMutez (unMutez -> a) (unMutez -> b)
  | a >= b = Just (Mutez (a - b))
  | otherwise = Nothing
{-# INLINE subMutez #-}

-- | Partial subtraction of 'Mutez', should be used only if you're
-- sure there'll be no underflow.
unsafeSubMutez :: HasCallStack => Mutez -> Mutez -> Mutez
unsafeSubMutez = fromMaybe (error "unsafeSubMutez: underflow") ... subMutez

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
  | otherwise = Just $ bimap toMutez' toMutez' (a `divMod` b)
  where
    toMutez' :: Integer -> Mutez
    toMutez' = Mutez . fromInteger

----------------------------------------------------------------------------
-- Timestamp
----------------------------------------------------------------------------

-- | Time in the real world.
-- Use the functions below to convert it to/from Unix time in seconds.
newtype Timestamp = Timestamp
  { unTimestamp :: POSIXTime
  } deriving stock (Show, Eq, Ord, Data, Generic)

timestampToSeconds :: Integral a => Timestamp -> a
timestampToSeconds = round . unTimestamp
{-# INLINE timestampToSeconds #-}

timestampFromSeconds :: Integral a => a -> Timestamp
timestampFromSeconds = Timestamp . fromIntegral
{-# INLINE timestampFromSeconds #-}

timestampFromUTCTime :: UTCTime -> Timestamp
timestampFromUTCTime = Timestamp . utcTimeToPOSIXSeconds
{-# INLINE timestampFromUTCTime #-}

-- | Add given amount of seconds to a 'Timestamp'.
timestampPlusSeconds :: Timestamp -> Integer -> Timestamp
timestampPlusSeconds ts sec = timestampFromSeconds (timestampToSeconds ts + sec)

-- | Display timestamp in human-readable way as used by Michelson.
-- Uses UTC timezone, though maybe we should take it as an argument.
formatTimestamp :: Timestamp -> Text
formatTimestamp =
  formatTimeRFC3339 . utcToZonedTime utc . posixSecondsToUTCTime . unTimestamp

instance Buildable Timestamp where
  build = build . formatTimestamp

-- | Parse textual representation of 'Timestamp'.
parseTimestamp :: Text -> Maybe Timestamp
parseTimestamp = fmap (timestampFromUTCTime . zonedTimeToUTC) . parseTimeRFC3339

-- | Return current time as 'Timestamp'.
getCurrentTime :: IO Timestamp
getCurrentTime = Timestamp <$> getPOSIXTime

----------------------------------------------------------------------------
-- JSON
----------------------------------------------------------------------------

deriveJSON defaultOptions ''Mutez
deriveJSON defaultOptions ''Timestamp
