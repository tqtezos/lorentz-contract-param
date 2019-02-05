-- | Module, containing CVal data type
-- which represents Michelson comparable values.

module Advanced.CValue
  (
    CVal (..)
  , Address
  ) where

import Data.Time.Clock (UTCTime)

import Advanced.Type (CT(..), T(..))
import Tezos.Crypto (KeyHash)

-- TODO use proper type
-- ByteString is not precisely catching "Hash of key" (which an address is)
-- We have 'KeyHash' in 'Tezos.Crypto'.
-- We have 'Address' in 'Michelson.Types'.
-- Probably the latter should be a wrapper over the former and be used here.

-- | Address type
type Address = ByteString

-- | Representation of comparable value
-- in Michelson language.
--
-- By specification, we're allowed to compare
-- only following types: int, nat, string, bytes,
-- mutez, bool, key_hash, timestamp, address.
--
-- Only these values can be used as map keys
-- or set elements.
data CVal t where
  CvInt       :: Integer -> CVal 'T_int
  CvNat       :: Natural -> CVal 'T_nat
  CvString    :: Text -> CVal 'T_string
  CvBytes     :: ByteString -> CVal 'T_bytes
  CvMutez     :: Int64 -> CVal 'T_mutez
  CvBool      :: Bool -> CVal 'T_bool
  CvKeyHash   :: KeyHash -> CVal 'T_key_hash
  CvTimestamp :: UTCTime -> CVal 'T_timestamp
  CvAddress   :: Address -> CVal 'T_address

deriving instance Show (CVal t)
