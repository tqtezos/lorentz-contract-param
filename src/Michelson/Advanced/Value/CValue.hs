-- | Module, containing CVal data type
-- which represents Michelson comparable values.

module Michelson.Advanced.Value.CValue
  ( CVal (..)
  ) where

import Data.Time.Clock (UTCTime)

import Michelson.Types (Mutez)
import Michelson.Advanced.Type (CT(..))
import Tezos.Crypto (KeyHash, Address)

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
  CvMutez     :: Mutez -> CVal 'T_mutez
  CvBool      :: Bool -> CVal 'T_bool
  CvKeyHash   :: KeyHash -> CVal 'T_key_hash
  CvTimestamp :: UTCTime -> CVal 'T_timestamp
  CvAddress   :: Address -> CVal 'T_address

deriving instance Show (CVal t)
deriving instance Eq (CVal t)
deriving instance Ord (CVal t)
