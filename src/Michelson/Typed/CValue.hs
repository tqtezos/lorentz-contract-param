-- | Module, containing CValue data type
-- which represents Michelson comparable values.

module Michelson.Typed.CValue
  ( CValue (..)
  ) where

import Michelson.Typed.T (CT(..))
import Tezos.Address (Address)
import Tezos.Core (Mutez, Timestamp)
import Tezos.Crypto (KeyHash)

-- | Representation of comparable value
-- in Michelson language.
--
-- By specification, we're allowed to compare
-- only following types: int, nat, string, bytes,
-- mutez, bool, key_hash, timestamp, address.
--
-- Only these values can be used as map keys
-- or set elements.
data CValue t where
  CvInt       :: Integer -> CValue 'CInt
  CvNat       :: Natural -> CValue 'CNat
  CvString    :: Text -> CValue 'CString
  CvBytes     :: ByteString -> CValue 'CBytes
  CvMutez     :: Mutez -> CValue 'CMutez
  CvBool      :: Bool -> CValue 'CBool
  CvKeyHash   :: KeyHash -> CValue 'CKeyHash
  CvTimestamp :: Timestamp -> CValue 'CTimestamp
  CvAddress   :: Address -> CValue 'CAddress

deriving instance Show (CValue t)
deriving instance Eq (CValue t)
deriving instance Ord (CValue t)
