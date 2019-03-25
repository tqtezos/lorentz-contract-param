-- | Module, containing CVal data type
-- which represents Michelson comparable values.

module Michelson.Typed.CValue
  ( CVal (..)
  , ToCVal
  , FromCVal
  , toCVal
  , fromCVal
  ) where

import Michelson.Typed.T (CT(..), ToCT)
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
data CVal t where
  CvInt       :: Integer -> CVal 'CInt
  CvNat       :: Natural -> CVal 'CNat
  CvString    :: Text -> CVal 'CString
  CvBytes     :: ByteString -> CVal 'CBytes
  CvMutez     :: Mutez -> CVal 'CMutez
  CvBool      :: Bool -> CVal 'CBool
  CvKeyHash   :: KeyHash -> CVal 'CKeyHash
  CvTimestamp :: Timestamp -> CVal 'CTimestamp
  CvAddress   :: Address -> CVal 'CAddress

deriving instance Show (CVal t)
deriving instance Eq (CVal t)
deriving instance Ord (CVal t)

-- | Converts a single Haskell value into @CVal@ representation.
class ToCVal a where
  toCVal :: a -> CVal (ToCT a)

-- | Converts a @CVal@ value into a single Haskell value.
class FromCVal t where
  fromCVal :: CVal (ToCT t) -> t

-- ToCVal, FromCVal instances

instance FromCVal Integer where
  fromCVal (CvInt i) = i

instance FromCVal Natural where
  fromCVal (CvNat i) = i

instance FromCVal Text where
  fromCVal (CvString s) = s

instance FromCVal Bool where
  fromCVal (CvBool b) = b

instance FromCVal ByteString where
  fromCVal (CvBytes b) = b

instance FromCVal Mutez where
  fromCVal (CvMutez m) = m

instance FromCVal KeyHash where
  fromCVal (CvKeyHash k) = k

instance FromCVal Timestamp where
  fromCVal (CvTimestamp t) = t

instance FromCVal Address where
  fromCVal (CvAddress a) = a

instance ToCVal Integer where
  toCVal = CvInt

instance ToCVal Int where
  toCVal = CvInt . fromIntegral

instance ToCVal Word64 where
  toCVal = CvNat . fromIntegral

instance ToCVal Natural where
  toCVal = CvNat

instance ToCVal Text where
  toCVal = CvString

instance ToCVal ByteString where
  toCVal = CvBytes

instance ToCVal Bool where
  toCVal = CvBool

instance ToCVal Mutez where
  toCVal = CvMutez

instance ToCVal KeyHash where
  toCVal = CvKeyHash

instance ToCVal Timestamp where
  toCVal = CvTimestamp

instance ToCVal Address where
  toCVal = CvAddress
