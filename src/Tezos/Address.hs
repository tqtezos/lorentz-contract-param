-- | Address in Tezos.

module Tezos.Address
  ( Address (..)
  , mkKeyAddress
  , mkContractAddressRaw

  -- * Formatting
  , formatAddress
  , mformatAddress
  , parseAddress
  , unsafeParseAddress
  ) where

import Data.Aeson (FromJSON(..), FromJSONKey, ToJSON(..), ToJSONKey)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as BS
import Fmt (fmt, hexF, pretty)
import qualified Formatting.Buildable as Buildable
import Test.QuickCheck (Arbitrary(..), oneof, vector)

import Michelson.Text
import Tezos.Crypto

-- | Data type corresponding to address structure in Tezos.
data Address
  = KeyAddress !KeyHash
  -- ^ `tz` address which is a hash of a public key.
  | ContractAddress !ByteString
  -- ^ `KT` address which corresponds to a callable contract.
  -- It's a hash of origination command.
  -- TODO: we should probably have a `Hash` type.
  deriving (Show, Eq, Ord)

-- | Smart constructor for 'KeyAddress'.
mkKeyAddress :: PublicKey -> Address
mkKeyAddress = KeyAddress . hashKey

-- | Smart constructor for 'ContractAddress'. Its argument is
-- serialized origination operation.
--
-- Note: it's quite unsafe to pass 'ByteString', because we can pass
-- some garbage which is not a serialized origination operation, but
-- this operation includes contract itself and necessary types are
-- defined in 'Michelson.*'. So we have to serialize this data outside
-- this module and pass it here as a 'ByteString'. Alternatively we
-- could add some constraint, but it would be almost as unsafe as
-- passing a 'ByteString'. For this reason we add `Raw` suffix to this
-- function and provide a safer function in 'Michelson.Untyped.Instr'.
-- We may reconsider it later.
mkContractAddressRaw :: ByteString -> Address
mkContractAddressRaw = ContractAddress . blake2b160 . blake2b160

----------------------------------------------------------------------------
-- Formatting/parsing
----------------------------------------------------------------------------

formatAddress :: Address -> Text
formatAddress =
  \case
    KeyAddress h -> formatKeyHash h
    ContractAddress bs -> encodeBase58Check (contractAddressPrefix <> bs)

mformatAddress :: Address -> MText
mformatAddress = mkMTextUnsafe . formatAddress

instance Buildable.Buildable Address where
  build = Buildable.build . formatAddress

-- | Errors that can happen during address parsing.
data ParseAddressError
  = ParseAddressWrongBase58Check
  -- ^ Address is not in Base58Check format.
  | ParseAddressBothFailed !CryptoParseError !ParseContractAddressError
  -- ^ Both address parsers failed with some error.
  deriving (Show, Eq)

instance Buildable.Buildable ParseAddressError where
  build =
    \case
      ParseAddressWrongBase58Check -> "Wrong base58check format"
      ParseAddressBothFailed pkErr contractErr ->
        mconcat
        [ "Address is neither `KeyAddress` ("
        , Buildable.build pkErr
        , "), nor `ContractAddress` ("
        , Buildable.build contractErr
        , ")"
        ]

-- | Parse an address from its human-readable textual representation
-- used by Tezos (e. g. "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"). Or
-- fail if it's invalid.
parseAddress :: Text -> Either ParseAddressError Address
parseAddress addressText =
  case parseKeyHash addressText of
    Left CryptoParseWrongBase58Check -> Left ParseAddressWrongBase58Check
    Left keyAddrErr -> first (ParseAddressBothFailed keyAddrErr) $
      parseContractAddress addressText
    Right keyHash -> Right (KeyAddress keyHash)

-- | Partial version of 'parseAddress' which assumes that the address
-- is correct. Can be used in tests.
unsafeParseAddress :: HasCallStack => Text -> Address
unsafeParseAddress = either (error . pretty) id . parseAddress

data ParseContractAddressError
  = ParseContractAddressWrongBase58Check
  | ParseContractAddressWrongTag !ByteString
  | ParseContractAddressWrongSize !Int
  deriving (Show, Eq)

instance Buildable.Buildable ParseContractAddressError where
  build =
    \case
      ParseContractAddressWrongBase58Check ->
        "Wrong base58check format"
      ParseContractAddressWrongTag tag ->
        "Wrong tag for a contract address: " <> fmt (hexF tag)
      ParseContractAddressWrongSize s ->
        "Wrong size for a contract address: " <> Buildable.build s

parseContractAddress :: Text -> Either ParseContractAddressError Address
parseContractAddress text =
  case decodeBase58CheckWithPrefix contractAddressPrefix text of
    Left (B58CheckWithPrefixWrongPrefix prefix) ->
      Left (ParseContractAddressWrongTag prefix)
    Left B58CheckWithPrefixWrongEncoding ->
      Left ParseContractAddressWrongBase58Check
    -- We know that the length must be 20.
    -- Currently it's hardcoded here, later we'll probably have a `Hash` type.
    Right bs | length bs == 20 -> Right (ContractAddress bs)
             | otherwise -> Left $ ParseContractAddressWrongSize (length bs)

-- It's a magic constant used by Tezos to encode a contract address.
-- It was deduced empirically.
contractAddressPrefix :: ByteString
contractAddressPrefix = "\2\90\121"

----------------------------------------------------------------------------
-- Aeson instances
----------------------------------------------------------------------------

instance ToJSON Address where
  toJSON = Aeson.String . formatAddress
  toEncoding = Aeson.text . formatAddress

instance ToJSONKey Address where
  toJSONKey = AesonTypes.toJSONKeyText formatAddress

instance FromJSON Address where
  parseJSON =
    Aeson.withText "Address" $
    either (fail . pretty) pure . parseAddress

instance FromJSONKey Address where
  fromJSONKey =
    AesonTypes.FromJSONKeyTextParser
      (either (fail . pretty) pure . parseAddress)

----------------------------------------------------------------------------
-- Arbitrary
----------------------------------------------------------------------------

instance Arbitrary Address where
  arbitrary = oneof [genKeyAddress, genContractAddress]
    where
      genKeyAddress = KeyAddress <$> arbitrary
      genContractAddress = ContractAddress . BS.pack <$> vector 20
