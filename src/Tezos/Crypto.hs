-- | Cryptographic primitives used in Tezos.

module Tezos.Crypto
  ( -- * Cryptographic primitive types
    PublicKey (..)
  , SecretKey
  , Signature (..)
  , KeyHash (..)
  , detSecretKey
  , toPublic

  -- * Formatting
  , CryptoParseError (..)
  , formatPublicKey
  , mformatPublicKey
  , parsePublicKey
  , mkPublicKey
  , formatSecretKey
  , parseSecretKey
  , formatSignature
  , mformatSignature
  , parseSignature
  , mkSignature
  , formatKeyHash
  , mformatKeyHash
  , parseKeyHash

  -- * Signing
  , sign
  , checkSignature

  -- * Hashing
  , hashKey
  , blake2b
  , blake2b160
  , sha256
  , sha512

  -- * Utilities
  , encodeBase58Check
  , decodeBase58Check
  , B58CheckWithPrefixError (..)
  , decodeBase58CheckWithPrefix
  ) where

import Crypto.Error (CryptoError, CryptoFailable(..), eitherCryptoError)
import Crypto.Hash (Blake2b_160, Blake2b_256, Digest, SHA256, SHA512, hash)
import Crypto.Number.Serialize (os2ip)
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Crypto.Random (drgNewSeed, seedFromInteger, withDRG)
import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import Data.ByteArray (ByteArrayAccess)
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base58 as Base58
import Data.Coerce (coerce)
import Fmt (fmt, hexF, pretty)
import qualified Formatting.Buildable as Buildable
import Test.QuickCheck (Arbitrary(..), vector)

import Michelson.Text

----------------------------------------------------------------------------
-- Types, instances, conversions
----------------------------------------------------------------------------

-- | ED25519 public cryptographic key.
newtype PublicKey = PublicKey
  { unPublicKey :: Ed25519.PublicKey
  } deriving (Show, Eq)

instance Arbitrary PublicKey where
  arbitrary = toPublic <$> arbitrary

-- | ED25519 secret cryptographic key.
newtype SecretKey = SecretKey
  { unSecretKey :: Ed25519.SecretKey
  } deriving (Show, Eq)

-- | Deterministicaly generate a secret key from seed.
detSecretKey :: ByteString -> SecretKey
detSecretKey seed =
  let chachaSeed = drgNewSeed . seedFromInteger . os2ip $ seed
      (sk, _) = withDRG chachaSeed Ed25519.generateSecretKey
  in SecretKey sk

instance Arbitrary SecretKey where
  arbitrary = detSecretKey . BS.pack <$> vector 32

-- | Create a public key from a secret key.
toPublic :: SecretKey -> PublicKey
toPublic = PublicKey . Ed25519.toPublic . unSecretKey

-- | ED25519 cryptographic signature.
newtype Signature = Signature
  { unSignature :: Ed25519.Signature
  } deriving (Show, Eq)

instance Arbitrary Signature where
  arbitrary = sign <$> arbitrary <*> (encodeUtf8 @String <$> arbitrary)

-- | b58check of a public key.
newtype KeyHash = KeyHash
  { unKeyHash :: ByteString
  } deriving (Show, Eq, Ord)

instance Arbitrary KeyHash where
  arbitrary = hashKey <$> arbitrary

----------------------------------------------------------------------------
-- Magic bytes
--
-- These magic bytes were deduced empirically by taking sample data
-- (public and secret keys, signatures, etc.), decoding it and
-- noticing that all of them have the same start bytes.
-- Tests prove that they are valid (if they were invalid, parse* functions
-- would return some crypto primitive for which underlying crypto would
-- work differently).
----------------------------------------------------------------------------

publicKeyTag :: ByteString
publicKeyTag = "\13\15\37\217"

secretKeyTag :: ByteString
secretKeyTag = "\13\15\58\7"

signatureTag :: ByteString
signatureTag = "\9\245\205\134\18"

keyHashTag :: ByteString
keyHashTag = "\6\161\159"

----------------------------------------------------------------------------
-- Formatting
----------------------------------------------------------------------------

-- | Error that can happen during parsing of cryptographic primitive types.
data CryptoParseError
  = CryptoParseWrongBase58Check
  | CryptoParseWrongTag !ByteString
  | CryptoParseCryptoError CryptoError
  deriving (Show, Eq)

instance Buildable.Buildable CryptoParseError where
  build =
    \case
      CryptoParseWrongBase58Check -> "Wrong base58check encoding of bytes"
      CryptoParseWrongTag tag -> "Prefix is wrong tag: " <> fmt (hexF tag)
      CryptoParseCryptoError err ->
        "Cryptographic library reported an error: " <>
        Buildable.build (displayException err)

formatPublicKey :: PublicKey -> Text
formatPublicKey = formatImpl publicKeyTag . unPublicKey

mformatPublicKey :: PublicKey -> MText
mformatPublicKey = mkMTextUnsafe . formatPublicKey

instance Buildable.Buildable PublicKey where
  build = Buildable.build . formatPublicKey

parsePublicKey :: Text -> Either CryptoParseError PublicKey
parsePublicKey = parseImpl publicKeyTag Ed25519.publicKey

mkPublicKey :: ByteArrayAccess ba => ba -> Either Text PublicKey
mkPublicKey ba =
  case Ed25519.publicKey ba of
    CryptoPassed k -> Right (PublicKey k)
    CryptoFailed e -> Left (show e)

formatSecretKey :: SecretKey -> Text
formatSecretKey = formatImpl secretKeyTag . unSecretKey

instance Buildable.Buildable SecretKey where
  build = Buildable.build . formatSecretKey

parseSecretKey :: Text -> Either CryptoParseError SecretKey
parseSecretKey = parseImpl secretKeyTag Ed25519.secretKey

formatSignature :: Signature -> Text
formatSignature = formatImpl signatureTag . unSignature

mformatSignature :: Signature -> MText
mformatSignature = mkMTextUnsafe . formatSignature

instance Buildable.Buildable Signature where
  build = Buildable.build . formatSignature

parseSignature :: Text -> Either CryptoParseError Signature
parseSignature = parseImpl signatureTag Ed25519.signature

mkSignature :: ByteArrayAccess ba => ba -> Either Text Signature
mkSignature ba =
  case Ed25519.signature ba of
    CryptoPassed s -> Right (Signature s)
    CryptoFailed e -> Left (show e)

formatKeyHash :: KeyHash -> Text
formatKeyHash = formatImpl keyHashTag . unKeyHash

mformatKeyHash :: KeyHash -> MText
mformatKeyHash = mkMTextUnsafe . formatKeyHash

instance Buildable.Buildable KeyHash where
  build = Buildable.build . formatKeyHash

parseKeyHash :: Text -> Either CryptoParseError KeyHash
parseKeyHash = parseImpl keyHashTag pure

formatImpl :: ByteArray.ByteArrayAccess x => ByteString -> x -> Text
formatImpl tag = encodeBase58Check . mappend tag . ByteArray.convert

parseImpl
  :: Coercible x res
  => ByteString
  -> (ByteString -> CryptoFailable x)
  -> Text
  -> Either CryptoParseError res
parseImpl expectedTag constructor text = do
  let convertErr :: B58CheckWithPrefixError -> CryptoParseError
      convertErr =
        \case B58CheckWithPrefixWrongPrefix prefix -> CryptoParseWrongTag prefix
              B58CheckWithPrefixWrongEncoding -> CryptoParseWrongBase58Check
  payload <- first convertErr $ decodeBase58CheckWithPrefix expectedTag text
  bimap CryptoParseCryptoError coerce $
    eitherCryptoError $ constructor payload

----------------------------------------------------------------------------
-- JSON encoding/decoding
----------------------------------------------------------------------------

instance ToJSON PublicKey where
  toJSON = Aeson.String . formatPublicKey
  toEncoding = Aeson.text . formatPublicKey

instance FromJSON PublicKey where
  parseJSON =
    Aeson.withText "PublicKey" $
    either (fail . pretty) pure . parsePublicKey

instance ToJSON Signature where
  toJSON = Aeson.String . formatSignature
  toEncoding = Aeson.text . formatSignature

instance FromJSON Signature where
  parseJSON =
    Aeson.withText "Signature" $
    either (fail . pretty) pure . parseSignature

instance ToJSON KeyHash where
  toJSON = Aeson.String . formatKeyHash
  toEncoding = Aeson.text . formatKeyHash

instance FromJSON KeyHash where
  parseJSON =
    Aeson.withText "KeyHash" $
    either (fail . pretty) pure . parseKeyHash

----------------------------------------------------------------------------
-- Signing
----------------------------------------------------------------------------

-- | Sign a message using the secret key.
sign :: SecretKey -> ByteString -> Signature
sign sk =
  Signature .
  Ed25519.sign (unSecretKey sk) (unPublicKey (toPublic sk)) . blake2b

-- | Check that a sequence of bytes has been signed with a given key.
checkSignature :: PublicKey -> Signature -> ByteString -> Bool
checkSignature (PublicKey pk) (Signature sig) bytes =
  Ed25519.verify pk (blake2b bytes) sig

----------------------------------------------------------------------------
-- Hashing
----------------------------------------------------------------------------

-- | Compute the b58check of a public key hash.
hashKey :: PublicKey -> KeyHash
hashKey (PublicKey pk) =
  KeyHash (fromDigest @Blake2b_160 $ hash @ByteString $ ByteArray.convert pk)

-- | Compute a cryptographic hash of a bytestring using the
-- Blake2b_256 cryptographic hash function. It's used by the BLAKE2B
-- instruction in Michelson.
blake2b :: ByteString -> ByteString
blake2b = fromDigest @Blake2b_256 . hash

-- | Compute a cryptographic hash of a bytestring using the
-- Blake2b_160 cryptographic hash function.
blake2b160 :: ByteString -> ByteString
blake2b160 = fromDigest @Blake2b_160 . hash

-- | Compute a cryptographic hash of a bytestring using the
-- Sha256 cryptographic hash function.
sha256 :: ByteString -> ByteString
sha256 = fromDigest @SHA256 . hash

-- | Compute a cryptographic hash of a bytestring using the
-- Sha512 cryptographic hash function.
sha512 :: ByteString -> ByteString
sha512 = fromDigest @SHA512 . hash

fromDigest :: forall a. Digest a -> ByteString
fromDigest = ByteArray.convert

----------------------------------------------------------------------------
-- Utility functions
----------------------------------------------------------------------------

-- | Encode a bytestring in Base58Check format.
encodeBase58Check :: ByteString -> Text
encodeBase58Check =
  decodeUtf8 . Base58.encodeBase58 Base58.bitcoinAlphabet . withCheckSum
  where
    withCheckSum :: ByteString -> ByteString
    withCheckSum bs = bs <> checkSum bs

-- | Decode a bytestring from Base58Check format.
decodeBase58Check :: Text -> Maybe ByteString
decodeBase58Check text = do
  bytes <- Base58.decodeBase58 Base58.bitcoinAlphabet (encodeUtf8 text)
  let (payload, chk) = BS.splitAt (length bytes - 4) bytes
  guard $ chk == checkSum payload
  return payload

checkSum :: ByteString -> ByteString
checkSum = BS.take 4 . (sha256 . sha256)

data B58CheckWithPrefixError
  = B58CheckWithPrefixWrongPrefix ByteString
  | B58CheckWithPrefixWrongEncoding
  deriving (Show)

-- | Parse a base58check encoded value expecting some prefix. If the
-- actual prefix matches the expected one, it's stripped of and the
-- resulting payload is returned.
decodeBase58CheckWithPrefix ::
     ByteString -> Text -> Either B58CheckWithPrefixError ByteString
decodeBase58CheckWithPrefix prefix text =
  case decodeBase58Check text of
    Nothing -> Left B58CheckWithPrefixWrongEncoding
    Just bs ->
      let (actualPrefix, payload) = BS.splitAt (length prefix) bs
       in if actualPrefix == prefix
          then Right payload
          else Left (B58CheckWithPrefixWrongPrefix actualPrefix)
