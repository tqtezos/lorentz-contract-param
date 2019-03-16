-- | Tests for 'Tezos.Crypto'.

module Test.Tezos.Crypto
  ( spec
  ) where

import Fmt (fmt, hexF, pretty)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe, shouldSatisfy)

import Tezos.Crypto

import Test.Util.QuickCheck (aesonRoundtrip, roundtripSpecSTB)

spec :: Spec
spec = describe "Tezos.Crypto" $ do
  describe "Signing" $ do
    describe "Formatting" $ do
      describe "parsePublicKey" $ do
        it "Successfully parses valid sample data" $
          mapM_ (parsePublicKeySample . sdPublicKey) sampleSignatures
        it "Fails to parse invalid data" $ do
          parsePublicKeyInvalid "aaa"
          parsePublicKeyInvalid
            "edpkuwTWKgQNnhR5v17H2DYHbfcxYepARyrPGbf1tbMoGQAj8Ljr3v"
          parsePublicKeyInvalid
            "edsigtrs8bK7vNfiR4Kd9dWasVa1bAWaQSu2ipnmLGZuwQa8ktCEMYVKqbWsbJ7zTS8dgYT9tiSUKorWCPFHosL5zPsiDwBQ6vb"
      describe "parseSignature" $ do
        it "Successfully parses valid sample data" $
          mapM_ (parseSignatureSample . sdSignature) sampleSignatures
        it "Fails to parse invalid data" $ do
          parseSignatureInvalid "bbb"
          parseSignatureInvalid
            "edpkuwTWKgQNnhR5v17H2DYHbfcxYepARyrPGbf1tbMoGQAj8Ljr3V"
          parseSignatureInvalid
            "edsigtrs8bK7vNfiR4Kd9dWasVa1bAWaQSu2ipnmLGZuwQa8ktCEMYVKqbWsbJ7zTS8dgYT9tiSUKorWCPFHosL5zPsiDwBQ6vB"
      describe "Roundtrip (parse . format ≡ pure)" $ do
        roundtripSpecSTB formatPublicKey parsePublicKey
        roundtripSpecSTB formatSecretKey parseSecretKey
        roundtripSpecSTB formatSignature parseSignature
        roundtripSpecSTB formatKeyHash parseKeyHash
      describe "Roundtrip (JSON encoding/deconding)" $ do
        aesonRoundtrip @PublicKey
        aesonRoundtrip @Signature
        aesonRoundtrip @KeyHash
    describe "checkSignature" $ do
      it "Works on sample data" $ mapM_ checkSignatureSample sampleSignatures
  describe "Bytes hashing" $ do
    hashingSpec "blake2b" blake2b blake2bHashes
    hashingSpec "sha256" sha256 sha256Hashes
    hashingSpec "sha512" sha512 sha512Hashes
  describe "Key hashing" $
    it "Works on sample data" $ mapM_ hashKeySample sampleKeyHashes

----------------------------------------------------------------------------
-- Signing
----------------------------------------------------------------------------

data SignatureData = SignatureData
  { sdPublicKey :: !Text
  , sdBytes :: !ByteString
  , sdSignature :: !Text
  , sdValid :: !Bool
  }

-- These signatures have been produced by `tezos-client`.
sampleSignatures :: [SignatureData]
sampleSignatures =
  [ SignatureData
    { sdPublicKey = "edpkuwTWKgQNnhR5v17H2DYHbfcxYepARyrPGbf1tbMoGQAj8Ljr3V"
    , sdBytes = "\0"
    , sdSignature = "edsigtrs8bK7vNfiR4Kd9dWasVa1bAWaQSu2ipnmLGZuwQa8ktCEMYVKqbWsbJ7zTS8dgYT9tiSUKorWCPFHosL5zPsiDwBQ6vb"
    , sdValid = True
    }
  , SignatureData
    { sdPublicKey = "edpkupH22qrz1sNQt5HSvWfRJFfyJ9dhNbZLptE6GR4JbMoBcACZZH"
    , sdBytes = "\0\0"
    , sdSignature = "edsigtj8LhbJ2B3qhZvqzA49raG65dydFcWZW9b9L7ntF3bb29zxaBFFL8SM1jeBUY66hG122znyVA4wpzLdwxcNZwSK3Szu7iD"
    , sdValid = True
    }
  , SignatureData
    { sdPublicKey = "edpkupH22qrz1sNQt5HSvWfRJFfyJ9dhNbZLptE6GR4JbMoBcACZZH"
    , sdBytes = "kot"
    , sdSignature = "edsigtrs8bK7vNfiR4Kd9dWasVa1bAWaQSu2ipnmLGZuwQa8ktCEMYVKqbWsbJ7zTS8dgYT9tiSUKorWCPFHosL5zPsiDwBQ6vb"
    , sdValid = False
    }
  ]

parsePublicKeySample :: Text -> Expectation
parsePublicKeySample publicKeyText =
  parsePublicKey publicKeyText `shouldSatisfy` isRight

parsePublicKeyInvalid :: Text -> Expectation
parsePublicKeyInvalid invalidPublicKeyText =
  parsePublicKey invalidPublicKeyText `shouldSatisfy` isLeft

parseSignatureSample :: Text -> Expectation
parseSignatureSample publicKeyText =
  parseSignature publicKeyText `shouldSatisfy` isRight

parseSignatureInvalid :: Text -> Expectation
parseSignatureInvalid invalidSignatureText =
  parseSignature invalidSignatureText `shouldSatisfy` isLeft

checkSignatureSample :: SignatureData -> Expectation
checkSignatureSample sd =
  checkSignature publicKey signature (sdBytes sd) `shouldBe` sdValid sd
  where
    publicKey = partialParse parsePublicKey (sdPublicKey sd)
    signature = partialParse parseSignature (sdSignature sd)

----------------------------------------------------------------------------
-- Hashing
----------------------------------------------------------------------------

-- These values have been computed using the following contract:
{-
parameter string;
storage bytes;
code { CDR; SHA512; # replace with desired function
       NIL operation; PAIR;};

-}

blake2bHashes, sha256Hashes, sha512Hashes :: [(ByteString, Text)]
blake2bHashes =
  [ ("\0", "03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314")  -- 0x00
  , ("\0\0", "9ee6dfb61a2fb903df487c401663825643bb825d41695e63df8af6162ab145a6")  -- 0x0000
  ]
sha256Hashes =
  [ ("\0", "6e340b9cffb37a989ca544e6bb780a2c78901d3fb33738768511a30617afa01d")  -- 0x00
  , ("\0\0", "96a296d224f285c67bee93c30f8a309157f0daa35dc5b87e410b78630a09cfc7")  -- 0x0000
  ]
sha512Hashes =
  [ ("\0", "b8244d028981d693af7b456af8efa4cad63d282e19ff14942c246e50d9351d22704a802a71c3580b6370de4ceb293c324a8423342557d4e5c38438f0e36910ee")  -- 0x00
  , ("#", "d369286ac86b60fa920f6464d26becacd9f4c8bd885b783407cdcaa74fafd45a8b56b364b63f6256c3ceef26278a1c7799d4243a8149b5ede5ce1d890b5c7236")  -- 0x23
  ]

hashingSpec :: String -> (ByteString -> ByteString) -> [(ByteString, Text)] -> Spec
hashingSpec funcName hashFunc pairs = do
  describe funcName $ do
    forM_ pairs $ \(bs, bsHashHex) -> do
      it ("correctly computes hash of 0x" <> fmt (hexF bs)) $
        fmt (hexF (hashFunc bs)) `shouldBe` bsHashHex

----------------------------------------------------------------------------
-- Key hashing
----------------------------------------------------------------------------

sampleKeyHashes :: [(Text, Text)]
sampleKeyHashes =
  [ ("edpkupH22qrz1sNQt5HSvWfRJFfyJ9dhNbZLptE6GR4JbMoBcACZZH"
    , "tz1NaZzLvdDBLfV2LWC6F4SJfNV2jHdZJXkJ"
    )
  , ("edpkuwTWKgQNnhR5v17H2DYHbfcxYepARyrPGbf1tbMoGQAj8Ljr3V"
    , "tz1Yz3VPaCNB5FjhdEVnSoN8Xv3ZM8g2LYhw"
    )
  ]

hashKeySample :: (Text, Text) -> Expectation
hashKeySample (pkText, keyHashText) = hashKey pk `shouldBe` keyHash
  where
    pk = partialParse parsePublicKey pkText
    keyHash = partialParse parseKeyHash keyHashText

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

-- If passed textual data is invalid, it's a programmer mistake.
partialParse :: (Text -> Either CryptoParseError a) -> Text -> a
partialParse parse = either (error . pretty) id . parse
