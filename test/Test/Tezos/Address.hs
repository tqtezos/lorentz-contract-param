-- | Tests for 'Tezos.Address'.

module Test.Tezos.Address
  ( spec_Address
  ) where

import Test.Hspec (Spec, describe, it, shouldSatisfy)

import Tezos.Address (Address, formatAddress, parseAddress)

import Test.Util.QuickCheck (ShowThroughBuild(..), aesonRoundtrip, roundtripSpecSTB)

spec_Address :: Spec
spec_Address = do
  describe "parseAddress" $ do
    it "Successfully parses valid sample data" $
      forM_ sampleAddresses (\a -> bimap STB STB (parseAddress a) `shouldSatisfy` isRight)
    it "Fails to parse invalid data" $ do
      forM_ invalidAddresses (\a -> bimap STB STB (parseAddress a) `shouldSatisfy` isLeft)
    describe "Formatting" $ do
      describe "Roundtrip (parse . format ≡ pure)" $ do
        roundtripSpecSTB formatAddress parseAddress
      describe "Roundtrip (JSON encoding/deconding)" $ do
        aesonRoundtrip @Address
  where
    sampleAddresses =
      [ "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"
      , "KT1G4hcQj2STN86GwC1XAkPtwPPhgfPKuE45"
      ]
    invalidAddresses =
      [ ""
      , "1"
      , "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUZU"
      , "KT1G4hcQj2STN86GwC1XAkPtwPPhgfPKuE46"
      ]
