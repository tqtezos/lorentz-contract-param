-- | Tests for 'Tezos.Address'.

module Test.Tezos.Address
  ( test_Roundtrip
  , test_parseAddress
  ) where

import Test.Hspec (shouldSatisfy)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Tezos.Address (Address, formatAddress, parseAddress)

import Test.Util.QuickCheck (ShowThroughBuild(..), aesonRoundtrip, roundtripTestSTB)

test_Roundtrip :: [TestTree]
test_Roundtrip =
    [ testGroup "parse . format ≡ pure"
      [ roundtripTestSTB formatAddress parseAddress ]
    , testGroup "JSON encoding/deconding"
      [ aesonRoundtrip @Address ]
    ]

test_parseAddress :: [TestTree]
test_parseAddress =
  [ testCase "Successfully parses valid sample data" $
    forM_ sampleAddresses (\a -> bimap STB STB (parseAddress a) `shouldSatisfy` isRight)
  , testCase "Fails to parse invalid data" $ do
    forM_ invalidAddresses (\a -> bimap STB STB (parseAddress a) `shouldSatisfy` isLeft)
  ]
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
