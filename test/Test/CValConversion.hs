-- | Testing of toCVal / fromCVal conversions

module Test.CValConversion
  ( unit_toCVal
  , unit_fromCVal
  , test_Roundtrip
  ) where

import Test.Hspec.Expectations (Expectation, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty, (===))

import Michelson.Typed (CValue(..), fromCVal, toCVal)

unit_toCVal :: Expectation
unit_toCVal = do
  toCVal @Integer 10 `shouldBe` CvInt 10
  toCVal @Integer (-10) `shouldBe` CvInt (-10)
  toCVal @Natural 10 `shouldBe` CvNat 10
  toCVal @Text "abc" `shouldBe` CvString "abc"
  toCVal True `shouldBe` CvBool True

unit_fromCVal :: Expectation
unit_fromCVal = do
  fromCVal (CvInt 10) `shouldBe` (10 :: Integer)
  fromCVal (CvString "abc") `shouldBe` ("abc" :: Text)
  fromCVal (CvBool True) `shouldBe` True

test_Roundtrip :: [TestTree]
test_Roundtrip =
  [ testProperty "Integer" $ \v -> fromCVal (toCVal @Integer v) === v
  , testProperty "Bool" $ \v -> fromCVal (toCVal @Bool v) === v
  ]
