-- | Testing of toCVal / fromCVal conversions

module Test.CValConversion
  ( spec
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)

import Michelson.Typed (CValue(..), fromCVal, toCVal)

-- | Spec to test toCVal / fromCVal conversions.
spec :: Spec
spec = do
  describe "ToCVal / FromCVal tests" $ do
    it "ToCVal / FromCVal manual tests" $ do
      toCVal @Int 10 `shouldBe` CvInt 10
      toCVal @Integer 10 `shouldBe` CvInt 10
      toCVal @Integer (-10) `shouldBe` CvInt (-10)
      toCVal @Word64 10 `shouldBe` CvNat 10
      toCVal @Natural 10 `shouldBe` CvNat 10
      toCVal @Text "abc" `shouldBe` CvString "abc"
      toCVal True `shouldBe` CvBool True
      fromCVal (CvInt 10) `shouldBe` (10 :: Integer)
      fromCVal (CvString "abc") `shouldBe` ("abc" :: Text)
      fromCVal (CvBool True) `shouldBe` True

    describe "ToCVal / FromCVal property tests" $ do
      prop "ToCVal / FromCVal: Integer"
        $ \v -> fromCVal (toCVal @Integer v) == v
      prop "ToCVal / FromCVal: Bool"
        $ \v -> fromCVal (toCVal @Bool v) == v
