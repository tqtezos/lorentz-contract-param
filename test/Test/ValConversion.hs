-- | Testing of toVal / fromVal conversions

module Test.ValConversion
  ( spec_ValConversion
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Arbitrary)

import Michelson.Typed (CValue(..), IsoValue(..), ToT, Value, Value'(..))

import Test.Util.QuickCheck (roundtripSpec)

-- | Spec to test toVal / fromVal conversions.
spec_ValConversion :: Spec
spec_ValConversion = do
  describe "ToVal / FromVal tests" $ do
    it "ToVal / FromVal manual tests" $ do
      check () $ (\case VUnit -> True;)
      check (10 :: Integer) $ (\case (VC (CvInt 10)) -> True; _ -> False)
      check ("abc" :: Text) $ (\case (VC (CvString "abc")) -> True; _ -> False)
      check (Just "abc" :: Maybe Text)
          $ (\case (VOption (Just (VC (CvString "abc")))) -> True; _ -> False)
      check (Left "abc" :: Either Text Text)
          $ (\case (VOr (Left (VC (CvString "abc")))) -> True; _ -> False)
      check (Left "abc" :: Either Text Integer)
          $ (\case (VOr (Left (VC (CvString "abc")))) -> True; _ -> False)
      check ((10, "abc") :: (Integer, Text))
          $ (\case (VPair (VC (CvInt 10), VC (CvString "abc"))) -> True; _ -> False)
      check (["abc", "cde"] :: [Text])
          $ (\case (VList [ VC (CvString "abc")
                          , VC (CvString "cde")]) -> True; _ -> False)

    describe "ToVal / FromVal property tests" $ do
      roundtrip @Integer
      roundtrip @Bool
      roundtrip @[Bool]
      roundtrip @(Maybe Integer)
      roundtrip @(Maybe (Maybe Integer))
      roundtrip @(Either Bool Integer)
      roundtrip @(Set Integer)
      roundtrip @(Set Integer)
      roundtrip @(Set Bool)
      roundtrip @(Map Integer Integer)
      roundtrip @(Map Integer Bool)
      roundtrip @(Map Integer (Maybe (Either Bool Bool)))
  where
    check :: IsoValue a => a -> (Value (ToT a) -> Bool) -> IO ()
    check v p = p (toVal v) `shouldBe` True

    roundtrip :: forall a.
      (Show a, Eq a, Arbitrary a, Typeable a, IsoValue a) => Spec
    roundtrip = roundtripSpec @a @_ @Void toVal (Right . fromVal)
