-- | Testing of toVal / fromVal conversions

module Test.ValConversion
  ( test_Roundtrip
  , unit_toVal
  ) where

import Test.HUnit (Assertion, (@?))
import Test.QuickCheck (Arbitrary)
import Test.Tasty (TestTree)

import Michelson.Typed (CValue(..), IsoValue(..), ToT, Value, Value'(..))

import Test.Util.QuickCheck (roundtripTest)

-- | TestTrees to test toVal / fromVal conversions (roundtrip)
test_Roundtrip :: [TestTree]
test_Roundtrip =
  [ roundtrip @Integer
  , roundtrip @Bool
  , roundtrip @[Bool]
  , roundtrip @(Maybe Integer)
  , roundtrip @(Maybe (Maybe Integer))
  , roundtrip @(Either Bool Integer)
  , roundtrip @(Set Integer)
  , roundtrip @(Set Integer)
  , roundtrip @(Set Bool)
  , roundtrip @(Map Integer Integer)
  , roundtrip @(Map Integer Bool)
  , roundtrip @(Map Integer (Maybe (Either Bool Bool)))
  ]
  where
    roundtrip :: forall a.
      (Show a, Eq a, Arbitrary a, Typeable a, IsoValue a) => TestTree
    roundtrip = roundtripTest @a @_ @Void toVal (Right . fromVal)

unit_toVal :: Assertion
unit_toVal = do
  check () $ (\case VUnit -> True;)
  check (10 :: Integer) $ (\case (VC (CvInt 10)) -> True; _ -> False)
  check ("abc" :: ByteString) $ (\case (VC (CvBytes "abc")) -> True; _ -> False)
  check (Just "abc" :: Maybe ByteString)
    $ (\case (VOption (Just (VC (CvBytes "abc")))) -> True; _ -> False)
  check (Left "abc" :: Either ByteString ByteString)
    $ (\case (VOr (Left (VC (CvBytes "abc")))) -> True; _ -> False)
  check (Left "abc" :: Either ByteString Integer)
    $ (\case (VOr (Left (VC (CvBytes "abc")))) -> True; _ -> False)
  check ((10, "abc") :: (Integer, ByteString))
    $ (\case (VPair (VC (CvInt 10), VC (CvBytes "abc"))) -> True; _ -> False)
  check (["abc", "cde"] :: [ByteString])
    $ (\case (VList [ VC (CvBytes "abc")
                    , VC (CvBytes "cde")]) -> True; _ -> False)
  where
    check :: IsoValue a => a -> (Value (ToT a) -> Bool) -> Assertion
    check v p = p (toVal v) @? "toVal returned unexpected result"
