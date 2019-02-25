module Test.Serialization.Aeson
  ( spec
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Test.Aeson.GenericSpecs
  (defaultSettings, roundtripAndGoldenSpecsWithSettings, goldenDirectoryOption,
   GoldenDirectoryOption(CustomDirectoryName))
import Test.Hspec (Spec)
import Test.QuickCheck (Arbitrary)

import Michelson.Types
  (Op, Timestamp, Mutez, Contract, InstrAbstract, Value, Elt,
   TypeAnn, FieldAnn, VarAnn)

import Test.Arbitrary ()


test :: forall a.
  (Arbitrary a, ToJSON a, FromJSON a, Typeable a)
  => Proxy a
  -> Spec
test _ =
  roundtripAndGoldenSpecsWithSettings
    (defaultSettings { goldenDirectoryOption = CustomDirectoryName "test/golden" })
    (Proxy @a)

spec :: Spec
spec = do
  -- Michelson types
  test (Proxy @Op)
  test (Proxy @Timestamp)
  test (Proxy @Mutez)

  -- these are actually all the same thing (Annotation a),
  -- where a is a phantom type,
  -- but let's test them in case they
  -- ever change for some reason
  test (Proxy @TypeAnn)
  test (Proxy @FieldAnn)
  test (Proxy @VarAnn)

  test (Proxy @(Contract Op))
  test (Proxy @(InstrAbstract Op))
  test (Proxy @(Value Op))
  test (Proxy @(Elt Op))
