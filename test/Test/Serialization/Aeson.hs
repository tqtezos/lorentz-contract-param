module Test.Serialization.Aeson
  ( spec
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Test.Aeson.GenericSpecs
  (GoldenDirectoryOption(CustomDirectoryName), defaultSettings, goldenDirectoryOption,
   roundtripAndGoldenSpecsWithSettings)
import Test.Hspec (Spec)
import Test.QuickCheck (Arbitrary)

import Michelson.Untyped (Contract, Elt, FieldAnn, InstrAbstract, Op, TypeAnn, Value, VarAnn)
import Tezos.Core (Mutez, Timestamp)

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
  -- Core Tezos types
  test (Proxy @Timestamp)
  test (Proxy @Mutez)

  -- Michelson types
  test (Proxy @Op)

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
