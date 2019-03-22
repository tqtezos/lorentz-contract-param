module Test.Serialization.Aeson
  ( spec
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Test.Aeson.GenericSpecs (roundtripADTSpecs, roundtripSpecs)
import Test.Hspec (Spec)
import Test.QuickCheck (Arbitrary)

import Michelson.Untyped (Contract, Elt, FieldAnn, InstrAbstract, Op, TypeAnn, Value, VarAnn)
import Tezos.Core (Mutez, Timestamp)

import Test.Arbitrary ()
import Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary)

-- Note: if we want to enforce a particular JSON format, we can extend
-- these test with golden tests (it's easy with `hspec-golden-aeson`).

-- For types with one constructor and/or without 'ToADTArbitrary' instance.
test :: forall a.
  (Arbitrary a, ToJSON a, FromJSON a, Typeable a)
  => Proxy a
  -> Spec
test = roundtripSpecs

-- For types with 'ToADTArbitrary' instance.
testADT :: forall a.
  (Show a, Eq a, Arbitrary a, ToADTArbitrary a, ToJSON a, FromJSON a)
  => Proxy a
  -> Spec
testADT = roundtripADTSpecs

spec :: Spec
spec = do
  -- Core Tezos types
  test (Proxy @Timestamp)
  test (Proxy @Mutez)

  -- Michelson types
  testADT (Proxy @Op)

  -- these are actually all the same thing (Annotation a),
  -- where a is a phantom type,
  -- but let's test them in case they
  -- ever change for some reason
  test (Proxy @TypeAnn)
  test (Proxy @FieldAnn)
  test (Proxy @VarAnn)

  test (Proxy @(Contract Op))
  testADT (Proxy @(InstrAbstract Op))
  test (Proxy @(Value Op))
  test (Proxy @(Elt Op))
