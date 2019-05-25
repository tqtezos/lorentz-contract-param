module Test.Serialization.Aeson
  ( test_Roundtrip
  ) where

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Test.QuickCheck (Arbitrary)
import Test.Tasty (TestTree)

import Michelson.Untyped
  (Contract, Elt, ExpandedOp, FieldAnn, InstrAbstract, TypeAnn, Value, VarAnn)
import Tezos.Core (Mutez, Timestamp)
import Util.Test.Arbitrary ()

import Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary(..))
import Test.Util.QuickCheck (roundtripADTTest, roundtripTest)

-- Note: if we want to enforce a particular JSON format, we can extend
-- these test with golden tests (it's easy with `hspec-golden-aeson`).

-- For types with one constructor and/or without 'ToADTArbitrary' instance.
test
  :: forall a. (Eq a, Show a, Arbitrary a, ToJSON a, FromJSON a, Typeable a)
  => TestTree
test = roundtripTest @a encode eitherDecode

-- For types with 'ToADTArbitrary' instance.
testADT :: forall a.
  (Show a, Eq a, ToADTArbitrary a, ToJSON a, FromJSON a, Typeable a)
  => TestTree
testADT = roundtripADTTest @a encode eitherDecode

test_Roundtrip :: [TestTree]
test_Roundtrip =
  [ -- Core Tezos types
    test @Timestamp
  , test @Mutez

  -- Michelson types
  , testADT @ExpandedOp

  -- these are actually all the same thing (Annotation a),
  -- where a is a phantom type,
  -- but let's test them in case they
  -- ever change for some reason
  , test @TypeAnn
  , test @FieldAnn
  , test @VarAnn

  , test @Contract
  , testADT @(InstrAbstract ExpandedOp)
  , test @Value
  , test @(Elt ExpandedOp)
  ]
