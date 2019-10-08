-- | Module, containing spec to test NatStorageContract.tz contract.
module Test.Lorentz.Contracts.VarStorage
  ( test_NatStorageContract
  ) where

import Test.Hspec.Expectations (shouldSatisfy)
import Test.QuickCheck (Property, arbitrary, choose, counterexample, (.&&.), (===))
import Test.QuickCheck.Property (expectFailure, forAll, withMaxSuccess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)

import Lorentz (compileLorentz)
import Lorentz.Contracts.VarStorage (varStorageContract)
import Michelson.Interpret (ContractEnv(..))
import Michelson.Test
  (ContractPropValidator, concatTestTrees, contractProp, midTimestamp, testTreesWithTypedContract)
import Michelson.Test.Dummy
import Michelson.Test.Util (failedProp)
import Michelson.Typed (CValue(..), Operation'(..), ToT, TransferTokens(..))
import qualified Michelson.Typed as T
import Tezos.Address (Address(..))
import Tezos.Core (Mutez, Timestamp, timestampPlusSeconds, unMutez, unsafeMkMutez, unsafeSubMutez)
import Tezos.Crypto (KeyHash)
import Util.Test.Arbitrary (runGen)

test_NatStorageContract :: IO [TestTree]
test_NatStorageContract = concatTestTrees
  [ one . testGroup "Michelson version" <$>
    testTreesWithTypedContract "contracts/NatStorageContract.tz" natStorageTest
  , one . testGroup "Lorentz version" <$>
    natStorageTest (compileLorentz (varStorageContract @Natural))
  ]
  where
    natStorageTest contract =
      pure
      [ testProperty "Submitted value stored" $ withMaxSuccess 200 $
          qcProp contract arbitrary arbitrary
      ]

    qcProp contract paramGen storGen =
      forAll (liftM2 (,) paramGen storGen) $ \(p, s) ->
        let validate = validateNatStorage env p s
         in contractProp contract validate env p s

    env = dummyContractEnv
            { ceNow = midTimestamp
            , ceAmount = unsafeMkMutez midAmount
            }
    midAmount = unMutez (maxBound `unsafeSubMutez` minBound) `div` 2

-- | Assert that new storage value is given parameter
validateNatStorage :: ContractEnv -> Natural -> Natural -> ContractPropValidator (ToT Natural) Property
validateNatStorage _ newNat storedNat (Left err, _) =
  failedProp $ "Unexpected script failure: " <> show (newNat, storedNat, err)
validateNatStorage _ newNat _ (Right (_, res), _) =
  T.toVal newNat === res

