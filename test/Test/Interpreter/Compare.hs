-- | Module, containing spec to test compare.tz contract.
module Test.Interpreter.Compare
  ( test_compare
  ) where

import Test.QuickCheck (Property, (===))
import Test.QuickCheck.Property (withMaxSuccess)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

import Michelson.Interpret (InterpreterState, MichelsonFailed)
import Michelson.Test (contractProp, testTreesWithTypedContract)
import Michelson.Test.Dummy
import Michelson.Test.Util (failedProp)
import Michelson.Typed (ToT, fromVal)
import qualified Michelson.Typed as T
import Tezos.Core (Mutez, unsafeMkMutez)

type Param = (Mutez, Mutez)
type ContractStorage = T.Value (ToT [Bool])
type ContractResult x
   = ( Either MichelsonFailed ([x], ContractStorage)
     , InterpreterState)

-- | Spec to test compare.tz contract.
test_compare :: IO [TestTree]
test_compare =
  testTreesWithTypedContract "contracts/tezos_examples/compare.tz" $ \contract ->
    let
      contractProp' inputParam =
        contractProp contract (validate (mkExpected inputParam))
        dummyContractEnv inputParam initStorage
    in pure
    [ testProperty "success test" $
        contractProp' (unsafeMkMutez 10, unsafeMkMutez 11)
    , testProperty "Random check" $
        withMaxSuccess 200 contractProp'
    ]
  where
    initStorage :: [Bool]
    initStorage = []

    mkExpected :: Param -> [Bool]
    mkExpected (a, b) = [a == b, a > b, a < b, a >= b, a <= b]

    validate
      :: [Bool]
      -> ContractResult x
      -> Property
    validate e (Right ([], fromVal -> l), _) = l === e
    validate _ (Left _, _) = failedProp "Unexpected fail of sctipt."
    validate _ _ = failedProp "Invalid result got."
