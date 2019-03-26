-- | Module, containing spec to test compare.tz contract.
module Test.Interpreter.Compare
  ( compareSpec
  ) where

import Test.Hspec (Spec, it, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, (===))
import Test.QuickCheck.Property (withMaxSuccess)

import Michelson.Interpret (InterpreterState, MichelsonFailed)
import Michelson.Typed (ToT, Val(..), fromVal)
import Morley.Test (contractProp, specWithTypedContract)
import Morley.Test.Dummy
import Morley.Test.Util (failedProp)
import Morley.Types (MorleyLogs)
import Tezos.Core (Mutez, unsafeMkMutez)

type Param = (Mutez, Mutez)
type ContractStorage instr = Val instr (ToT [Bool])
type ContractResult x instr
   = ( Either MichelsonFailed ([x], ContractStorage instr)
     , InterpreterState MorleyLogs)

-- | Spec to test compare.tz contract.
compareSpec :: Spec
compareSpec = parallel $ do

  specWithTypedContract "contracts/compare.tz" $ \contract -> do
    let
      contractProp' inputParam =
        contractProp contract (validate (mkExpected inputParam))
        dummyContractEnv inputParam initStorage

    it "success test" $
      contractProp' (unsafeMkMutez 10, unsafeMkMutez 11)

    prop "Random check" $ withMaxSuccess 200 contractProp'
  where
    initStorage :: [Bool]
    initStorage = []

    mkExpected :: Param -> [Bool]
    mkExpected (a, b) = [a == b, a > b, a < b, a >= b, a <= b]

    validate
      :: [Bool]
      -> ContractResult x instr
      -> Property
    validate e (Right ([], fromVal -> l), _) = l === e
    validate _ (Left _, _) = failedProp "Unexpected fail of sctipt."
    validate _ _ = failedProp "Invalid result got."
