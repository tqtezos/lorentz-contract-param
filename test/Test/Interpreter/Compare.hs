-- | Module, containing spec to test compare.tz contract.
module Test.Interpreter.Compare
  ( compareSpec
  ) where

import Test.Hspec (Spec, it, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, arbitrary, (===))
import Test.QuickCheck.Property (forAll, withMaxSuccess)

import Michelson.Interpret (InterpreterState, MichelsonFailed)
import Michelson.Typed (ToT, Val(..), fromVal, toVal)
import Morley.Test (contractProp, specWithTypedContract)
import Morley.Test.Util (failedProp)
import Morley.Types (MorleyLogs)
import Tezos.Core (Mutez, unsafeMkMutez)

import Test.Util.Interpreter (dummyContractEnv)

type Param = (Mutez, Mutez)
type ContractStorage instr = Val instr (ToT [Bool])
type ContractResult x instr
   = ( Either MichelsonFailed ([x], ContractStorage instr)
     , InterpreterState MorleyLogs)

-- | Spec to test compare.tz contract.
compareSpec :: Spec
compareSpec = parallel $ do

  specWithTypedContract "contracts/compare.tz" $ \contract -> do
    it "success test" $
      contractProp' contract (unsafeMkMutez 10, unsafeMkMutez 11)

    prop "Random check"
      $ withMaxSuccess 200
      $ forAll ((,) <$> arbitrary <*> arbitrary)
      $ contractProp' contract


  where
    mkStorage :: ContractStorage instr
    mkStorage = VList []

    mkExpected :: Param -> [Bool]
    mkExpected (a, b) = [a == b, a > b, a < b, a >= b, a <= b]

    validate
      :: [Bool]
      -> ContractResult x instr
      -> Property
    validate e (Right ([], fromVal -> l), _) = l === e
    validate _ (Left _, _) = failedProp "Unexpected fail of sctipt."
    validate _ _ = failedProp "Invalid result got."

    contractProp' contract inputs =
      contractProp contract
        (\_ _ _ -> validate (mkExpected inputs))
        dummyContractEnv
        (toVal inputs)
        mkStorage
