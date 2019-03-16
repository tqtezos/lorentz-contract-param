-- | Module, containing spec to test conditionals.tz contract.
module Test.Interpreter.Conditionals
  ( conditionalsSpec
  ) where

import Test.Hspec (Spec, it, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, arbitrary, (===))
import Test.QuickCheck.Property (forAll, withMaxSuccess)

import Michelson.Interpret (MichelsonFailed)
import Michelson.Typed (CVal(..), ToT, Val(..), toVal)
import Morley.Test (contractProp, specWithContract)
import Test.Util.Interpreter (dummyContractEnv)
import Test.Util.QuickCheck (failedProp, qcIsLeft, qcIsRight)

type Param = Either Text (Maybe Integer)
type ContractParam instr = Val instr (ToT Param)
type ContractStorage instr = Val instr (ToT Text)
type ContractResult x instr = Either MichelsonFailed ([x], ContractStorage instr)

-- | Spec to test conditionals.tz contract.
conditionalsSpec :: Spec
conditionalsSpec = parallel $ do

  specWithContract "contracts/conditionals.tz" $ \contract -> do
    it "success 1 test" $
      contractProp' contract $ Left "abc"

    prop "Random check"
      $ withMaxSuccess 200
      $ forAll arbitrary
      $ \(inputs' :: Either String (Maybe Integer)) ->
          contractProp' contract (first toText inputs')

  where
    mkStorage :: ContractStorage instr
    mkStorage = toVal @Text "storage"

    mkParam :: Param -> ContractParam instr
    mkParam = toVal

    validate
      :: Show x
      => Param
      -> ContractResult x instr
      -> Property
    validate (Left a) (Right ([], VC (CvString b))) = a === b
    validate (Right Nothing) r = qcIsLeft r
    validate (Right (Just a)) r | a < 0 = qcIsLeft r
    validate (Right (Just a)) r | a >= 0 = qcIsRight r
    validate _ res = failedProp $ "Unexpected result: " <> show res

    contractProp' contract inputs =
      contractProp contract
        (\_ _ _ -> validate inputs)
        dummyContractEnv
        (mkParam inputs)
        mkStorage
