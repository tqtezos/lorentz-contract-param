-- | Module, containing spec to test conditionals.tz contract.
module Test.Interpreter.Conditionals
  ( conditionalsSpec
  ) where

import Test.Hspec (Spec, it, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, (===))
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Property (withMaxSuccess)

import Michelson.Interpret (InterpreterState, MichelsonFailed)
import Michelson.Test (contractProp, specWithTypedContract)
import Michelson.Test.Dummy (dummyContractEnv)
import Michelson.Test.Util (failedProp, qcIsLeft, qcIsRight)
import Michelson.Typed (CValue(..), ToT)
import Michelson.Text
import qualified Michelson.Typed as T

type Param = Either MText (Maybe Integer)
type ContractStorage = T.Value (ToT MText)
type ContractResult x
   = ( Either MichelsonFailed ([x], ContractStorage)
     , InterpreterState)

-- | Spec to test conditionals.tz contract.
conditionalsSpec :: Spec
conditionalsSpec = parallel $ do

  specWithTypedContract "contracts/tezos_examples/conditionals.tz" $ \contract -> do
    let
      contractProp' inputParam =
        contractProp contract (validate inputParam) dummyContractEnv inputParam
        [mt|storage|]

    it "success 1 test" $
      contractProp' $ Left [mt|abc|]

    prop "Random check" $ withMaxSuccess 200 contractProp'
  where
    validate
      :: Show x
      => Param
      -> ContractResult x
      -> Property
    validate (Left a) (Right ([], T.VC (CvString b)), _) = a === b
    validate (Right Nothing) r = qcIsLeft $ fst r
    validate (Right (Just a)) r
      | a < 0 = qcIsLeft $ fst r
      | otherwise = qcIsRight $ fst r
    validate _ res = failedProp $ "Unexpected result: " <> show res
