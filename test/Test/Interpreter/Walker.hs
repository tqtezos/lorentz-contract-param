-- | Module, containing spec to walker contract.

module Test.Interpreter.Walker
  ( walkerSpec
  ) where

import Fmt ((+|), (|+))
import Test.Hspec (Expectation, Spec, expectationFailure, it, shouldBe)

import Lorentz (compileLorentz)
import Michelson.Test (ContractPropValidator, contractRepeatedProp)
import Michelson.Test.Dummy
import qualified Michelson.Typed as T
import Util.Named ((.!))

import Test.Lorentz.Contracts.Walker

-- | Spec to test walker contract.
walkerSpec :: Spec
walkerSpec = do
  it "Go right" $
    walkerProp [GoRight]
      Storage{ pos = Position 1 0, power = 0 }

  it "Go left" $
    walkerProp [GoLeft]
      Storage{ pos = Position (-1) 0, power = 0 }

  it "Walk a lot" $
    let commands = mconcat
          [ replicate 1 GoUp
          , replicate 2 GoLeft
          , one $ Boost (#coef1 .! 5, #coef2 .! 0)
          , replicate 3 GoDown
          , replicate 4 GoRight
          , one $ Boost (#coef1 .! 3, #coef2 .! 999)
          ]
    in walkerProp commands
      Storage{ pos = Position 2 (-2), power = 8 }

walkerProp :: [Parameter] -> Storage -> Expectation
walkerProp params (T.toVal -> expected) =
  contractRepeatedProp
    (compileLorentz walkerContract)
    (validateFinishedWith ([], expected))
    dummyContractEnv params Storage{ pos = Position 0 0, power = 0 }

validateFinishedWith
  :: ([T.Operation], T.Value st)
  -> ContractPropValidator st Expectation
validateFinishedWith expected (res, _) = case res of
  Left err -> expectationFailure $ "Interpretation failed: " +| err |+ ""
  Right got -> got `shouldBe` expected
