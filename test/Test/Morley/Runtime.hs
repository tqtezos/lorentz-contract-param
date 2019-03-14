-- | Tests for Morley.Runtime.

module Test.Morley.Runtime
  ( spec
  ) where

import Control.Lens (at)
import Fmt (pretty)
import Test.Hspec
  (Expectation, Spec, context, describe, expectationFailure, it, parallel, shouldBe, shouldSatisfy,
  specify)

import Michelson.Interpret (ContractEnv(..), InterpretUntypedError(..), InterpretUntypedResult(..))
import Michelson.Typed (unsafeValToValue)
import Michelson.Untyped
import Morley.Ext (interpretMorleyUntyped)
import Morley.Runtime
import Morley.Runtime.GState (GState(..), initGState)
import Morley.Types (MorleyLogs)
import Test.Util.Interpreter (ContractAux(..), dummyContractEnv, dummyMaxSteps, dummyNow)
import Tezos.Address (Address(..))
import Tezos.Core (unsafeMkMutez)

import Test.Util.Interpreter (dummyOrigination)

spec :: Spec
spec = describe "Morley.Runtime" $ do
  -- Safe to run in parallel, because 'interpreterPure' is pure.
  describe "interpreterPure" $ parallel $ do
    context "Updates storage value of executed contract" $ do
      specify "contract1" $ updatesStorageValue contractAux1
      specify "contract2" $ updatesStorageValue contractAux2
    it "Fails to originate an already originated contract" failsToOriginateTwice

----------------------------------------------------------------------------
-- Test code
----------------------------------------------------------------------------

data UnexpectedFailed =
  UnexpectedFailed (InterpretUntypedError MorleyLogs)
  deriving (Show)

instance Exception UnexpectedFailed

updatesStorageValue :: ContractAux -> Expectation
updatesStorageValue ca = either throwM handleResult $ do
  let
    contract = caContract ca
    ce = caEnv ca
    origination = (dummyOrigination (caStorage ca) contract)
      { ooBalance = ceBalance ce
      }
    addr = mkContractAddress origination
    txData = TxData
      { tdSenderAddress = ceSender ce
      , tdParameter = caParameter ca
      , tdAmount = unsafeMkMutez 100
      }
    interpreterOps =
      [ OriginateOp origination
      , TransferOp addr txData
      ]
  (addr,) <$> interpreterPure dummyNow dummyMaxSteps initGState interpreterOps
  where
    toNewStorage :: InterpretUntypedResult MorleyLogs -> Value Op
    toNewStorage InterpretUntypedResult {..} = unsafeValToValue iurNewStorage

    handleResult :: (Address, InterpreterRes) -> Expectation
    handleResult (addr, ir) = do
      expectedValue <-
        either (throwM . UnexpectedFailed) (pure . toNewStorage) $
        interpretMorleyUntyped
                  (caContract ca) (caParameter ca) (caStorage ca) (caEnv ca)
      case gsAddresses (_irGState ir) ^. at addr of
        Nothing -> expectationFailure $ "Address not found: " <> pretty addr
        Just (ASContract cs) -> csStorage cs `shouldBe` expectedValue
        Just _ -> expectationFailure $ "Address has unexpected state " <> pretty addr

failsToOriginateTwice :: Expectation
failsToOriginateTwice =
  interpreterPure dummyNow dummyMaxSteps initGState ops `shouldSatisfy`
  isAlreadyOriginated
  where
    contract = caContract contractAux1
    origination = dummyOrigination (caStorage contractAux1) contract
    ops = [OriginateOp origination, OriginateOp origination]
    isAlreadyOriginated (Left (IEAlreadyOriginated {})) = True
    isAlreadyOriginated _ = False

----------------------------------------------------------------------------
-- Data
----------------------------------------------------------------------------

contractAux1 :: ContractAux
contractAux1 = ContractAux
  { caContract = contract
  , caEnv = dummyContractEnv
  , caStorage = ValueTrue
  , caParameter = ValueString "aaa"
  }
  where
    contract :: Contract Op
    contract = Contract
      { para = Type tstring noAnn
      , stor = Type tbool noAnn
      , code =
        [ Op $ CDR noAnn noAnn
        , Op $ NIL noAnn noAnn $ Type T_operation noAnn
        , Op $ PAIR noAnn noAnn noAnn noAnn
        ]
      }

contractAux2 :: ContractAux
contractAux2 = contractAux1
  { caContract = (caContract contractAux1)
    { code =
      [ Op $ CDR noAnn noAnn
      , Op $ NOT noAnn
      , Op $ NIL noAnn noAnn $ Type T_operation noAnn
      , Op $ PAIR noAnn noAnn noAnn noAnn
      ]
    }
  }
