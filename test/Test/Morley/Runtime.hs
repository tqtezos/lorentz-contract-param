-- | Tests for Morley.Runtime.

module Test.Morley.Runtime
  ( spec
  ) where

import Control.Lens (at)
import Test.Hspec
  (Expectation, Spec, context, describe, it, parallel, shouldBe, shouldSatisfy, specify)

import Michelson.Interpret (ContractEnv(..), InterpretUntypedError(..), InterpretUntypedResult(..))
import Michelson.Typed (unsafeValToValue)
import Michelson.Untyped
import Morley.Nop (interpretMorleyUntyped)
import Morley.Runtime
import Morley.Runtime.GState (GState(..), initGState)
import Morley.Types (NopInstr)
import Test.Util.Interpreter (ContractAux(..), dummyContractEnv, dummyNow, dummyMaxSteps)
import Tezos.Address (Address(..))
import Tezos.Core (unsafeMkMutez)

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
  UnexpectedFailed (InterpretUntypedError NopInstr)
  deriving (Show)

instance Exception UnexpectedFailed

updatesStorageValue :: ContractAux -> Expectation
updatesStorageValue ca = either throwM handleResult $ do
  let
    contract = caContract ca
    ce = caEnv ca
    account = Account
      { accBalance = ceBalance ce
      , accStorage = caStorage ca
      , accContract = contract
      }
  gState' <- _irGState <$>
    interpreterPure dummyNow dummyMaxSteps initGState [OriginateOp account]
  -- Note: `contractAddress` most likely should require the
  -- contract to be originated, even though now it doesn't.
  let
    addr = contractAddress contract
    txData = TxData
      { tdSenderAddress = ceSender ce
      , tdParameter = caParameter ca
      , tdAmount = unsafeMkMutez 100
      }
  (addr,) <$> interpreterPure dummyNow dummyMaxSteps gState' [TransferOp addr txData]
  where
    toNewStorage :: InterpretUntypedResult -> Value (Op NopInstr)
    toNewStorage InterpretUntypedResult {..} = unsafeValToValue iurNewStorage

    handleResult :: (Address, InterpreterRes) -> Expectation
    handleResult (addr, ir) = do
      expectedValue <-
        either (throwM . UnexpectedFailed) (pure . toNewStorage) $
        interpretMorleyUntyped
                  (caContract ca) (caParameter ca) (caStorage ca) (caEnv ca)
      accStorage <$> (gsAccounts (_irGState ir) ^. at addr) `shouldBe`
        Just expectedValue

failsToOriginateTwice :: Expectation
failsToOriginateTwice =
  interpreterPure dummyNow dummyMaxSteps initGState ops `shouldSatisfy`
  isAlreadyOriginated
  where
    contract = caContract contractAux1
    ce = caEnv contractAux1
    account = Account
      { accBalance = ceBalance ce
      , accStorage = caStorage contractAux1
      , accContract = contract
      }
    ops = [OriginateOp account, OriginateOp account]
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
    contract :: Contract (Op NopInstr)
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
