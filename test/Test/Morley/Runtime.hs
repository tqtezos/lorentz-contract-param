-- | Tests for Morley.Runtime.

module Test.Morley.Runtime
  ( spec
  , dummyContractEnv
  ) where

import Control.Lens (at)
import Test.Hspec
  (Expectation, Spec, context, describe, expectationFailure, it, parallel, shouldBe, shouldSatisfy,
  specify)

import Michelson.Interpret (ContractEnv(..), MichelsonFailed(..), michelsonInterpreter)
import Michelson.TypeCheck (typeCheckContract)
import Michelson.Typed (Sing, fromMType, withSomeSingT)
import Michelson.Untyped
import Morley.Nop (nopHandler)
import Morley.Runtime
import Morley.Runtime.GState (GState(..), initGState)
import Morley.Types (NopInstr)
import Tezos.Address (Address)
import Tezos.Core (Mutez(..), Timestamp(..))

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
  UnexpectedFailed MichelsonFailed
  deriving (Show)

instance Exception UnexpectedFailed

updatesStorageValue :: ContractAux -> Expectation
updatesStorageValue ca = either throwM handleResult $ do
  let
    contract = caContract ca
    ce = caEnv ca
    account = Account
      { accBalance = ceBalance ce
      , accStorage = ceStorage ce
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
      , tdParameter = ceParameter ce
      , tdAmount = Mutez 100
      }
  (addr,) <$> interpreterPure dummyNow dummyMaxSteps gState' [TransferOp addr txData]
  where
    handleResult :: (Address, InterpreterRes) -> Expectation
    handleResult (addr, ir) =
      withSomeSingT (fromMType $ para (caContract ca)) $ \(_ :: Sing cp) ->
        case typeCheckContract nopHandler (fmap unOp (caContract ca)) of
          Right typedContract -> do
            expectedValue <-
              either (throwM . UnexpectedFailed) (pure . snd) $
              michelsonInterpreter @cp nopHandler (caEnv ca) typedContract
            accStorage <$> (gsAccounts (_irGState ir) ^. at addr) `shouldBe`
              Just expectedValue
          Left _ -> expectationFailure "Illtyped contract"

failsToOriginateTwice :: Expectation
failsToOriginateTwice =
  interpreterPure dummyNow dummyMaxSteps initGState ops `shouldSatisfy`
  isAlreadyOriginated
  where
    contract = caContract contractAux1
    ce = caEnv contractAux1
    account = Account
      { accBalance = ceBalance ce
      , accStorage = ceStorage ce
      , accContract = contract
      }
    ops = [OriginateOp account, OriginateOp account]
    isAlreadyOriginated (Left (IEAlreadyOriginated {})) = True
    isAlreadyOriginated _ = False

----------------------------------------------------------------------------
-- Data
----------------------------------------------------------------------------

dummyNow :: Timestamp
dummyNow = Timestamp 100

dummyMaxSteps :: Word64
dummyMaxSteps = 100500

dummyContractEnv :: ContractEnv NopInstr
dummyContractEnv = ContractEnv
  { ceNow = dummyNow
  , ceMaxSteps = dummyMaxSteps
  , ceBalance = Mutez 100
  , ceStorage = ValueUnit
  , ceContracts = mempty
  , ceParameter = ValueUnit
  , ceSource = ContractAddress "x"
  , ceSender = ContractAddress "x"
  , ceAmount = Mutez 100
  }

-- Contract and auxiliary data
data ContractAux = ContractAux
  { caContract :: !(Contract (Op NopInstr))
  , caEnv :: !(ContractEnv NopInstr)
  }

contractAux1 :: ContractAux
contractAux1 = ContractAux
  { caContract = contract
  , caEnv = env
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
    env :: ContractEnv NopInstr
    env = dummyContractEnv
      { ceStorage = ValueTrue
      , ceParameter = ValueString "aaa"
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
