-- | Tests for Michelson.Runtime.

module Test.Michelson.Runtime
  ( test_interpreterPure
  ) where

import Control.Lens (at)
import Fmt (pretty)
import Test.Hspec.Expectations (Expectation, expectationFailure, shouldSatisfy)
import Test.HUnit (Assertion, assertFailure, (@?=))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Michelson.ErrorPos (InstrCallStack(..), Pos(..), SrcPos(..))
import Michelson.Interpret (ContractEnv(..), InterpretUntypedResult(..), interpretUntyped)
import Michelson.Runtime
import Michelson.Runtime.GState (GState(..), initGState)
import Michelson.Test.Dummy (dummyContractEnv, dummyMaxSteps, dummyNow, dummyOrigination)
import Michelson.Typed (untypeValue)
import Michelson.Text (mt)
import Michelson.Untyped
import Tezos.Address (Address(..))
import Tezos.Core (unsafeMkMutez)

test_interpreterPure :: IO [TestTree]
test_interpreterPure = do
  illTypedContract <-
    prepareContract (Just "contracts/ill-typed/sum-strings.tz")

  pure
    [ testGroup "Updates storage value of executed contract" $
      [ testCase "contract1" $ updatesStorageValue contractAux1
      , testCase "contract2" $ updatesStorageValue contractAux2
      ]
    , testCase "Fails to originate an already originated contract" failsToOriginateTwice
    , testCase "Fails to originate an ill-typed contract"
        (failsToOriginateIllTyped (ValueString [mt||]) illTypedContract)
    ]

----------------------------------------------------------------------------
-- Test code
----------------------------------------------------------------------------

-- | Data type, that containts contract and its auxiliary data.
--
-- This type is mostly used for testing purposes.
data ContractAux = ContractAux
  { caContract :: !Contract
  , caEnv :: !ContractEnv
  , caStorage :: !Value
  , caParameter :: !Value
  }

updatesStorageValue :: ContractAux -> Assertion
updatesStorageValue ca = either (assertFailure . pretty) handleResult $ do
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
    toNewStorage :: InterpretUntypedResult -> Value
    toNewStorage InterpretUntypedResult {..} = untypeValue iurNewStorage

    handleResult :: (Address, InterpreterRes) -> Assertion
    handleResult (addr, ir) = do
      expectedValue <-
        either (assertFailure . pretty) (pure . toNewStorage) $
        interpretUntyped
                  (caContract ca) (caParameter ca) (caStorage ca) (caEnv ca)
      case gsAddresses (_irGState ir) ^. at addr of
        Nothing -> expectationFailure $ "Address not found: " <> pretty addr
        Just (ASContract cs) -> csStorage cs @?= expectedValue
        Just _ -> expectationFailure $ "Address has unexpected state " <> pretty addr

failsToOriginateTwice :: Expectation
failsToOriginateTwice =
  simpleTest ops isAlreadyOriginated
  where
    contract = caContract contractAux1
    origination = dummyOrigination (caStorage contractAux1) contract
    ops = [OriginateOp origination, OriginateOp origination]
    isAlreadyOriginated (Left (IEAlreadyOriginated {})) = True
    isAlreadyOriginated _ = False

failsToOriginateIllTyped :: Value -> Contract -> Expectation
failsToOriginateIllTyped initialStorage illTypedContract =
  simpleTest ops isIllTypedContract
  where
    origination = dummyOrigination initialStorage illTypedContract
    ops = [OriginateOp origination]
    isIllTypedContract (Left (IEIllTypedContract {})) = True
    isIllTypedContract _ = False

simpleTest ::
     [InterpreterOp]
  -> (Either InterpreterError InterpreterRes -> Bool)
  -> Expectation
simpleTest ops predicate =
  interpreterPure dummyNow dummyMaxSteps initGState ops `shouldSatisfy`
  predicate

----------------------------------------------------------------------------
-- Data
----------------------------------------------------------------------------

ics :: Word -> InstrCallStack
ics x = InstrCallStack [] (SrcPos (Pos x) (Pos 0))

contractAux1 :: ContractAux
contractAux1 = ContractAux
  { caContract = contract
  , caEnv = dummyContractEnv
  , caStorage = ValueTrue
  , caParameter = ValueString [mt|aaa|]
  }
  where
    contract :: Contract
    contract = Contract
      { para = Type tstring noAnn
      , stor = Type tbool noAnn
      , code =
        [ WithSrcEx (ics 0) $ PrimEx (CDR noAnn noAnn)
        , WithSrcEx (ics 1) $ PrimEx (NIL noAnn noAnn $ Type TOperation noAnn)
        , WithSrcEx (ics 2) $ PrimEx (PAIR noAnn noAnn noAnn noAnn)
        ]
      }

contractAux2 :: ContractAux
contractAux2 = contractAux1
  { caContract = (caContract contractAux1)
    { code =
      [ WithSrcEx (ics 0) $ PrimEx (CDR noAnn noAnn)
      , WithSrcEx (ics 1) $ PrimEx (NOT noAnn)
      , WithSrcEx (ics 2) $ PrimEx (NIL noAnn noAnn $ Type TOperation noAnn)
      , WithSrcEx (ics 3) $ PrimEx (PAIR noAnn noAnn noAnn noAnn)
      ]
    }
  }
