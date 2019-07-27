-- | Tests for the contract that calls self several times.

module Test.Interpreter.CallSelf
  ( test_self_caller
  ) where

import Test.HUnit (Assertion, (@?=))
import Test.QuickCheck (Gen, choose, forAll, withMaxSuccess)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)

import Michelson.Interpret (ContractEnv(..), InterpreterState(..), RemainingSteps(..))
import Michelson.Runtime.GState
import Michelson.Test (ContractPropValidator, contractProp, testTreesWithContract)
import Michelson.Test.Dummy
import Michelson.Test.Integrational
import Michelson.Typed
import qualified Michelson.Untyped as U
import Tezos.Address (Address)
import Tezos.Core (unsafeMkMutez)

test_self_caller :: IO [TestTree]
test_self_caller =
  testTreesWithContract "contracts/call_self_several_times.tz" $ \selfCaller ->
  pure (testImpl selfCaller)

data Fixture = Fixture
  { fMaxSteps :: RemainingSteps
  , fParameter :: !Word64
  } deriving Show

gasForOneExecution :: Num a => a
gasForOneExecution = 19

gasForLastExecution :: Num a => a
gasForLastExecution = 20

fExpectSuccess :: Fixture -> Bool
fExpectSuccess Fixture {..} =
  -- note: fParameter must be ≥ 1
  fMaxSteps >= fromIntegral ((fParameter - 1) * gasForOneExecution + gasForLastExecution)

genFixture :: Gen Fixture
genFixture =
  Fixture <$> (RemainingSteps <$> choose (minGas, maxGas)) <*> choose (minCalls, maxCalls)
  where
    minCalls = 1
    maxCalls = 10
    minGas = 0
    maxGas = gasForOneExecution * maxCalls

type Parameter = 'Tc 'CInt
type Storage = 'Tc 'CNat

testImpl ::
     (U.Contract, Contract Parameter Storage)
  -> [TestTree]
testImpl (uSelfCaller, selfCaller) =
  [ testCase ("With parameter 1 single execution consumes " <>
      show @_ @Int gasForLastExecution <> " gas") $
    contractProp selfCaller (unitValidator gasForLastExecution) unitContractEnv
    (1 :: Integer) (0 :: Natural)

  , testCase ("With parameter 2 single execution consumes " <>
      show @_ @Int gasForOneExecution <> " gas") $
    contractProp selfCaller (unitValidator gasForOneExecution) unitContractEnv
    (2 :: Integer) (0 :: Natural)

  , testProperty propertyDescription $
    withMaxSuccess 10 $
    forAll genFixture $ \fixture ->
      integrationalTestProperty (integrationalScenario uSelfCaller fixture)
  ]
  where
    -- Environment for unit test
    unitContractEnv = dummyContractEnv
    -- Validator for unit test
    unitValidator ::
      RemainingSteps -> ContractPropValidator Storage Assertion
    unitValidator gasDiff (_, isRemainingSteps -> remSteps) =
      remSteps @?= ceMaxSteps unitContractEnv - gasDiff

    propertyDescription =
      "calls itself as many times as you pass to it as a parameter, " <>
      "it fails due to gas limit if the number is large, otherwise the " <>
      "storage is updated to the number of calls"

integrationalScenario :: U.Contract -> Fixture -> IntegrationalScenario
integrationalScenario uSelfCaller fixture = do
  setMaxSteps (fMaxSteps fixture)
  address <- originate uSelfCaller "self-caller" (U.ValueInt 0) (unsafeMkMutez 1)
  let
    txData :: TxData
    txData = TxData
      { tdSenderAddress = genesisAddress
      , tdParameter = U.ValueInt (fromIntegral $ fParameter fixture)
      , tdAmount = minBound
      }
  transfer txData address
  validate (validator address)
  where
    validator :: Address -> IntegrationalValidator
    validator address
      | fExpectSuccess fixture =
        let expectedStorage =
              U.ValueInt (fromIntegral $ fParameter fixture)
         in Right $ expectStorageUpdateConst address expectedStorage
      | otherwise = Left expectGasExhaustion
