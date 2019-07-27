-- | Tests for the 'environment.tz' contract

module Test.Interpreter.EnvironmentSpec
  ( test_environment
  ) where

import Test.QuickCheck (Arbitrary(..), choose, withMaxSuccess)
import Test.QuickCheck.Instances.Text ()
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

import Michelson.Interpret (RemainingSteps(..))
import Michelson.Runtime.GState
import Michelson.Test (testTreesWithContract)
import Michelson.Test.Integrational
import Michelson.Typed
import qualified Michelson.Typed as T
import qualified Michelson.Untyped as U
import Tezos.Address
import Tezos.Core

test_environment :: IO [TestTree]
test_environment =
  testTreesWithContract "contracts/environment.tz" (pure . one . testImpl)

data Fixture = Fixture
  { fNow :: !Timestamp
  , fMaxSteps :: !RemainingSteps
  , fPassOriginatedAddress :: !Bool
  , fBalance :: !Mutez
  , fAmount :: !Mutez
  } deriving (Show)

instance Arbitrary Fixture where
  arbitrary = do
    fNow <- timestampFromSeconds <$> choose (100000, 111111)
    fMaxSteps <- RemainingSteps <$> choose (1015, 1028)
    fPassOriginatedAddress <- arbitrary
    fBalance <- unsafeMkMutez <$> choose (1, 1234)
    fAmount <- unsafeMkMutez <$> choose (1, 42)
    return Fixture {..}

shouldExpectFailed :: Fixture -> Bool
shouldExpectFailed fixture =
  or
    [ fBalance fixture > unsafeMkMutez 1000
    , fNow fixture < timestampFromSeconds 100500
    , fPassOriginatedAddress fixture
    , fAmount fixture < unsafeMkMutez 15
    ]

shouldReturn :: Fixture -> U.Value
shouldReturn fixture
  | fMaxSteps fixture - consumedGas > 1000 = U.ValueTrue
  | otherwise = U.ValueFalse
  where
    consumedGas = 19

testImpl ::
    (U.Contract, T.Contract ('Tc 'CAddress) ('Tc 'CBool))
  -> TestTree
testImpl (uEnvironment, _environment)  = do
  testProperty description $
    withMaxSuccess 50 $
      integrationalTestProperty . scenario
  where
    scenario = integrationalScenario uEnvironment
    description =
      "This contract fails under conditions described in a comment at the " <>
      "beginning of this contract and returns whether remaining gas is " <>
      "greater than 1000"

integrationalScenario :: U.Contract -> Fixture -> IntegrationalScenario
integrationalScenario contract fixture = do
  -- First of all let's set desired gas limit and NOW
  setNow $ fNow fixture
  setMaxSteps $ fMaxSteps fixture

  -- Then let's originated the 'environment.tz' contract
  environmentAddress <-
    originate contract "environment" U.ValueFalse (fBalance fixture)

  -- And transfer tokens to it
  let
    param
      | fPassOriginatedAddress fixture = environmentAddress
      | otherwise = genesisAddress
    txData = TxData
      { tdSenderAddress = genesisAddress
      , tdParameter = U.ValueString (mformatAddress param)
      , tdAmount = fAmount fixture
      }
  transfer txData environmentAddress

  -- Execute operations and check that interpreter fails when one of
  -- failure conditions is met or updates environment's storage
  -- approriately
  let
    validator
      | shouldExpectFailed fixture =
        Left $ expectMichelsonFailed (const True) environmentAddress
      | otherwise =
        Right $ expectStorageConst environmentAddress $ shouldReturn fixture
  validate validator
