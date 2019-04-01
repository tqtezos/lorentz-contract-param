-- | Tests for the 'environment.tz' contract

module Test.Interpreter.EnvironmentSpec
  ( environmentSpec
  ) where

import Test.Hspec (Spec)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary(..), choose)
import Test.QuickCheck.Instances.Text ()

import Michelson.Interpret (RemainingSteps(..))
import Michelson.Typed
import Michelson.Untyped (UntypedContract, UntypedValue)
import qualified Michelson.Untyped as Untyped
import Morley.Runtime.GState
import Morley.Test (specWithContract)
import Morley.Test.Integrational
import Tezos.Address
import Tezos.Core

environmentSpec :: Spec
environmentSpec =
  specWithContract "contracts/environment.tz" specImpl

data Fixture = Fixture
  { fNow :: !Timestamp
  , fMaxSteps :: !RemainingSteps
  , fPassOriginatedAddress :: !Bool
  , fBalance :: !Mutez
  , fAmount :: !Mutez
  } deriving (Show)

instance Arbitrary Fixture where
  arbitrary = do
    fNow <- timestampFromSeconds @Int <$> choose (100000, 111111)
    fMaxSteps <- RemainingSteps <$> choose (1000, 1200)
    fPassOriginatedAddress <- arbitrary
    fBalance <- unsafeMkMutez <$> choose (1, 1234)
    fAmount <- unsafeMkMutez <$> choose (1, 42)
    return Fixture {..}

shouldExpectFailed :: Fixture -> Bool
shouldExpectFailed fixture =
  or
    [ fBalance fixture > unsafeMkMutez 1000
    , fNow fixture < timestampFromSeconds @Int 100500
    , fPassOriginatedAddress fixture
    , fAmount fixture < unsafeMkMutez 15
    ]

shouldReturn :: Fixture -> UntypedValue
shouldReturn fixture
  | fMaxSteps fixture - consumedGas > 1000 = Untyped.ValueTrue
  | otherwise = Untyped.ValueFalse
  where
    consumedGas = 19

specImpl ::
    (UntypedContract, Contract ('Tc 'CAddress) ('Tc 'CBool))
  -> Spec
specImpl (uEnvironment, _environment)  = do
  let scenario = integrationalScenario uEnvironment
  modifyMaxSuccess (min 12) $
    prop description $
      integrationalTestExpectation . scenario
  where
    description =
      "This contract fails under conditions described in a comment at the " <>
      "beginning of this contract and returns whether remaining gas is " <>
      "greater than 1000"

integrationalScenario :: UntypedContract -> Fixture -> IntegrationalScenario
integrationalScenario contract fixture = do
  -- First of all let's set desired gas limit and NOW
  setNow $ fNow fixture
  setMaxSteps $ fMaxSteps fixture

  -- Then let's originated the 'environment.tz' contract
  environmentAddress <-
    originate contract Untyped.ValueFalse (fBalance fixture)

  -- And transfer tokens to it
  let
    param
      | fPassOriginatedAddress fixture = environmentAddress
      | otherwise = genesisAddress
    txData = TxData
      { tdSenderAddress = genesisAddress
      , tdParameter = Untyped.ValueString (formatAddress param)
      , tdAmount = fAmount fixture
      }
  transfer txData environmentAddress

  -- Execute operations and check that interpreter fails when one of
  -- failure conditions is met or updates environment's storage
  -- approriately
  let
    validator
      | shouldExpectFailed fixture =
        Left $ expectMichelsonFailed environmentAddress
      | otherwise =
        Right $ expectStorageConst environmentAddress $ shouldReturn fixture
  validate validator
