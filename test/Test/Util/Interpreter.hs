module Test.Util.Interpreter
  ( ContractAux(..)
  , dummyContractEnv
  , dummyMaxSteps
  , dummyNow
  , dummyOrigination
  , simplerIntegrationalTestExpectation
  , simplerIntegrationalTestProperty
  ) where

import Test.Hspec (Expectation)
import Test.QuickCheck (Property)

import Michelson.Interpret (ContractEnv(..), RemainingSteps)
import Michelson.Untyped
import Morley.Runtime (InterpreterOp)
import Morley.Runtime.GState (genesisAddress, genesisKeyHash)
import Morley.Test.Integrational
  (IntegrationalValidator, integrationalTestExpectation, integrationalTestProperty)
import Tezos.Core (Timestamp(..), unsafeMkMutez)

dummyNow :: Timestamp
dummyNow = Timestamp 100

dummyMaxSteps :: RemainingSteps
dummyMaxSteps = 100500

dummyContractEnv :: ContractEnv
dummyContractEnv = ContractEnv
  { ceNow = dummyNow
  , ceMaxSteps = dummyMaxSteps
  , ceBalance = unsafeMkMutez 100
  , ceContracts = mempty
  , ceSelf = genesisAddress
  , ceSource = genesisAddress
  , ceSender = genesisAddress
  , ceAmount = unsafeMkMutez 100
  }

dummyOrigination ::
     Value Op
  -> Contract Op
  -> OriginationOperation
dummyOrigination storage contract = OriginationOperation
  { ooManager = genesisKeyHash
  , ooDelegate = Nothing
  , ooSpendable = False
  , ooDelegatable = False
  , ooBalance = unsafeMkMutez 100
  , ooStorage = storage
  , ooContract = contract
  }

-- | 'integrationalTestExpectation' which uses dummy now and max steps.
simplerIntegrationalTestExpectation :: [InterpreterOp] -> IntegrationalValidator -> Expectation
simplerIntegrationalTestExpectation =
  integrationalTestExpectation dummyNow dummyMaxSteps

-- | 'integrationalTestProperty' which uses dummy now and max steps.
simplerIntegrationalTestProperty :: [InterpreterOp] -> IntegrationalValidator -> Property
simplerIntegrationalTestProperty =
  integrationalTestProperty dummyNow dummyMaxSteps

-- | Data type, that containts contract and its auxiliary data.
--
-- This type is mostly used for testing purposes.
data ContractAux = ContractAux
  { caContract :: !(Contract Op)
  , caEnv :: !ContractEnv
  , caStorage :: !(Value Op)
  , caParameter :: !(Value Op)
  }
