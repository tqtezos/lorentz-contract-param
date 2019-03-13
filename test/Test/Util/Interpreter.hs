module Test.Util.Interpreter
  ( ContractAux(..)
  , dummyContractEnv
  , dummyMaxSteps
  , dummyNow
  , dummyKeyHash
  , dummyContractAddress
  , dummyOrigination
  , simplerIntegrationalTestExpectation
  , simplerIntegrationalTestProperty
  ) where

import Test.Hspec (Expectation)
import Test.QuickCheck (Property, arbitrary)

import Michelson.Interpret (ContractEnv(..))
import Michelson.Untyped
import Morley.Runtime (InterpreterOp)
import Morley.Test.Integrational
  (UpdatesValidator, integrationalTestExpectation, integrationalTestProperty)
import Tezos.Address (Address(..), mkContractAddressRaw)
import Tezos.Core (Timestamp(..), unsafeMkMutez)
import Tezos.Crypto (KeyHash)

import Test.Util.QuickCheck (runGen)

dummyNow :: Timestamp
dummyNow = Timestamp 100

dummyMaxSteps :: Word64
dummyMaxSteps = 100500

-- We cheat here, but it's not important in tests
dummyContractAddress :: Address
dummyContractAddress = mkContractAddressRaw ""

dummyContractEnv :: ContractEnv
dummyContractEnv = ContractEnv
  { ceNow = dummyNow
  , ceMaxSteps = dummyMaxSteps
  , ceBalance = unsafeMkMutez 100
  , ceContracts = mempty
  , ceSelf = dummyContractAddress
  , ceSource = dummyContractAddress
  , ceSender = dummyContractAddress
  , ceAmount = unsafeMkMutez 100
  }

dummyKeyHash :: KeyHash
dummyKeyHash = runGen arbitrary

dummyOrigination ::
     Value Op
  -> Contract Op
  -> OriginationOperation
dummyOrigination storage contract = OriginationOperation
  { ooManager = dummyKeyHash
  , ooDelegate = Nothing
  , ooSpendable = False
  , ooDelegatable = False
  , ooBalance = unsafeMkMutez 100
  , ooStorage = storage
  , ooContract = contract
  }

-- | 'integrationalTestExpectation' which uses dummy now and max steps.
simplerIntegrationalTestExpectation :: [InterpreterOp] -> UpdatesValidator -> Expectation
simplerIntegrationalTestExpectation =
  integrationalTestExpectation dummyNow dummyMaxSteps

-- | 'integrationalTestProperty' which uses dummy now and max steps.
simplerIntegrationalTestProperty :: [InterpreterOp] -> UpdatesValidator -> Property
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
