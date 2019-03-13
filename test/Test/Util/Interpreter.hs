module Test.Util.Interpreter
  ( ContractAux(..)
  , dummyContractEnv
  , dummyMaxSteps
  , dummyNow
  , dummyKeyHash
  , dummyOrigination
  ) where

import Test.QuickCheck (arbitrary)

import Michelson.Interpret (ContractEnv(..))
import Michelson.Untyped
import Morley.Types (NopInstr)
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

dummyContractEnv :: ContractEnv NopInstr
dummyContractEnv = ContractEnv
  { ceNow = dummyNow
  , ceMaxSteps = dummyMaxSteps
  , ceBalance = unsafeMkMutez 100
  , ceContracts = mempty
  , ceSource = dummyContractAddress
  , ceSender = dummyContractAddress
  , ceAmount = unsafeMkMutez 100
  }

dummyKeyHash :: KeyHash
dummyKeyHash = runGen arbitrary

dummyOrigination ::
     Value (Op NopInstr)
  -> Contract (Op NopInstr)
  -> OriginationOperation NopInstr
dummyOrigination storage contract = OriginationOperation
  { ooManager = dummyKeyHash
  , ooDelegate = Nothing
  , ooSpendable = False
  , ooDelegatable = False
  , ooBalance = unsafeMkMutez 100
  , ooStorage = storage
  , ooContract = contract
  }

-- | Data type, that containts contract and its auxiliary data.
--
-- This type is mostly used for testing purposes.
data ContractAux = ContractAux
  { caContract :: !(Contract (Op NopInstr))
  , caEnv :: !(ContractEnv NopInstr)
  , caStorage :: !(Value (Op NopInstr))
  , caParameter :: !(Value (Op NopInstr))
  }
