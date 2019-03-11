module Test.Util.Interpreter
  ( ContractAux(..)
  , dummyContractEnv
  , dummyMaxSteps
  , dummyNow
  ) where

import Michelson.Interpret (ContractEnv(..))
import Michelson.Untyped
import Morley.Types (NopInstr)
import Tezos.Address (Address(..))
import Tezos.Core (Timestamp(..), unsafeMkMutez)

dummyNow :: Timestamp
dummyNow = Timestamp 100

dummyMaxSteps :: Word64
dummyMaxSteps = 100500

dummyContractEnv :: ContractEnv NopInstr
dummyContractEnv = ContractEnv
  { ceNow = dummyNow
  , ceMaxSteps = dummyMaxSteps
  , ceBalance = unsafeMkMutez 100
  , ceContracts = mempty
  , ceSource = ContractAddress "x"
  , ceSender = ContractAddress "x"
  , ceAmount = unsafeMkMutez 100
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
