-- | Utility functions for unit testing.

module Michelson.Test.Unit
  ( ContractReturn
  , ContractPropValidator
  , contractProp
  , contractPropVal
  ) where

import Michelson.Interpret (ContractEnv, ContractReturn)
import Michelson.Typed (Contract, ToT, ToVal(..))
import qualified Michelson.Typed as T
import Morley.Ext (interpretMorley)
import Morley.Types (MorleyLogs)

-- | Type for contract execution validation.
--
-- It's a function which is supplied with contract execution output
-- (failure or new storage with operation list).
--
-- Function returns a property which type is designated by type variable @prop@
-- and might be 'Test.QuickCheck.Property' or 'Test.Hspec.Expectation'
-- or anything else relevant.
type ContractPropValidator st prop = ContractReturn MorleyLogs st -> prop

-- | Contract's property tester against given input.
-- Takes contract environment, initial storage and parameter,
-- interprets contract on this input and invokes validation function.
contractProp
  :: ( ToVal param, ToVal storage
     , ToT param ~ cp, ToT storage ~ st
     )
  => Contract cp st
  -> ContractPropValidator st prop
  -> ContractEnv
  -> param
  -> storage
  -> prop
contractProp instr check env param initSt =
  contractPropVal instr check env (toVal param) (toVal initSt)

-- | Version of 'contractProp' which takes 'Val' as arguments instead
-- of regular Haskell values.
contractPropVal
  :: Contract cp st
  -> ContractPropValidator st prop
  -> ContractEnv
  -> T.Value cp
  -> T.Value st
  -> prop
contractPropVal instr check env param initSt =
  check $ interpretMorley instr param initSt env
