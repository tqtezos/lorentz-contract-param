-- | Interpreter of Michelson contracts.

module Michelson.Interpret
       ( ContractEnv (..)
       , MichelsonFailed (..)
       , michelsonInterpreter
       ) where

import Michelson.Types

-- | Environment for contract execution.
data ContractEnv = ContractEnv
  { ceNow :: !Timestamp
  -- ^ Timestamp of the block whose validation triggered this execution.
  , ceMaxSteps :: !Word64
  -- ^ Number of steps after which execution unconditionally terminates.
  , ceBalance :: !Mutez
  -- ^ Current amount of mutez of the current contract.
  , ceStorage :: !(Value Op)
  -- ^ Storage value associated with the current contract.
  , ceContracts :: !(Map Address (Contract Op))
  -- ^ Mapping from existing contracts' addresses to their executable
  -- representation.
  , ceParameter :: !(Value Op)
  -- ^ Parameter passed to the contract.
  , ceSource :: !Address
  -- ^ The contract that initiated the current transaction.
  , ceSender :: !Address
  -- ^ The contract that initiated the current internal transaction.
  , ceAmount :: !Mutez
  -- ^ Amount of the current transaction.
  }

-- | Represents `[FAILED]` state of a Michelson program. Contains
-- value that was on top of the stack when `FAILWITH` was called.
newtype MichelsonFailed = MichelsonFailed
  { unMichelsonFailed :: Value Op
  } deriving (Show)

-- TODO [TM-16] Implement!
-- | Interpret a contract without performing any side effects.
michelsonInterpreter :: ContractEnv -> Contract Op -> Either MichelsonFailed ([NetworkOp], Value Op)
michelsonInterpreter _ _ = pure ([], ValueFalse)
