-- | Module, containing data types for Michelson value.

module Michelson.Typed.Value
  ( Val (..)
  , CreateAccount (..)
  , CVal (..)
  , Operation (..)
  , SetDelegate (..)
  , TransferTokens (..)

  , unsafeValToValue
  , unsafeValToOperation
  ) where

import Michelson.Typed.CValue (CVal(..))
import Michelson.Typed.T (T(..))
import Michelson.Untyped (Op, Value)
import Tezos.Address (Address)
import Tezos.Core (Mutez)
import Tezos.Crypto (KeyHash, PublicKey, Signature)

-- | Data type, representing operation, list of which is returned
-- by Michelson contract (according to calling convention).
--
-- These operations are to be further executed against system state
-- after the contract execution.
data Operation instr where
  OpTransferTokens :: TransferTokens instr p -> Operation instr
  OpSetDelegate :: SetDelegate -> Operation instr
  OpCreateAccount :: CreateAccount -> Operation instr

deriving instance Show (Operation instr)

data TransferTokens instr p = TransferTokens
  { ttContractParameter :: Val instr p
  , ttAmount :: Mutez
  , ttContract :: Val instr ('T_contract p)
  } deriving (Show)

data SetDelegate = SetDelegate
  { sdMbKeyHash :: Maybe KeyHash
  } deriving (Show)

data CreateAccount = CreateAccount
  { caKeyHash :: KeyHash
  , caMbKeyHash :: Maybe KeyHash
  , caBool :: Bool
  , caMutez :: Mutez
  } deriving (Show)

-- | Representation of Michelson value.
--
-- Type parameter @instr@ stands for Michelson instruction
-- type, i.e. data type to represent an instruction of language.
data Val instr t where
  VC :: CVal t -> Val instr ('T_c t)
  VKey :: PublicKey -> Val instr 'T_key
  VUnit :: Val instr 'T_unit
  VSignature :: Signature -> Val instr 'T_signature
  VOption :: Maybe (Val instr t) -> Val instr ('T_option t)
  VList :: [Val instr t] -> Val instr ('T_list t)
  VSet :: Set (CVal t) -> Val instr ('T_set t)
  VOp :: Operation instr -> Val instr 'T_operation
  VContract :: Address -> Val instr ('T_contract p)
  VPair :: (Val instr l, Val instr r) -> Val instr ('T_pair l r)
  VOr :: Either (Val instr l) (Val instr r) -> Val instr ('T_or l r)
  VLam
    :: Show (instr '[inp] '[out])
    => instr (inp ': '[]) (out ': '[]) -> Val instr ('T_lambda inp out)
  VMap :: Map (CVal k) (Val instr v) -> Val instr ('T_map k v)
  VBigMap :: Map (CVal k) (Val instr v) -> Val instr ('T_big_map k v)

deriving instance Show (Val instr t)

-- TODO: actually we should handle big maps with something close
-- to following:
--
--  VBigMap :: BigMap op ref k v -> Val cp ('T_big_map k v)
--
-- data ValueOp v
--     = New v
--     | Upd v
--     | Rem
--     | NotExisted
--
-- data BigMap op ref k v = BigMap
--  { bmRef :: ref k v, bmChanges :: Map (CVal k) (ValueOp (Val cp v)) }

unsafeValToValue :: Val instr t -> Value (Op nop)
unsafeValToValue = undefined

unsafeValToOperation :: Val instr t -> Operation instr
unsafeValToOperation = undefined
