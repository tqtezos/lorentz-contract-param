-- | Module, containing data types for Michelson value.

module Michelson.Typed.Value
  ( Val (..)
  , CVal (..)
  , Operation (..)
  ) where

import Michelson.Typed.T (T(..))
import Michelson.Typed.CValue (CVal(..))
import Michelson.Types (Mutez)
import Tezos.Crypto (Address, PublicKey, Signature)


-- | Data type, representing operation, list of which is returned
-- by Michelson contract (according to calling convention).
--
-- These operations are to be further executed against system state
-- after the contract execution.
data Operation instr where
  TransferTokens ::
       { opContractParameter :: Val instr p
       , opAmount :: Mutez
       , opContract :: Val instr ('T_contract p)
       } -> Operation instr

deriving instance Show (Operation instr)

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


