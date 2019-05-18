-- | Module, containing data types for Michelson value.

module Michelson.Typed.Value
  ( Value' (..)
  , ContractInp1
  , ContractInp
  , ContractOut1
  , ContractOut
  , CreateAccount (..)
  , CreateContract (..)
  , CValue (..)
  , Operation' (..)
  , SetDelegate (..)
  , TransferTokens (..)
  ) where

import Data.Singletons (SingI)
import Fmt (Buildable(build), (+|), (|+))

import Michelson.EqParam
import Michelson.Typed.CValue (CValue(..))
import Michelson.Typed.Scope (HasNoOp)
import Michelson.Typed.T (T(..))
import Tezos.Address (Address)
import Tezos.Core (Mutez)
import Tezos.Crypto (KeyHash, PublicKey, Signature)

-- | Data type, representing operation, list of which is returned
-- by Michelson contract (according to calling convention).
--
-- These operations are to be further executed against system state
-- after the contract execution.
data Operation' instr where
  OpTransferTokens
    :: (Typeable p, SingI p, HasNoOp p)
    => TransferTokens instr p -> Operation' instr
  OpSetDelegate :: SetDelegate -> Operation' instr
  OpCreateAccount :: CreateAccount -> Operation' instr
  OpCreateContract
    :: ( Show (instr (ContractInp cp st) (ContractOut st)), SingI cp, SingI st
       , Typeable instr, Typeable cp, Typeable st, HasNoOp cp, HasNoOp st)
    => CreateContract instr cp st
    -> Operation' instr

instance Buildable (Operation' instr) where
  build =
    \case
      OpTransferTokens tt -> build tt
      OpSetDelegate sd -> build sd
      OpCreateAccount ca -> build ca
      OpCreateContract cc -> build cc

deriving instance Show (Operation' instr)
instance Eq (Operation' instr) where
  op1 == op2 = case (op1, op2) of
    (OpTransferTokens tt1, OpTransferTokens tt2) -> eqParam1 tt1 tt2
    (OpTransferTokens _, _) -> False
    (OpSetDelegate sd1, OpSetDelegate sd2) -> sd1 == sd2
    (OpSetDelegate _, _) -> False
    (OpCreateAccount ca1, OpCreateAccount ca2) -> ca1 == ca2
    (OpCreateAccount _, _) -> False
    (OpCreateContract cc1, OpCreateContract cc2) -> eqParam3 cc1 cc2
    (OpCreateContract _, _) -> False

data TransferTokens instr p = TransferTokens
  { ttContractParameter :: !(Value' instr p)
  , ttAmount :: !Mutez
  , ttContract :: !(Value' instr ('TContract p))
  } deriving (Show, Eq)

instance Buildable (TransferTokens instr p) where
  build TransferTokens {..} =
    "Transfer " +| ttAmount |+ " tokens to " +| destAddr |+ ""
    where
      destAddr = case ttContract of VContract addr -> addr

data SetDelegate = SetDelegate
  { sdMbKeyHash :: !(Maybe KeyHash)
  } deriving (Show, Eq)

instance Buildable SetDelegate where
  build (SetDelegate mbDelegate) =
    "Set delegate to " <> maybe "<nobody>" build mbDelegate

data CreateAccount = CreateAccount
  { caManager :: !KeyHash
  , caDelegate :: !(Maybe KeyHash)
  , caSpendable :: !Bool
  , caBalance :: !Mutez
  } deriving (Show, Eq)

instance Buildable CreateAccount where
  build CreateAccount {..} =
    "Create a new account with manager " +| caManager |+
    " and delegate " +| maybe "<nobody>" build caDelegate |+
    ", spendable: " +| caSpendable |+
    " and balance = " +| caBalance |+ ""

data CreateContract instr cp st
  = ( Show (instr (ContractInp cp st) (ContractOut st))
    , Eq (instr (ContractInp cp st) (ContractOut st))
    )
  => CreateContract
  { ccManager :: !KeyHash
  , ccDelegate :: !(Maybe KeyHash)
  , ccSpendable :: !Bool
  , ccDelegatable :: !Bool
  , ccBalance :: !Mutez
  , ccStorageVal :: !(Value' instr st)
  , ccContractCode :: !(instr (ContractInp cp st) (ContractOut st))
  }

instance Buildable (CreateContract instr cp st) where
  build CreateContract {..} =
    "Create a new contract with manager " +| ccManager |+
    " and delegate " +| maybe "<nobody>" build ccDelegate |+
    ", spendable: " +| ccSpendable |+
    ", delegatable: " +| ccDelegatable |+
    " and balance = " +| ccBalance |+ ""

deriving instance Show (CreateContract instr cp st)
deriving instance Eq (CreateContract instr cp st)

type ContractInp1 param st = 'TPair param st
type ContractInp param st = '[ ContractInp1 param st ]

type ContractOut1 st = 'TPair ('TList 'TOperation) st
type ContractOut st = '[ ContractOut1 st ]

-- | Representation of Michelson value.
--
-- Type parameter @instr@ stands for Michelson instruction
-- type, i.e. data type to represent an instruction of language.

data Value' instr t where
  VC :: CValue t -> Value' instr ('Tc t)
  VKey :: PublicKey -> Value' instr 'TKey
  VUnit :: Value' instr 'TUnit
  VSignature :: Signature -> Value' instr 'TSignature
  VOption :: forall t instr. Maybe (Value' instr t) -> Value' instr ('TOption t)
  VList :: forall t instr. [Value' instr t] -> Value' instr ('TList t)
  VSet :: forall t instr. Set (CValue t) -> Value' instr ('TSet t)
  VOp :: Operation' instr -> Value' instr 'TOperation
  VContract :: forall p instr. Address -> Value' instr ('TContract p)
  VPair :: forall l r instr. (Value' instr l, Value' instr r) -> Value' instr ('TPair l r)
  VOr :: forall l r instr. Either (Value' instr l) (Value' instr r) -> Value' instr ('TOr l r)
  VLam
    :: forall inp out instr.
       ( Show (instr '[inp] '[out])
       , Eq (instr '[inp] '[out])
       )
    => instr (inp ': '[]) (out ': '[]) -> Value' instr ('TLambda inp out)
  VMap :: forall k v instr. Map (CValue k) (Value' instr v) -> Value' instr ('TMap k v)
  VBigMap :: forall k v instr. Map (CValue k) (Value' instr v) -> Value' instr ('TBigMap k v)

deriving instance Show (Value' instr t)
deriving instance Eq (Value' instr t)

-- TODO: actually we should handle big maps with something close
-- to following:
--
--  VBigMap :: BigMap op ref k v -> Value' cp ('TBigMap k v)
--
-- data Value'Op v
--     = New v
--     | Upd v
--     | Rem
--     | NotExisted
--
-- data BigMap op ref k v = BigMap
--  { bmRef :: ref k v, bmChanges :: Map (CValue k) (Value'Op (Value' cp v)) }
