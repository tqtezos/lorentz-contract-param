-- | Module, containing data types for Michelson value
-- and instruction / instrunction sequence.

module Advanced.Value
  ( Val (..)
  , Instr (..)
  , (#)
  , SomeInstr (..)
  , ContractT
  ) where

import Data.Typeable (typeOf)
import qualified Text.Show

import Advanced.Arith (Add, ArithOp(..))
import Advanced.CValue (Address, CVal(..))
import Advanced.Type (CT(..), T(..))
import Tezos.Crypto (PublicKey, Signature)

-- | Representation of Michelson value.
--
-- Type parameter @op@ states for intrinsic operation
-- (list of which is returned by the contract to be executed
-- afterwards).
--
-- Type parameter @ref@ states for big map reference (as big map is
-- normally not fully loaded into memory).
data Val op t where
  VC :: CVal t -> Val op ('T_c t)
  VKey :: PublicKey -> Val op 'T_key
  VUnit :: Val op 'T_unit
  VSignature :: Signature -> Val op 'T_signature
  VOption :: Maybe (Val op t) -> Val op ('T_option t)
  VList :: [Val op t] -> Val op ('T_list t)
  VSet :: Set (CVal t) -> Val op ('T_set t)
  VOp :: op -> Val op 'T_operation
  VContract :: Address -> Val op ('T_contract p)
  VPair :: (Val op l, Val op r) -> Val op ('T_pair l r)
  VOr :: Either (Val op l) (Val op r) -> Val op ('T_or l r)
  VLam :: Instr op (inp ': '[]) (out ': '[]) -> Val op ('T_lambda inp out)
  VMap :: Map (CVal k) (Val op v) -> Val op ('T_map k v)
--  VBigMap :: BigMap op ref k v -> Val op ('T_big_map k v)

deriving instance Show op => Show (Val op t)

-- data ValueOp v
--     = New v
--     | Upd v
--     | Rem
--     | NotExisted
--
-- data BigMap op ref k v = BigMap { bmRef :: ref k v, bmChanges :: Map (CVal k) (ValueOp (Val op v)) }

-- | Infix version of @Seq@ constructor.
--
-- One can represent sequence of Michelson opertaions as follows:
-- @SWAP; DROP; DUP;@ -> @SWAP # DROP # DUP@.
(#) :: Instr op a b -> Instr op b c -> Instr op a c
(#) = Seq

infixl 0 #

-- TODO support add operation for mutez
-- don't forget to throw arith exception (async exception)

-- | Representation of Michelson instruction or sequence
-- of instructions.
--
-- Each Michelson instruction is represented by exactly one
-- constructor of this data type. Sequence of instructions
-- is represented with use of @Seq@ constructor in following
-- way: @SWAP; DROP ; DUP;@ -> @SWAP `Seq` DROP `Seq` DUP@.
-- Special case where there are no instructions is represented
-- by constructor @Nop@, e.g. @IF_NONE {} { SWAP; DROP; }@ ->
-- @IF_NONE Nop (SWAP `Seq` DROP)@.
--
-- Type parameter @op@ states for intrinsic operation
-- (list of which is returned by the contract to be executed
-- afterwards).
--
-- Type parameter @inp@ states for input stack type. That is,
-- type of the stack that is required for operation to execute.
--
-- Type parameter @out@ states for output stack type or type
-- of stack that will be left after instruction's execution.
data Instr op (inp :: [T]) (out :: [T]) where
  Seq :: Instr op a b -> Instr op b c -> Instr op a c
  Nop :: Instr op s s -- Added to parse construction like  `IF_NONE {} { SWAP; DROP; }`

  DROP :: Instr op (a ': s) s
  DUP  :: Instr op (a ': s) (a ': a ': s)
  SWAP :: Instr op (a ': b ': s) (b ': a ': s)
  PUSH :: forall t s op. Val op t -> Instr op s (t ': s)
  SOME :: Instr op (a ': s) ('T_option a ': s)
  NONE :: forall a s op. Instr op s ('T_option a ': s)
  UNIT :: Instr op s ('T_unit ': s)
  IF_NONE :: Instr op s (b ': s) -> Instr op (a ': s) (b ': s) -> Instr op ('T_option a ': s) (b ': s)
  PAIR :: Instr op (a ': b ': s) ('T_pair a b ': s)
  CAR :: Instr op ('T_pair a b ': s) (a ': s)
  CDR :: Instr op ('T_pair a b ': s) (b ': s)
  -- CDR               VarNote FieldNote
  -- LEFT              TypeNote VarNote FieldNote FieldNote Type
  -- RIGHT             TypeNote VarNote FieldNote FieldNote Type
  -- IF_LEFT           [op] [op]
  -- IF_RIGHT          [op] [op]
  -- NIL               TypeNote VarNote Type
  -- CONS              VarNote
  -- IF_CONS           [op] [op]
  -- SIZE              VarNote
  -- EMPTY_SET         TypeNote VarNote Comparable
  -- EMPTY_MAP         TypeNote VarNote Comparable Type
  -- MAP               VarNote [op]
  -- ITER              VarNote [op]
  -- MEM               VarNote
  -- GET               VarNote
  -- UPDATE
  -- IF                [op] [op]
  -- LOOP              [op]
  -- LOOP_LEFT         [op]
  -- LAMBDA            VarNote Type Type [op]
  -- EXEC              VarNote
  DIP :: Instr op a c -> Instr op (b ': a) (b ': c)
  -- FAILWITH
  -- CAST              TypeNote VarNote
  -- RENAME            VarNote
  -- PACK              VarNote
  -- UNPACK            VarNote Type
  -- CONCAT            VarNote
  -- SLICE             VarNote
  -- ISNAT
  ADD :: ArithOp Add n m => Instr op ('T_c n ': 'T_c m ': s) ('T_c (ArithResT Add n m) ': s)
  -- SUB               VarNote
  -- MUL               VarNote
  -- EDIV              VarNote
  -- ABS               VarNote
  -- NEG
  -- MOD
  -- LSL               VarNote
  -- LSR               VarNote
  -- OR                VarNote
  -- AND               VarNote
  -- XOR               VarNote
  -- NOT               VarNote
  -- COMPARE           VarNote
  -- EQ                VarNote
  -- NEQ               VarNote
  -- LT                VarNote
  -- GT                VarNote
  -- LE                VarNote
  -- GE                VarNote
  -- INT               VarNote
  -- SELF              VarNote
  -- CONTRACT          Type
  TRANSFER_TOKENS :: Instr op (p ': 'T_c 'T_mutez ': 'T_contract p ': s) ('T_operation ': s)
  -- SET_DELEGATE
  -- CREATE_ACCOUNT    VarNote VarNote
  -- CREATE_CONTRACT   VarNote VarNote
  -- CREATE_CONTRACT2  VarNote VarNote (Contract op)
  -- IMPLICIT_ACCOUNT  VarNote
  -- NOW               VarNote
  -- AMOUNT            VarNote
  -- BALANCE           VarNote
  -- CHECK_SIGNATURE   VarNote
  -- SHA256            VarNote
  -- SHA512            VarNote
  -- BLAKE2B           VarNote
  -- HASH_KEY          VarNote
  -- STEPS_TO_QUOTA    VarNot e
  -- SOURCE            VarNote
  -- SENDER            VarNote
  -- ADDRESS           VarNote

deriving instance Show op => Show (Instr op inp out)

data SomeInstr op where
  SomeInstr :: (Typeable inp, Typeable out) => Instr op inp out -> SomeInstr op

instance (Show op, Typeable op) => Show (SomeInstr op) where
  show (SomeInstr instr) = show instr ++ " :: " ++ show (typeOf instr)

type ContractT op storage param =
  Instr op '[ 'T_pair param storage ] '[ 'T_pair ('T_list 'T_operation) storage ]
