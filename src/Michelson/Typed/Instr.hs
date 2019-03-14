-- | Module, containing data types for Michelson value.

module Michelson.Typed.Instr
  ( Instr (..)
  , (#)
  , Contract
  , ExtT
  , InstrExtT
  ) where

import Data.Singletons (SingI)
import Data.Kind (Type)

import Michelson.Typed.Arith
import Michelson.Typed.Polymorphic
import Michelson.Typed.T (CT(..), T(..))
import Michelson.Typed.Value (ContractInp, ContractOut, Val(..))

-- | Infix version of @Seq@ constructor.
--
-- One can represent sequence of Michelson opertaions as follows:
-- @SWAP; DROP; DUP;@ -> @SWAP # DROP # DUP@.
(#) :: Instr a b -> Instr b c -> Instr a c
(#) = Seq

infixl 0 #

-- | ExtT is extension of Instr by Morley instructions
type family ExtT (instr :: [T] -> [T] -> Type) :: Type

type InstrExtT = ExtT Instr

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
-- Type parameter @inp@ states for input stack type. That is,
-- type of the stack that is required for operation to execute.
--
-- Type parameter @out@ states for output stack type or type
-- of stack that will be left after instruction's execution.
data Instr (inp :: [T]) (out :: [T]) where
  Seq :: Instr a b -> Instr b c -> Instr a c
  Nop :: Instr s s
  -- ^ Nop operation. Missing in Michelson spec, added to parse construction
  -- like  `IF {} { SWAP; DROP; }`.

  Ext :: ExtT Instr -> Instr s s

  DROP :: Instr (a ': s) s
  DUP  :: Instr (a ': s) (a ': a ': s)
  SWAP :: Instr (a ': b ': s) (b ': a ': s)
  PUSH :: forall t s . SingI t => Val Instr t -> Instr s (t ': s)
  SOME :: Instr (a ': s) ('T_option a ': s)
  NONE :: forall a s . SingI a => Instr s ('T_option a ': s)
  UNIT :: Instr s ('T_unit ': s)
  IF_NONE
    :: Instr s s'
    -> Instr (a ': s) s'
    -> Instr ('T_option a ': s) s'
  PAIR :: Instr (a ': b ': s) ('T_pair a b ': s)
  CAR :: Instr ('T_pair a b ': s) (a ': s)
  CDR :: Instr ('T_pair a b ': s) (b ': s)
  LEFT :: forall a b s . SingI b => Instr (a ': s) ('T_or a b ': s)
  RIGHT :: forall a b s. SingI a => Instr (b ': s) ('T_or a b ': s)
  IF_LEFT :: Instr (a ': s) s'
          -> Instr (b ': s) s'
          -> Instr ('T_or a b ': s) s'
  IF_RIGHT :: Instr (b ': s) s'
          -> Instr (a ': s) s'
          -> Instr ('T_or a b ': s) s'
  NIL :: SingI p => Instr s ('T_list p ': s)
  CONS :: Instr (a ': 'T_list a ': s) ('T_list a ': s)
  IF_CONS :: Instr (a ': 'T_list a ': s) s'
          -> Instr s s'
          -> Instr ('T_list a ': s) s'
  SIZE :: SizeOp c => Instr (c ': s) ('T_c 'T_nat ': s)
  EMPTY_SET :: SingI e => Instr s ('T_set e ': s)
  EMPTY_MAP :: (SingI a, SingI b) => Instr s ('T_map a b ': s)
  MAP :: MapOp c b => Instr (MapOpInp c ': s) (b ': s)
      -> Instr (c ': s) (MapOpRes c b ': s)
  ITER :: IterOp c => Instr (IterOpEl c ': s) s -> Instr (c ': s) s
  MEM :: MemOp c => Instr ('T_c (MemOpKey c) ': c ': s) ('T_c 'T_bool ': s)
  GET
    :: GetOp c
    => Instr ('T_c (GetOpKey c) ': c ': s) ('T_option (GetOpVal c) ': s)
  UPDATE
    :: UpdOp c
    => Instr ('T_c (UpdOpKey c) ': UpdOpParams c ': c ': s) (c ': s)
  IF :: Instr s s'
     -> Instr s s'
     -> Instr ('T_c 'T_bool ': s) s'
  LOOP :: Instr s ('T_c 'T_bool ': s)
       -> Instr ('T_c 'T_bool ': s) s
  LOOP_LEFT :: Instr (a ': s) ('T_or a b ': s)
            -> Instr ('T_or a b ': s) (b ': s)
  LAMBDA :: forall i o s. (SingI i, SingI o)
         => Val Instr ('T_lambda i o) -> Instr s ('T_lambda i o ': s)
  EXEC :: Instr (t1 ': 'T_lambda t1 t2 ': s) (t2 ': s)
  DIP :: Instr a c -> Instr (b ': a) (b ': c)
  FAILWITH :: Instr (a ': s) t
  CAST :: forall a s . SingI a => Instr (a ': s) (a ': s)
  RENAME :: Instr (a ': s) (a ': s)
  PACK :: Instr (a ': s) ('T_c 'T_bytes ': s)
  UNPACK :: SingI a => Instr ('T_c 'T_bytes ': s) ('T_option a ': s)
  CONCAT :: ConcatOp c => Instr (c ': c ': s) (c ': s)
  CONCAT' :: ConcatOp c => Instr ('T_list c ': s) (c ': s)
  SLICE
    :: SliceOp c
    => Instr ('T_c 'T_nat ': 'T_c 'T_nat ': c ': s) ('T_option c ': s)
  ISNAT :: Instr ('T_c 'T_int ': s) ('T_option ('T_c 'T_nat) ': s)
  ADD
    :: ArithOp Add n m
    => Instr ('T_c n ': 'T_c m ': s) ('T_c (ArithRes Add n m) ': s)
  SUB
    :: ArithOp Sub n m
    => Instr ('T_c n ': 'T_c m ': s) ('T_c (ArithRes Sub n m) ': s)
  MUL
    :: ArithOp Mul n m
    => Instr ('T_c n ': 'T_c m ': s) ('T_c (ArithRes Mul n m) ': s)
  EDIV
    :: EDivOp n m
    => Instr ('T_c n ': 'T_c m ': s)
                 (('T_option ('T_pair ('T_c (EDivOpRes n m))
                                      ('T_c (EModOpRes n m)))) ': s)
  ABS
    :: UnaryArithOp Abs n
    => Instr ('T_c n ': s) ('T_c (UnaryArithRes Abs n) ': s)
  NEG
    :: UnaryArithOp Neg n
    => Instr ('T_c n ': s) ('T_c (UnaryArithRes Neg n) ': s)
  LSL
    :: ArithOp Lsl n m
    => Instr ('T_c n ': 'T_c m ': s) ('T_c (ArithRes Lsl n m) ': s)
  LSR
    :: ArithOp Lsr n m
    => Instr ('T_c n ': 'T_c m ': s) ('T_c (ArithRes Lsr n m) ': s)
  OR
    :: ArithOp Or n m
    => Instr ('T_c n ': 'T_c m ': s) ('T_c (ArithRes Or n m) ': s)
  AND
    :: ArithOp And n m
    => Instr ('T_c n ': 'T_c m ': s) ('T_c (ArithRes And n m) ': s)
  XOR
    :: ArithOp Xor n m
    => Instr ('T_c n ': 'T_c m ': s) ('T_c (ArithRes Xor n m) ': s)
  NOT
    :: UnaryArithOp Not n
    => Instr ('T_c n ': s) ('T_c (UnaryArithRes Not n) ': s)
  COMPARE
    :: ArithOp Compare n m
    => Instr ('T_c n ': 'T_c m ': s) ('T_c (ArithRes Compare n m) ': s)
  EQ
    :: UnaryArithOp Eq' n
    => Instr ('T_c n ': s) ('T_c (UnaryArithRes Eq' n) ': s)
  NEQ
    :: UnaryArithOp Neq n
    => Instr ('T_c n ': s) ('T_c (UnaryArithRes Neq n) ': s)
  LT
    :: UnaryArithOp Lt n
    => Instr ('T_c n ': s) ('T_c (UnaryArithRes Lt n) ': s)
  GT
    :: UnaryArithOp Gt n
    => Instr ('T_c n ': s) ('T_c (UnaryArithRes Gt n) ': s)
  LE
    :: UnaryArithOp Le n
    => Instr ('T_c n ': s) ('T_c (UnaryArithRes Le n) ': s)
  GE
    :: UnaryArithOp Ge n
    => Instr ('T_c n ': s) ('T_c (UnaryArithRes Ge n) ': s)
  INT :: Instr ('T_c 'T_nat ': s) ('T_c 'T_int ': s)
  SELF :: forall (cp :: T) s . Instr s ('T_contract cp ': s)
  CONTRACT
    :: SingI p => Instr ('T_c 'T_address ': s) ('T_option ('T_contract p) ': s)
  TRANSFER_TOKENS
    :: Instr (p ': 'T_c 'T_mutez ': 'T_contract p ': s)
                   ('T_operation ': s)
  SET_DELEGATE
    :: Instr ('T_option ('T_c 'T_key_hash) ': s) ('T_operation ': s)

  CREATE_ACCOUNT
    :: Instr
        ('T_c 'T_key_hash ': 'T_option ('T_c 'T_key_hash) ': 'T_c 'T_bool
         ': 'T_c 'T_mutez ': s) ('T_operation ': 'T_c 'T_address ': s)

  CREATE_CONTRACT
    :: (SingI p, SingI g)
    => Instr
        ('T_c 'T_key_hash ': 'T_option ('T_c 'T_key_hash) ': 'T_c 'T_bool
          ': 'T_c 'T_bool ': 'T_c 'T_mutez
          ': 'T_lambda ('T_pair p g)
                       ('T_pair ('T_list 'T_operation) g) ': g ': s)
        ('T_operation ': 'T_c 'T_address ': s)
  CREATE_CONTRACT2
    :: (SingI p, SingI g)
    => Instr '[ 'T_pair p g ] '[ 'T_pair ('T_list 'T_operation) g ]
    -> Instr ('T_c 'T_key_hash ':
              'T_option ('T_c 'T_key_hash) ':
              'T_c 'T_bool ':
              'T_c 'T_bool ':
              'T_c 'T_mutez ':
               g ': s)
             ('T_operation ': 'T_c 'T_address ': s)

  IMPLICIT_ACCOUNT
    :: Instr ('T_c 'T_key_hash ': s) ('T_contract 'T_unit ': s)
  NOW :: Instr s ('T_c 'T_timestamp ': s)
  AMOUNT :: Instr s ('T_c 'T_mutez ': s)
  BALANCE :: Instr s ('T_c 'T_mutez ': s)
  CHECK_SIGNATURE
    :: Instr ('T_key ': 'T_signature ': 'T_c 'T_bytes ': s)
                   ('T_c 'T_bool ': s)
  SHA256 :: Instr ('T_c 'T_bytes ': s) ('T_c 'T_bytes ': s)
  SHA512 :: Instr ('T_c 'T_bytes ': s) ('T_c 'T_bytes ': s)
  BLAKE2B :: Instr ('T_c 'T_bytes ': s) ('T_c 'T_bytes ': s)
  HASH_KEY :: Instr ('T_key ': s) ('T_c 'T_key_hash ': s)
  STEPS_TO_QUOTA :: Instr s ('T_c 'T_nat ': s)
  SOURCE :: Instr s ('T_c 'T_address ': s)
  SENDER :: Instr s ('T_c 'T_address ': s)
  ADDRESS :: Instr ('T_contract a ': s) ('T_c 'T_address ': s)

deriving instance Show (ExtT Instr) => Show (Instr inp out)

type Contract cp st = Instr (ContractInp cp st) (ContractOut st)
