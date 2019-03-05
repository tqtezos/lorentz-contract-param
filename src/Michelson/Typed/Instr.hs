-- | Module, containing data types for Michelson value.

module Michelson.Typed.Instr
  ( Instr (..)
  , (#)
  , ContractInp
  , ContractOut
  , Contract
  ) where

import Data.Singletons (SingI)

import Michelson.Typed.Arith
import Michelson.Typed.Polymorphic
import Michelson.Typed.T (CT(..), T(..))
import Michelson.Typed.Value (Val(..))

-- | Infix version of @Seq@ constructor.
--
-- One can represent sequence of Michelson opertaions as follows:
-- @SWAP; DROP; DUP;@ -> @SWAP # DROP # DUP@.
(#) :: Instr cp a b -> Instr cp b c -> Instr cp a c
(#) = Seq

infixl 0 #

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
-- Type parameter @cp@ stands for SELF contract parameter.
-- If value contains lambda inside and that lambda uses SELF instruction,
-- result of this SELF call will be of type @contract cp@.
--
-- Type parameter @inp@ states for input stack type. That is,
-- type of the stack that is required for operation to execute.
--
-- Type parameter @out@ states for output stack type or type
-- of stack that will be left after instruction's execution.
data Instr cp (inp :: [T]) (out :: [T]) where
  Seq :: Instr cp a b -> Instr cp b c -> Instr cp a c
  Nop :: Instr cp s s
  -- ^ Nop operation. Missing in Michelson spec, added to parse construction
  -- like  `IF {} { SWAP; DROP; }`.

  DROP :: Instr cp (a ': s) s
  DUP  :: Instr cp (a ': s) (a ': a ': s)
  SWAP :: Instr cp (a ': b ': s) (b ': a ': s)
  PUSH :: forall t cp s. SingI t => Val (Instr cp) t -> Instr cp s (t ': s)
  SOME :: Instr cp (a ': s) ('T_option a ': s)
  NONE :: forall a s cp. SingI a => Instr cp s ('T_option a ': s)
  UNIT :: Instr cp s ('T_unit ': s)
  IF_NONE
    :: Instr cp s s'
    -> Instr cp (a ': s) s'
    -> Instr cp ('T_option a ': s) s'
  PAIR :: Instr cp (a ': b ': s) ('T_pair a b ': s)
  CAR :: Instr cp ('T_pair a b ': s) (a ': s)
  CDR :: Instr cp ('T_pair a b ': s) (b ': s)
  LEFT :: forall a b cp s. SingI b => Instr cp (a ': s) ('T_or a b ': s)
  RIGHT :: forall a b cp s. SingI a => Instr cp (b ': s) ('T_or a b ': s)
  IF_LEFT :: Instr cp (a ': s) s'
          -> Instr cp (b ': s) s'
          -> Instr cp ('T_or a b ': s) s'
  IF_RIGHT :: Instr cp (b ': s) s'
          -> Instr cp (a ': s) s'
          -> Instr cp ('T_or a b ': s) s'
  NIL :: SingI p => Instr cp s ('T_list p ': s)
  CONS :: Instr cp (a ': 'T_list a ': s) ('T_list a ': s)
  IF_CONS :: Instr cp (a ': 'T_list a ': s) s'
          -> Instr cp s s'
          -> Instr cp ('T_list a ': s) s'
  SIZE :: SizeOp c => Instr cp (c ': s) ('T_c 'T_nat ': s)
  EMPTY_SET :: SingI e => Instr cp s ('T_set e ': s)
  EMPTY_MAP :: (SingI a, SingI b) => Instr cp s ('T_map a b ': s)
  MAP :: MapOp c => Instr cp (MapOpInp c ': s) (b ': s)
      -> Instr cp (c ': s) (MapOpRes c b ': s)
  ITER :: IterOp c => Instr cp (IterOpEl c ': s) s -> Instr cp (c ': s) s
  MEM :: MemOp c => Instr cp ('T_c (MemOpKey c) ': c ': s) ('T_c 'T_bool ': s)
  GET
    :: GetOp c
    => Instr cp ('T_c (GetOpKey c) ': c ': s) ('T_option (GetOpVal c) ': s)
  UPDATE
    :: UpdOp c
    => Instr cp ('T_c (UpdOpKey c) ': UpdOpParams c ': c ': s) (c ': s)
  IF :: Instr cp s s'
     -> Instr cp s s'
     -> Instr cp ('T_c 'T_bool ': s) s'
  LOOP :: Instr cp s ('T_c 'T_bool ': s)
       -> Instr cp ('T_c 'T_bool ': s) s
  LOOP_LEFT :: Instr cp (a ': s) ('T_or a b ': s)
            -> Instr cp ('T_or a b ': s) (b ': s)
  LAMBDA :: forall i o cp s. (SingI i, SingI o)
         => Val (Instr cp) ('T_lambda i o) -> Instr cp s ('T_lambda i o ': s)
  EXEC :: Instr cp (t1 ': 'T_lambda t1 t2 ': s) (t2 ': s)
  DIP :: Instr cp a c -> Instr cp (b ': a) (b ': c)
  FAILWITH :: Instr cp (a ': s) t
  CAST :: forall a cp s. SingI a => Instr cp (a ': s) (a ': s)
  RENAME :: Instr cp (a ': s) (a ': s)
  PACK :: Instr cp (a ': s) ('T_c 'T_bytes ': s)
  UNPACK :: SingI a => Instr cp ('T_c 'T_bytes ': s) ('T_option a ': s)
  CONCAT :: ConcatOp c => Instr cp (c ': c ': s) (c ': s)
  CONCAT' :: ConcatOp c => Instr cp ('T_list c ': s) (c ': s)
  SLICE
    :: SliceOp c
    => Instr cp ('T_c 'T_nat ': 'T_c 'T_nat ': c ': s) ('T_option c ': s)
  ISNAT :: Instr cp ('T_c 'T_int ': s) ('T_option ('T_c 'T_nat) ': s)
  ADD
    :: ArithOp Add n m
    => Instr cp ('T_c n ': 'T_c m ': s) ('T_c (ArithRes Add n m) ': s)
  SUB
    :: ArithOp Sub n m
    => Instr cp ('T_c n ': 'T_c m ': s) ('T_c (ArithRes Sub n m) ': s)
  MUL
    :: ArithOp Mul n m
    => Instr cp ('T_c n ': 'T_c m ': s) ('T_c (ArithRes Mul n m) ': s)
  EDIV
    :: EDivOp n m
    => Instr cp ('T_c n ': 'T_c m ': s)
                 (('T_option ('T_pair ('T_c (EDivOpRes n m))
                                      ('T_c (EModOpRes n m)))) ': s)
  ABS
    :: UnaryArithOp Abs n
    => Instr cp ('T_c n ': s) ('T_c (UnaryArithRes Abs n) ': s)
  NEG
    :: UnaryArithOp Neg n
    => Instr cp ('T_c n ': s) ('T_c (UnaryArithRes Neg n) ': s)
  LSL
    :: ArithOp Lsl n m
    => Instr cp ('T_c n ': 'T_c m ': s) ('T_c (ArithRes Lsl n m) ': s)
  LSR
    :: ArithOp Lsr n m
    => Instr cp ('T_c n ': 'T_c m ': s) ('T_c (ArithRes Lsr n m) ': s)
  OR
    :: ArithOp Or n m
    => Instr cp ('T_c n ': 'T_c m ': s) ('T_c (ArithRes Or n m) ': s)
  AND
    :: ArithOp And n m
    => Instr cp ('T_c n ': 'T_c m ': s) ('T_c (ArithRes And n m) ': s)
  XOR
    :: ArithOp Xor n m
    => Instr cp ('T_c n ': 'T_c m ': s) ('T_c (ArithRes Xor n m) ': s)
  NOT
    :: UnaryArithOp Not n
    => Instr cp ('T_c n ': s) ('T_c (UnaryArithRes Not n) ': s)
  COMPARE
    :: ArithOp Compare n m
    => Instr cp ('T_c n ': 'T_c m ': s) ('T_c (ArithRes Compare n m) ': s)
  EQ
    :: UnaryArithOp Eq' n
    => Instr cp ('T_c n ': s) ('T_c (UnaryArithRes Eq' n) ': s)
  NEQ
    :: UnaryArithOp Neq n
    => Instr cp ('T_c n ': s) ('T_c (UnaryArithRes Neq n) ': s)
  LT
    :: UnaryArithOp Lt n
    => Instr cp ('T_c n ': s) ('T_c (UnaryArithRes Lt n) ': s)
  GT
    :: UnaryArithOp Gt n
    => Instr cp ('T_c n ': s) ('T_c (UnaryArithRes Gt n) ': s)
  LE
    :: UnaryArithOp Le n
    => Instr cp ('T_c n ': s) ('T_c (UnaryArithRes Le n) ': s)
  GE
    :: UnaryArithOp Ge n
    => Instr cp ('T_c n ': s) ('T_c (UnaryArithRes Ge n) ': s)
  INT :: Instr cp ('T_c 'T_nat ': s) ('T_c 'T_int ': s)
  SELF :: Instr cp s ('T_contract cp ': s)
  CONTRACT
    :: SingI p => Instr cp ('T_c 'T_address ': s) ('T_option ('T_contract p) ': s)
  TRANSFER_TOKENS
    :: Instr cp (p ': 'T_c 'T_mutez ': 'T_contract p ': s)
                   ('T_operation ': s)
  SET_DELEGATE
    :: Instr cp ('T_option ('T_c 'T_key_hash) ': s) ('T_operation ': s)

  CREATE_ACCOUNT
    :: Instr cp
        ('T_c 'T_key_hash ': 'T_option ('T_c 'T_key_hash) ': 'T_c 'T_bool
         ': 'T_c 'T_mutez ': s) ('T_operation ': 'T_c 'T_address ': s)

  CREATE_CONTRACT
    :: Instr cp
        ('T_c 'T_key_hash ': 'T_option ('T_c 'T_key_hash) ': 'T_c 'T_bool
          ': 'T_c 'T_bool ': 'T_c 'T_mutez
          ': 'T_lambda ('T_pair p g)
                       ('T_pair ('T_list 'T_operation) g) ': g ': s)
        ('T_operation ': 'T_c 'T_address ': s)
  CREATE_CONTRACT2
    :: (SingI p, SingI g)
    => Instr p '[ 'T_pair p g ] '[ 'T_pair ('T_list 'T_operation) g ]
    -> Instr cp ('T_c 'T_key_hash ': 'T_option ('T_c 'T_key_hash)
                    ': 'T_c 'T_bool ': 'T_c 'T_bool ': 'T_c 'T_mutez ': g ': s)
                   ('T_operation ': 'T_c 'T_address ': s)
  IMPLICIT_ACCOUNT
    :: Instr cp ('T_c 'T_key_hash ': s) ('T_contract 'T_unit ': s)
  NOW :: Instr cp s ('T_c 'T_timestamp ': s)
  AMOUNT :: Instr cp s ('T_c 'T_mutez ': s)
  BALANCE :: Instr cp s ('T_c 'T_mutez ': s)
  CHECK_SIGNATURE
    :: Instr cp ('T_key ': 'T_signature ': 'T_c 'T_bytes ': s)
                   ('T_c 'T_bool ': s)
  SHA256 :: Instr cp ('T_c 'T_bytes ': s) ('T_c 'T_bytes ': s)
  SHA512 :: Instr cp ('T_c 'T_bytes ': s) ('T_c 'T_bytes ': s)
  BLAKE2B :: Instr cp ('T_c 'T_bytes ': s) ('T_c 'T_bytes ': s)
  HASH_KEY :: Instr cp ('T_key ': s) ('T_c 'T_key_hash ': s)
  STEPS_TO_QUOTA :: Instr cp s ('T_c 'T_nat ': s)
  SOURCE :: Instr cp s ('T_c 'T_address ': s)
  SENDER :: Instr cp s ('T_c 'T_address ': s)
  ADDRESS :: Instr cp ('T_contract a ': s) ('T_c 'T_address ': s)

deriving instance Show (Instr cp inp out)

type ContractInp param st = '[ 'T_pair param st ]
type ContractOut st = '[ 'T_pair ('T_list 'T_operation) st ]
type Contract cp st = Instr cp (ContractInp cp st) (ContractOut st)

