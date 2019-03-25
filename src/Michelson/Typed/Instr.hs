-- | Module, containing data types for Michelson value.

module Michelson.Typed.Instr
  ( Instr (..)
  , (#)
  , Contract
  , ExtT
  , InstrExtT
  ) where

import Data.Kind (Type)
import Data.Singletons (SingI)

import Michelson.Typed.Arith
import Michelson.Typed.Polymorphic
import Michelson.Typed.T (CT(..), T(..))
import Michelson.Typed.Value (ContractInp, ContractOut, Val(..))

-- | Infix version of @Seq@ constructor.
--
-- One can represent sequence of Michelson opertaions as follows:
-- @SWAP; DROP; DUP;@ -> @SWAP # DROP # DUP@.
(#) :: Typeable b => Instr a b -> Instr b c -> Instr a c
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

-- pva701: Typeable constraints are added during TM-29.
-- Maybe it makes sense to think how to eliminate them
-- if they break something
data Instr (inp :: [T]) (out :: [T]) where
  Seq :: Typeable b => Instr a b -> Instr b c -> Instr a c
  -- | Nop operation. Missing in Michelson spec, added to parse construction
  -- like  `IF {} { SWAP; DROP; }`.
  Nop :: Instr s s

  Ext :: ExtT Instr -> Instr s s

  DROP :: Instr (a ': s) s
  DUP  :: Instr (a ': s) (a ': a ': s)
  SWAP :: Instr (a ': b ': s) (b ': a ': s)
  PUSH :: forall t s . SingI t => Val Instr t -> Instr s (t ': s)
  SOME :: Instr (a ': s) ('TOption a ': s)
  NONE :: forall a s . SingI a => Instr s ('TOption a ': s)
  UNIT :: Instr s ('TUnit ': s)
  IF_NONE
    :: (Typeable a, Typeable s)
    => Instr s s'
    -> Instr (a ': s) s'
    -> Instr ('TOption a ': s) s'
  PAIR :: Instr (a ': b ': s) ('TPair a b ': s)
  CAR :: Instr ('TPair a b ': s) (a ': s)
  CDR :: Instr ('TPair a b ': s) (b ': s)
  LEFT :: forall a b s . SingI b => Instr (a ': s) ('TOr a b ': s)
  RIGHT :: forall a b s . SingI a => Instr (b ': s) ('TOr a b ': s)
  IF_LEFT
    :: (Typeable s, Typeable a, Typeable b)
    => Instr (a ': s) s'
    -> Instr (b ': s) s'
    -> Instr ('TOr a b ': s) s'
  IF_RIGHT
    :: (Typeable s, Typeable b, Typeable a)
    => Instr (b ': s) s'
    -> Instr (a ': s) s'
    -> Instr ('TOr a b ': s) s'
  NIL :: SingI p => Instr s ('TList p ': s)
  CONS :: Instr (a ': 'TList a ': s) ('TList a ': s)
  IF_CONS
    :: (Typeable s, Typeable a)
    => Instr (a ': 'TList a ': s) s'
    -> Instr s s'
    -> Instr ('TList a ': s) s'
  SIZE :: SizeOp c => Instr (c ': s) ('Tc 'CNat ': s)
  EMPTY_SET :: SingI e => Instr s ('TSet e ': s)
  EMPTY_MAP :: (SingI a, SingI b) => Instr s ('TMap a b ': s)
  MAP :: (Typeable (MapOpInp c ': s), MapOp c b)
      => Instr (MapOpInp c ': s) (b ': s)
      -> Instr (c ': s) (MapOpRes c b ': s)
  ITER :: (Typeable (IterOpEl c ': s), IterOp c) => Instr (IterOpEl c ': s) s -> Instr (c ': s) s
  MEM :: MemOp c => Instr ('Tc (MemOpKey c) ': c ': s) ('Tc 'CBool ': s)
  GET
    :: GetOp c
    => Instr ('Tc (GetOpKey c) ': c ': s) ('TOption (GetOpVal c) ': s)
  UPDATE
    :: UpdOp c
    => Instr ('Tc (UpdOpKey c) ': UpdOpParams c ': c ': s) (c ': s)
  IF :: Typeable s
     => Instr s s'
     -> Instr s s'
     -> Instr ('Tc 'CBool ': s) s'
  LOOP :: Typeable s
       => Instr s ('Tc 'CBool ': s)
       -> Instr ('Tc 'CBool ': s) s
  LOOP_LEFT
    :: (Typeable a, Typeable s)
    => Instr (a ': s) ('TOr a b ': s)
    -> Instr ('TOr a b ': s) (b ': s)
  LAMBDA :: forall i o s . (SingI i, SingI o)
         => Val Instr ('TLambda i o) -> Instr s ('TLambda i o ': s)
  EXEC :: Typeable t1 => Instr (t1 ': 'TLambda t1 t2 ': s) (t2 ': s)
  DIP :: Typeable a => Instr a c -> Instr (b ': a) (b ': c)
  FAILWITH :: Instr (a ': s) t
  CAST :: forall a s . SingI a => Instr (a ': s) (a ': s)
  RENAME :: Instr (a ': s) (a ': s)
  PACK :: Instr (a ': s) ('Tc 'CBytes ': s)
  UNPACK :: SingI a => Instr ('Tc 'CBytes ': s) ('TOption a ': s)
  CONCAT :: ConcatOp c => Instr (c ': c ': s) (c ': s)
  CONCAT' :: ConcatOp c => Instr ('TList c ': s) (c ': s)
  SLICE
    :: SliceOp c
    => Instr ('Tc 'CNat ': 'Tc 'CNat ': c ': s) ('TOption c ': s)
  ISNAT :: Instr ('Tc 'CInt ': s) ('TOption ('Tc 'CNat) ': s)
  ADD
    :: ArithOp Add n m
    => Instr ('Tc n ': 'Tc m ': s) ('Tc (ArithRes Add n m) ': s)
  SUB
    :: ArithOp Sub n m
    => Instr ('Tc n ': 'Tc m ': s) ('Tc (ArithRes Sub n m) ': s)
  MUL
    :: ArithOp Mul n m
    => Instr ('Tc n ': 'Tc m ': s) ('Tc (ArithRes Mul n m) ': s)
  EDIV
    :: EDivOp n m
    => Instr ('Tc n ': 'Tc m ': s)
                 (('TOption ('TPair ('Tc (EDivOpRes n m))
                                      ('Tc (EModOpRes n m)))) ': s)
  ABS
    :: UnaryArithOp Abs n
    => Instr ('Tc n ': s) ('Tc (UnaryArithRes Abs n) ': s)
  NEG
    :: UnaryArithOp Neg n
    => Instr ('Tc n ': s) ('Tc (UnaryArithRes Neg n) ': s)
  LSL
    :: ArithOp Lsl n m
    => Instr ('Tc n ': 'Tc m ': s) ('Tc (ArithRes Lsl n m) ': s)
  LSR
    :: ArithOp Lsr n m
    => Instr ('Tc n ': 'Tc m ': s) ('Tc (ArithRes Lsr n m) ': s)
  OR
    :: ArithOp Or n m
    => Instr ('Tc n ': 'Tc m ': s) ('Tc (ArithRes Or n m) ': s)
  AND
    :: ArithOp And n m
    => Instr ('Tc n ': 'Tc m ': s) ('Tc (ArithRes And n m) ': s)
  XOR
    :: ArithOp Xor n m
    => Instr ('Tc n ': 'Tc m ': s) ('Tc (ArithRes Xor n m) ': s)
  NOT
    :: UnaryArithOp Not n
    => Instr ('Tc n ': s) ('Tc (UnaryArithRes Not n) ': s)
  COMPARE
    :: ArithOp Compare n m
    => Instr ('Tc n ': 'Tc m ': s) ('Tc (ArithRes Compare n m) ': s)
  EQ
    :: UnaryArithOp Eq' n
    => Instr ('Tc n ': s) ('Tc (UnaryArithRes Eq' n) ': s)
  NEQ
    :: UnaryArithOp Neq n
    => Instr ('Tc n ': s) ('Tc (UnaryArithRes Neq n) ': s)
  LT
    :: UnaryArithOp Lt n
    => Instr ('Tc n ': s) ('Tc (UnaryArithRes Lt n) ': s)
  GT
    :: UnaryArithOp Gt n
    => Instr ('Tc n ': s) ('Tc (UnaryArithRes Gt n) ': s)
  LE
    :: UnaryArithOp Le n
    => Instr ('Tc n ': s) ('Tc (UnaryArithRes Le n) ': s)
  GE
    :: UnaryArithOp Ge n
    => Instr ('Tc n ': s) ('Tc (UnaryArithRes Ge n) ': s)
  INT :: Instr ('Tc 'CNat ': s) ('Tc 'CInt ': s)
  SELF :: forall (cp :: T) s . Instr s ('TContract cp ': s)
  CONTRACT
    :: SingI p => Instr ('Tc 'CAddress ': s) ('TOption ('TContract p) ': s)
  TRANSFER_TOKENS
    :: Instr (p ': 'Tc 'CMutez ': 'TContract p ': s)
                   ('TOperation ': s)
  SET_DELEGATE
    :: Instr ('TOption ('Tc 'CKeyHash) ': s) ('TOperation ': s)

  CREATE_ACCOUNT
    :: Instr
        ('Tc 'CKeyHash ': 'TOption ('Tc 'CKeyHash) ': 'Tc 'CBool
         ': 'Tc 'CMutez ': s) ('TOperation ': 'Tc 'CAddress ': s)

  CREATE_CONTRACT
    :: (SingI p, SingI g)
    => Instr
        ('Tc 'CKeyHash ': 'TOption ('Tc 'CKeyHash) ': 'Tc 'CBool
          ': 'Tc 'CBool ': 'Tc 'CMutez
          ': 'TLambda ('TPair p g)
                       ('TPair ('TList 'TOperation) g) ': g ': s)
        ('TOperation ': 'Tc 'CAddress ': s)
  CREATE_CONTRACT2
    :: (SingI p, SingI g)
    => Instr '[ 'TPair p g ] '[ 'TPair ('TList 'TOperation) g ]
    -> Instr ('Tc 'CKeyHash ':
              'TOption ('Tc 'CKeyHash) ':
              'Tc 'CBool ':
              'Tc 'CBool ':
              'Tc 'CMutez ':
               g ': s)
             ('TOperation ': 'Tc 'CAddress ': s)

  IMPLICIT_ACCOUNT
    :: Instr ('Tc 'CKeyHash ': s) ('TContract 'TUnit ': s)
  NOW :: Instr s ('Tc 'CTimestamp ': s)
  AMOUNT :: Instr s ('Tc 'CMutez ': s)
  BALANCE :: Instr s ('Tc 'CMutez ': s)
  CHECK_SIGNATURE
    :: Instr ('TKey ': 'TSignature ': 'Tc 'CBytes ': s)
                   ('Tc 'CBool ': s)
  SHA256 :: Instr ('Tc 'CBytes ': s) ('Tc 'CBytes ': s)
  SHA512 :: Instr ('Tc 'CBytes ': s) ('Tc 'CBytes ': s)
  BLAKE2B :: Instr ('Tc 'CBytes ': s) ('Tc 'CBytes ': s)
  HASH_KEY :: Instr ('TKey ': s) ('Tc 'CKeyHash ': s)
  STEPS_TO_QUOTA :: Instr s ('Tc 'CNat ': s)
  SOURCE :: Instr s ('Tc 'CAddress ': s)
  SENDER :: Instr s ('Tc 'CAddress ': s)
  ADDRESS :: Instr ('TContract a ': s) ('Tc 'CAddress ': s)

deriving instance Show (ExtT Instr) => Show (Instr inp out)

type Contract cp st = Instr (ContractInp cp st) (ContractOut st)
