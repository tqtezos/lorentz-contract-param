{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Michelson.Types where

import qualified Data.ByteString as B
import           Data.Maybe
import           Data.Natural
import           Data.Sequence   as Seq
import qualified Data.Text       as T
import           Prelude         (Eq, Integer, Ord, (++), (.))
import           Text.Show


{- Contract types -}
data Contract = Contract Parameter Storage Code deriving Show

data Parameter = Parameter Type deriving Show
data Storage = Storage Type deriving Show
data Code = Code Ops deriving Show

data Stack = Stack {elems :: Seq Data} deriving Show

{- Data types -}
data Data where
  Int        :: Integer -> Data
  String     :: T.Text -> Data
  Bytes      :: B.ByteString -> Data
  Unit       :: Data
  True       :: Data
  False      :: Data
  Pair       :: Data -> Data -> Data
  Left       :: Data -> Data
  Right      :: Data -> Data
  Some       :: Data -> Data
  None       :: Data
  Seq        :: Seq Data -> Data
  Map        :: Seq Elt -> Data
  DataOps    :: Ops -> Data
  deriving Show

data Elt = Elt Data Data deriving Show

{- Michelson Types -}

-- Type Annotations
type TypeNote = Maybe T.Text
type FieldNote = Maybe T.Text
type VarNote = Maybe T.Text

-- Annotated type
data Type = Type T TypeNote FieldNote deriving Show

-- Annotated Comparable Sub-type
data Comparable = Comparable CT TypeNote deriving Show

-- Michelson Type
data T =
    T_comparable CT
  | T_key
  | T_unit
  | T_signature
  | T_option Type
  | T_list Type
  | T_set Comparable
  | T_operation
  | T_contract Type
  | T_pair Type Type
  | T_or Type Type
  | T_lambda Type Type
  | T_map Comparable Type
  | T_big_map Comparable Type
  deriving Show

-- Comparable Sub-Type
data CT =
    T_int
  | T_nat
  | T_string
  | T_bytes
  | T_mutez
  | T_bool
  | T_key_hash
  | T_timestamp
  | T_address
  deriving Show

-- instruction sequence
data Ops = Ops { ops :: Seq Op } deriving Show
data Prims = Prims { instrs :: Seq I } deriving Show

{- Michelson Instructions and Instruction Macros -}

data PairStruct = Leaf | Nest PairStruct PairStruct deriving Show
data CadrStruct = A | D deriving Show

data Macro =
    CMP I
  | IFX I Ops Ops
  | IFCMP I Ops Ops
  | FAIL
  | PAPAIR PairStruct VarNote [FieldNote]
  | UNPAIR PairStruct [VarNote] [FieldNote]
  | CADR [CadrStruct] VarNote FieldNote
  | SET_CADR [CadrStruct] VarNote FieldNote
  | MAP_CADR [CadrStruct] VarNote FieldNote Ops
  | DIIP Integer Ops
  | DUUP Integer VarNote
  | ASSERT
  | ASSERTX I
  | ASSERT_CMP I
  | ASSERT_NONE
  | ASSERT_SOME
  | ASSERT_LEFT
  | ASSERT_RIGHT
  | IF_SOME Ops Ops
  deriving Show

data Op =
    PRIM I
  | MAC Macro
  | SEQ Ops
  deriving Show

data I =
    DROP
  | DUP VarNote
  | SWAP
  | PUSH VarNote Type Data
  | SOME TypeNote VarNote FieldNote
  | NONE TypeNote VarNote FieldNote Type
  | UNIT TypeNote VarNote
  | IF_NONE Ops Ops
  | PAIR              TypeNote VarNote FieldNote FieldNote
  | CAR               VarNote FieldNote
  | CDR               VarNote FieldNote
  | LEFT              TypeNote VarNote FieldNote FieldNote Type
  | RIGHT             TypeNote VarNote FieldNote FieldNote Type
  | IF_LEFT           Ops Ops
  | IF_RIGHT          Ops Ops
  | NIL               TypeNote VarNote Type
  | CONS              VarNote
  | IF_CONS           Ops Ops
  | SIZE              VarNote
  | EMPTY_SET         TypeNote VarNote Comparable
  | EMPTY_MAP         TypeNote VarNote Comparable Type
  | MAP               VarNote Ops
  | ITER              VarNote Ops
  | MEM               VarNote
  | GET               VarNote
  | UPDATE
  | IF                Ops Ops
  | LOOP              Ops
  | LOOP_LEFT         Ops
  | LAMBDA            VarNote Type Type Ops
  | EXEC              VarNote
  | DIP               Ops
  | FAILWITH
  | CAST              TypeNote VarNote
  | RENAME            VarNote
  | PACK              VarNote
  | UNPACK            VarNote Type
  | CONCAT            VarNote
  | SLICE             VarNote
  | ISNAT
  | ADD               VarNote
  | SUB               VarNote
  | MUL               VarNote
  | EDIV              VarNote
  | ABS               VarNote
  | NEG
  | MOD
  | LSL               VarNote
  | LSR               VarNote
  | OR                VarNote
  | AND               VarNote
  | XOR               VarNote
  | NOT               VarNote
  | COMPARE           VarNote
  | EQ                VarNote
  | NEQ               VarNote
  | LT                VarNote
  | GT                VarNote
  | LE                VarNote
  | GE                VarNote
  | INT               VarNote
  | SELF              VarNote
  | CONTRACT          Type
  | TRANSFER_TOKENS   VarNote
  | SET_DELEGATE
  | CREATE_ACCOUNT    VarNote VarNote
  | CREATE_CONTRACT   VarNote VarNote
  | CREATE_CONTRACT2  VarNote VarNote Contract
  | IMPLICIT_ACCOUNT  VarNote
  | NOW               VarNote
  | AMOUNT            VarNote
  | BALANCE           VarNote
  | CHECK_SIGNATURE   VarNote
  | SHA256            VarNote
  | SHA512            VarNote
  | BLAKE2B           VarNote
  | HASH_KEY          VarNote
  | STEPS_TO_QUOTA    VarNote
  | SOURCE            VarNote
  | SENDER            VarNote
  | ADDRESS           VarNote
  deriving Show

