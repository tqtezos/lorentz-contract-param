{-# LANGUAGE NoImplicitPrelude #-}

module Language.Michelson.Types where

import qualified Data.ByteString as B
import           Data.Maybe
import           Data.Natural
import qualified Data.Text       as T
import           Prelude         (Eq, Integer, Ord, undefined, (++), (.))
import           Text.Show

{- Contract types -}
data Contract = Contract
  { para :: Parameter
  , stor :: Storage
  , code :: Code
  } deriving (Eq, Show)

type Parameter = Type
type Storage = Type
type Code = [Op]

newtype Stack = Stack {elems :: [Data]} deriving Show

{- Data types -}
data Data =
    Int     Integer
  | String  T.Text
  | Bytes   B.ByteString
  | Unit
  | True
  | False
  | Pair    Data Data
  | Left    Data
  | Right   Data
  | Some    Data
  | None
  | Seq     [Data]
  | Map     [Elt]
  | DataOps [Op]
  deriving (Eq, Show)

data Elt = Elt Data Data deriving (Eq, Show)

{- Michelson Types -}
-- Type Annotations
type TypeNote = Maybe T.Text
type FieldNote = Maybe T.Text
type VarNote = Maybe T.Text

-- Annotated type
data Type = Type T TypeNote FieldNote deriving (Eq, Show)

-- Annotated Comparable Sub-type
data Comparable = Comparable CT TypeNote deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

{- Michelson Instructions and Instruction Macros -}
data Op =
    PRIM I
  | MAC Macro
  | SEQ [Op]
  deriving (Eq, Show)

data PairStruct = F (VarNote, FieldNote) | P PairStruct PairStruct
  deriving (Eq, Show)
data CadrStruct = A | D deriving (Eq, Show)

data Macro =
    CMP I VarNote
  | IFX I [Op] [Op]
  | IFCMP I VarNote [Op] [Op]
  | FAIL
  | PAPAIR PairStruct TypeNote VarNote
  | UNPAIR PairStruct
  | CADR [CadrStruct] VarNote FieldNote
  | SET_CADR [CadrStruct] VarNote FieldNote
  | MAP_CADR [CadrStruct] VarNote FieldNote [Op]
  | DIIP Integer [Op]
  | DUUP Integer VarNote
  | ASSERT
  | ASSERTX I
  | ASSERT_CMP I
  | ASSERT_NONE
  | ASSERT_SOME
  | ASSERT_LEFT
  | ASSERT_RIGHT
  | IF_SOME [Op] [Op]
  deriving (Eq, Show)

data I =
    DROP
  | DUP VarNote
  | SWAP
  | PUSH              VarNote Type Data
  | SOME              TypeNote VarNote FieldNote
  | NONE              TypeNote VarNote FieldNote Type
  | UNIT              TypeNote VarNote
  | IF_NONE           [Op] [Op]
  | PAIR              TypeNote VarNote FieldNote FieldNote
  | CAR               VarNote FieldNote
  | CDR               VarNote FieldNote
  | LEFT              TypeNote VarNote FieldNote FieldNote Type
  | RIGHT             TypeNote VarNote FieldNote FieldNote Type
  | IF_LEFT           [Op] [Op]
  | IF_RIGHT          [Op] [Op]
  | NIL               TypeNote VarNote Type
  | CONS              VarNote
  | IF_CONS           [Op] [Op]
  | SIZE              VarNote
  | EMPTY_SET         TypeNote VarNote Comparable
  | EMPTY_MAP         TypeNote VarNote Comparable Type
  | MAP               VarNote [Op]
  | ITER              VarNote [Op]
  | MEM               VarNote
  | GET               VarNote
  | UPDATE
  | IF                [Op] [Op]
  | LOOP              [Op]
  | LOOP_LEFT         [Op]
  | LAMBDA            VarNote Type Type [Op]
  | EXEC              VarNote
  | DIP               [Op]
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
  deriving (Eq, Show)

