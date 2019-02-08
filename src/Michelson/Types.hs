{-# LANGUAGE DeriveDataTypeable #-}

module Michelson.Types
  (
  -- * Contract basics
    Parameter
  , Storage
  , Contract (..)

    -- * Data types
  , Value (..)
  , Elt (..)

  -- Typechecker types
  , InstrAbstract (..)
  , Instr
  , Op (..)

    -- * Michelson types
  , TypeNote
  , FieldNote
  , VarNote
  , Type (..)
  , Comparable (..)
  , T (..)
  , CT (..)
  ) where

import qualified Data.ByteString as B
import Data.Data (Data(..))
import qualified Data.Text as T

type Parameter = Type
type Storage = Type
data Contract op = Contract
  { para :: Parameter
  , stor :: Storage
  , code :: [op]
  } deriving (Eq, Show, Functor, Data)

-------------------------------------
-- Flattened types after macroexpander
-------------------------------------
type Instr = InstrAbstract Op
newtype Op = Op {unOp :: Instr}
  deriving (Eq, Show)

-------------------------------------
-- Basic polymorphic types for Parser/Macro/Typechecker modules
-------------------------------------

{- Data types -}
data Value op =
    Int     Integer
  | String  T.Text
  | Bytes   B.ByteString
  | Unit
  | True
  | False
  | Pair    (Value op) (Value op)
  | Left    (Value op)
  | Right   (Value op)
  | Some    (Value op)
  | None
  | Seq     [Value op]
  | Map     [Elt op]
  | DataOps [op]
  deriving (Eq, Show, Functor, Data)

data Elt op = Elt (Value op) (Value op)
  deriving (Eq, Show, Functor, Data)

data InstrAbstract op =
    DROP
  | DUP               VarNote
  | SWAP
  | PUSH              VarNote Type (Value op)
  | SOME              TypeNote VarNote FieldNote
  | NONE              TypeNote VarNote FieldNote Type
  | UNIT              TypeNote VarNote
  | IF_NONE           [op] [op]
  | PAIR              TypeNote VarNote FieldNote FieldNote
  | CAR               VarNote FieldNote
  | CDR               VarNote FieldNote
  | LEFT              TypeNote VarNote FieldNote FieldNote Type
  | RIGHT             TypeNote VarNote FieldNote FieldNote Type
  | IF_LEFT           [op] [op]
  | IF_RIGHT          [op] [op]
  | NIL               TypeNote VarNote Type
  | CONS              VarNote
  | IF_CONS           [op] [op]
  | SIZE              VarNote
  | EMPTY_SET         TypeNote VarNote Comparable
  | EMPTY_MAP         TypeNote VarNote Comparable Type
  | MAP               VarNote [op]
  | ITER              VarNote [op]
  | MEM               VarNote
  | GET               VarNote
  | UPDATE
  | IF                [op] [op]
  | LOOP              [op]
  | LOOP_LEFT         [op]
  | LAMBDA            VarNote Type Type [op]
  | EXEC              VarNote
  | DIP               [op]
  | FAILWITH
  | CAST              Type VarNote
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
  | CREATE_CONTRACT2  VarNote VarNote (Contract op)
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
  deriving (Eq, Show, Functor, Data)

-------------------------------------
-- Basic types for Michelson types --
-------------------------------------

{- Michelson Types -}
-- Type Annotations
type TypeNote = Maybe T.Text
type FieldNote = Maybe T.Text
type VarNote = Maybe T.Text

-- Annotated type
data Type = Type T TypeNote
  deriving (Eq, Show, Data)

-- Annotated Comparable Sub-type
data Comparable = Comparable CT TypeNote
  deriving (Eq, Show, Data)

-- Michelson Type
data T =
    T_comparable CT
  | T_key
  | T_unit
  | T_signature
  | T_option FieldNote Type
  | T_list Type
  | T_set Comparable
  | T_operation
  | T_contract Type
  | T_pair FieldNote FieldNote Type Type
  | T_or FieldNote FieldNote Type Type
  | T_lambda Type Type
  | T_map Comparable Type
  | T_big_map Comparable Type
  deriving (Eq, Show, Data)

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
  deriving (Eq, Show, Data)
