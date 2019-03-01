{-# LANGUAGE DeriveDataTypeable, DerivingStrategies #-}

-- | Michelson instructions in untyped model.

module Michelson.Untyped.Instr
  ( InstrAbstract (..)
  , Instr
  , Op (..)
  ) where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Data (Data(..))

import Michelson.Untyped.Annotation (FieldAnn, TypeAnn, VarAnn)
import Michelson.Untyped.Contract (Contract)
import Michelson.Untyped.Type (Comparable, Type)
import Michelson.Untyped.Value (Value)

-------------------------------------
-- Flattened types after macroexpander
-------------------------------------
type Instr nop = InstrAbstract nop (Op nop)
newtype Op nop = Op {unOp :: Instr nop}
  deriving stock (Eq, Show, Generic)

-------------------------------------
-- Abstract instruction
-------------------------------------

-- | Michelson instruction with abstract parameter `op`.  This
-- parameter is necessary, because at different stages of our pipeline
-- it will be different. Initially it can contain macros and
-- non-flattened instructions, but then it contains only vanilla
-- Michelson instructions.
data InstrAbstract nop op
  = NOP nop
  | DROP
  | DUP               VarAnn
  | SWAP
  | PUSH              VarAnn Type (Value op)
  | SOME              TypeAnn VarAnn FieldAnn
  | NONE              TypeAnn VarAnn FieldAnn Type
  | UNIT              TypeAnn VarAnn
  | IF_NONE           [op] [op]
  | PAIR              TypeAnn VarAnn FieldAnn FieldAnn
  | CAR               VarAnn FieldAnn
  | CDR               VarAnn FieldAnn
  | LEFT              TypeAnn VarAnn FieldAnn FieldAnn Type
  | RIGHT             TypeAnn VarAnn FieldAnn FieldAnn Type
  | IF_LEFT           [op] [op]
  | IF_RIGHT          [op] [op]
  | NIL               TypeAnn VarAnn Type
  | CONS              VarAnn -- TODO add TypeNote param
  | IF_CONS           [op] [op]
  | SIZE              VarAnn
  | EMPTY_SET         TypeAnn VarAnn Comparable
  | EMPTY_MAP         TypeAnn VarAnn Comparable Type
  | MAP               VarAnn [op]
  | ITER              [op]
  | MEM               VarAnn
  | GET               VarAnn
  | UPDATE
  | IF                [op] [op]
  | LOOP              [op]
  | LOOP_LEFT         [op]
  | LAMBDA            VarAnn Type Type [op]
  -- TODO check on alphanet whether we can pass TypeNote
  | EXEC              VarAnn
  | DIP               [op]
  | FAILWITH
  | CAST              VarAnn Type
  | RENAME            VarAnn
  | PACK              VarAnn
  | UNPACK            VarAnn Type
  | CONCAT            VarAnn
  | SLICE             VarAnn
  | ISNAT             VarAnn
  | ADD               VarAnn
  | SUB               VarAnn
  | MUL               VarAnn
  | EDIV              VarAnn
  | ABS               VarAnn
  -- TODO why no varnote for NEG
  | NEG
  | LSL               VarAnn
  | LSR               VarAnn
  | OR                VarAnn
  | AND               VarAnn
  | XOR               VarAnn
  | NOT               VarAnn
  | COMPARE           VarAnn
  | EQ                VarAnn
  | NEQ               VarAnn
  | LT                VarAnn
  | GT                VarAnn
  | LE                VarAnn
  | GE                VarAnn
  | INT               VarAnn
  | SELF              VarAnn
  | CONTRACT          VarAnn Type
  | TRANSFER_TOKENS   VarAnn
  | SET_DELEGATE      VarAnn
  | CREATE_ACCOUNT    VarAnn VarAnn
  | CREATE_CONTRACT   VarAnn VarAnn
  | CREATE_CONTRACT2  VarAnn VarAnn (Contract op)
  | IMPLICIT_ACCOUNT  VarAnn
  | NOW               VarAnn
  | AMOUNT            VarAnn
  | BALANCE           VarAnn
  | CHECK_SIGNATURE   VarAnn
  | SHA256            VarAnn
  | SHA512            VarAnn
  | BLAKE2B           VarAnn
  | HASH_KEY          VarAnn
  | STEPS_TO_QUOTA    VarAnn
  | SOURCE            VarAnn
  | SENDER            VarAnn
  | ADDRESS           VarAnn
  deriving (Eq, Show, Functor, Data, Generic)

----------------------------------------------------------------------------
-- JSON serialization
----------------------------------------------------------------------------

deriveJSON defaultOptions ''Op
deriveJSON defaultOptions ''InstrAbstract
