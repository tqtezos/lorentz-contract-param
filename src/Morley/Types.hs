{-# LANGUAGE DeriveDataTypeable #-}
module Morley.Types
  (
   -- * Rexported from Michelson.Types
    Parameter
  , Storage
  , Contract (..)
  , Value (..)
  , Elt (..)
  , InstrAbstract (..)
  , Instr
  , Op (..)
  , TypeNote
  , FieldNote
  , VarNote
  , Type (..)
  , Comparable (..)
  , T (..)
  , CT (..)

  -- * Typechecker types
  , ExpandedInstr
  , ExpandedOp (..)

  -- * Michelson Instructions and Instruction Macros
  , ParsedOp (..)
  , PairStruct (..)
  , CadrStruct (..)
  , Macro (..)
  , ParsedInstr
  ) where

import Michelson.Types
  (CT(..), Comparable(..), Contract(..), Elt(..), FieldNote, Instr, InstrAbstract(..), Op(..),
  Parameter, Storage, T(..), Type(..), TypeNote, Value(..), VarNote)

import Data.Data (Data(..))

-------------------------------------
-- Types produced by parser
-------------------------------------
type ParsedInstr = InstrAbstract ParsedOp
data ParsedOp =
    PRIM ParsedInstr
  | MAC Macro
  | SEQ [ParsedOp]
  deriving (Eq, Show)

-------------------------------------
-- Types after macroexpander
-------------------------------------
type ExpandedInstr = InstrAbstract ExpandedOp
data ExpandedOp =
    PRIM_EX ExpandedInstr
  | SEQ_EX [ExpandedOp]
  deriving (Eq, Show, Data)

data PairStruct = F (VarNote, FieldNote) | P PairStruct PairStruct
  deriving (Eq, Show)
data CadrStruct = A | D deriving (Eq, Show)

data Macro =
    CMP ParsedInstr VarNote
  | IFX ParsedInstr [ParsedOp] [ParsedOp]
  | IFCMP ParsedInstr VarNote [ParsedOp] [ParsedOp]
  | FAIL
  | PAPAIR PairStruct TypeNote VarNote
  | UNPAIR PairStruct
  | CADR [CadrStruct] VarNote FieldNote
  | SET_CADR [CadrStruct] VarNote FieldNote
  | MAP_CADR [CadrStruct] VarNote FieldNote [ParsedOp]
  | DIIP Integer [ParsedOp]
  | DUUP Integer VarNote
  | ASSERT
  | ASSERTX ParsedInstr
  | ASSERT_CMP ParsedInstr
  | ASSERT_NONE
  | ASSERT_SOME
  | ASSERT_LEFT
  | ASSERT_RIGHT
  | IF_SOME [ParsedOp] [ParsedOp]
  deriving (Eq, Show)
