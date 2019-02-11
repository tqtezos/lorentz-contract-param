{-# OPTIONS_GHC -Wno-orphans #-}
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
  , TypeAnn
  , FieldAnn
  , VarAnn
  , ann
  , noAnn
  , Type (..)
  , Comparable (..)
  , T (..)
  , CT (..)

  -- Parser types
  , Parser
  , ParserException(..)

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

import Data.Data (Data(..))
import qualified Data.Text as T
import Michelson.Types
  (CT(..), Comparable(..), Contract(..), Elt(..), FieldAnn, Instr, InstrAbstract(..), Op(..),
  Parameter, Storage, T(..), Type(..), TypeAnn, Value(..), VarAnn, ann, noAnn)
import Morley.Default (Default(..))
import Text.Megaparsec

-------------------------------------
-- Types for the parser
-------------------------------------

type Parser = Parsec Void T.Text
instance Default a => Default (Parser a)         where def = pure def

data ParserException = ParserException (ParseErrorBundle T.Text Void)
  deriving (Show)

instance Exception ParserException where
  displayException (ParserException bundle) = errorBundlePretty bundle

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

data PairStruct = F (VarAnn, FieldAnn) | P PairStruct PairStruct
  deriving (Eq, Show)
data CadrStruct = A | D deriving (Eq, Show)

data Macro =
    CMP ParsedInstr VarAnn
  | IFX ParsedInstr [ParsedOp] [ParsedOp]
  | IFCMP ParsedInstr VarAnn [ParsedOp] [ParsedOp]
  | FAIL
  | PAPAIR PairStruct TypeAnn VarAnn
  | UNPAIR PairStruct
  | CADR [CadrStruct] VarAnn FieldAnn
  | SET_CADR [CadrStruct] VarAnn FieldAnn
  | MAP_CADR [CadrStruct] VarAnn FieldAnn [ParsedOp]
  | DIIP Integer [ParsedOp]
  | DUUP Integer VarAnn
  | ASSERT
  | ASSERTX ParsedInstr
  | ASSERT_CMP ParsedInstr
  | ASSERT_NONE
  | ASSERT_SOME
  | ASSERT_LEFT
  | ASSERT_RIGHT
  | IF_SOME [ParsedOp] [ParsedOp]
  deriving (Eq, Show)
