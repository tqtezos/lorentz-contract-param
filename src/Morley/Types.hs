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
  , Annotation (..)
  , InternalByteString(..)
  , unInternalByteString

  -- Parser types
  , CustomParserException (..)
  , Parser
  , ParserException (..)

  -- * Typechecker types
  , ExpandedInstr
  , ExpandedOp (..)

  -- * Michelson Instructions and Instruction Macros
  , ParsedOp (..)
  , PairStruct (..)
  , CadrStruct (..)
  , Macro (..)
  , ParsedInstr

  -- * Morley Instructions
  , NopInstr(..)
  , InlineTest (..)
  , PrintComment (..)
  , StackTypePattern (..)
  , StackRef(..)
  ) where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Data (Data(..))
import qualified Data.Text as T
import Michelson.Untyped
  (Annotation(..), CT(..), Comparable(..), Contract(..), Elt(..), FieldAnn, Instr,
  InstrAbstract(..), InternalByteString(..), Op(..), Parameter, Storage, T(..), Type(..), TypeAnn,
  Value(..), VarAnn, ann, noAnn, unInternalByteString)
import Morley.Default (Default(..))
import Text.Megaparsec
import qualified Text.Show

-------------------------------------
-- Types for the parser
-------------------------------------

data CustomParserException
  = UnknownTypeException
  | OddNumberBytesException
  | UnexpectedLineBreak
  deriving (Eq, Data, Ord, Show)

instance ShowErrorComponent CustomParserException where
  showErrorComponent UnknownTypeException = "unknown type"
  showErrorComponent OddNumberBytesException = "odd number bytes"
  showErrorComponent UnexpectedLineBreak = "unexpected linebreak"

type Parser = Parsec CustomParserException T.Text
instance Default a => Default (Parser a) where
  def = pure def

data ParserException = ParserException (ParseErrorBundle T.Text CustomParserException)

instance Show ParserException where
  show (ParserException bundle) = errorBundlePretty bundle

instance Exception ParserException where
  displayException (ParserException bundle) = errorBundlePretty bundle

-------------------------------------
-- Types produced by parser
-------------------------------------
type ParsedInstr = InstrAbstract NopInstr ParsedOp
data ParsedOp
  = PRIM ParsedInstr
  | MAC Macro
  | SEQ [ParsedOp]
  deriving (Eq, Show, Data, Generic)

-- Mark a specific point in contract execution for the rest of the pipeline
data NopInstr =
    STACKTYPE StackTypePattern
  | TEST InlineTest
  | PRINT PrintComment
  deriving (Eq, Show, Data, Generic)

-- A stack pattern-match
data StackTypePattern
 = StkEmpty
 | StkRest
 | StkCons Type StackTypePattern
  deriving (Eq, Show, Data, Generic)

-- A print format with references into the stack
newtype PrintComment = PrintComment [Either T.Text StackRef]
  deriving (Eq, Show, Data, Generic)

newtype StackRef = StackRef Integer
  deriving (Eq, Show, Data, Generic)

-- An inline test assertion
data InlineTest = InlineTest
  { testName :: T.Text
  , testComment :: PrintComment
  , testInstrs :: [ParsedOp]
  } deriving (Eq, Show, Data, Generic)

-------------------------------------
-- Types after macroexpander
-------------------------------------
type ExpandedInstr = InstrAbstract NopInstr ExpandedOp
data ExpandedOp
  = PRIM_EX ExpandedInstr
  | SEQ_EX [ExpandedOp]
  deriving (Eq, Show, Data)

data PairStruct
  = F (VarAnn, FieldAnn)
  | P PairStruct PairStruct
  deriving (Eq, Show, Data, Generic)

data CadrStruct
  = A
  | D
  deriving (Eq, Show, Data, Generic)

data Macro
  = CMP ParsedInstr VarAnn
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
  deriving (Eq, Show, Data, Generic)

deriveJSON defaultOptions ''ParsedOp
deriveJSON defaultOptions ''NopInstr
deriveJSON defaultOptions ''PrintComment
deriveJSON defaultOptions ''StackTypePattern
deriveJSON defaultOptions ''StackRef
deriveJSON defaultOptions ''InlineTest
deriveJSON defaultOptions ''PairStruct
deriveJSON defaultOptions ''CadrStruct
deriveJSON defaultOptions ''Macro
