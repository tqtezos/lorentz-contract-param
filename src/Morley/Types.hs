{-# LANGUAGE DeriveDataTypeable, DerivingStrategies #-}

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
import Fmt (Buildable(build), Builder, genericF, listF)
import Text.Megaparsec (ParseErrorBundle, Parsec, ShowErrorComponent(..), errorBundlePretty)
import Text.Show (show)

import Michelson.Untyped
  (Annotation(..), CT(..), Comparable(..), Contract(..), Elt(..), FieldAnn, Instr,
  InstrAbstract(..), InternalByteString(..), Op(..), Parameter, Storage, T(..), Type(..), TypeAnn,
  Value(..), VarAnn, ann, noAnn, unInternalByteString)
import Morley.Default (Default(..))

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

instance Buildable ParsedOp where
  build = genericF

-- Mark a specific point in contract execution for the rest of the pipeline
data NopInstr =
    STACKTYPE StackTypePattern
  | TEST InlineTest
  | PRINT PrintComment
  deriving (Eq, Show, Data, Generic)

instance Buildable NopInstr where
  build = genericF

-- A stack pattern-match
data StackTypePattern
 = StkEmpty
 | StkRest
 | StkCons Type StackTypePattern
  deriving (Eq, Show, Data, Generic)

-- | Convert 'StackTypePattern' to a list of types. Also returns
-- 'Bool' which is 'True' if the pattern is a fixed list of types and
-- 'False' if it's a pattern match on the head of the stack.
stackTypePatternToList :: StackTypePattern -> ([Type], Bool)
stackTypePatternToList StkEmpty = ([], True)
stackTypePatternToList StkRest = ([], False)
stackTypePatternToList (StkCons t pat) =
  first (t :) $ stackTypePatternToList pat

instance Buildable StackTypePattern where
  build = listF . pairToList . stackTypePatternToList
    where
      pairToList :: ([Type], Bool) -> [Builder]
      pairToList (types, fixed)
        | fixed = map build types
        | otherwise = map build types ++ ["..."]

-- A print format with references into the stack
newtype PrintComment = PrintComment
  { unPrintComment :: [Either T.Text StackRef]
  } deriving (Eq, Show, Data, Generic)

instance Buildable PrintComment where
  build = foldMap (either build build) . unPrintComment

newtype StackRef = StackRef Integer
  deriving (Eq, Show, Data, Generic)

instance Buildable StackRef where
  build (StackRef i) = "%[" <> build i <> "]"

-- An inline test assertion
data InlineTest = InlineTest
  { testName :: T.Text
  , testComment :: PrintComment
  , testInstrs :: [ParsedOp]
  } deriving (Eq, Show, Data, Generic)

instance Buildable InlineTest where
  build = genericF

-------------------------------------
-- Types after macroexpander
-------------------------------------
type ExpandedInstr = InstrAbstract NopInstr ExpandedOp
data ExpandedOp
  = PRIM_EX ExpandedInstr
  | SEQ_EX [ExpandedOp]
  deriving stock (Eq, Show, Data, Generic)

instance Buildable ExpandedOp where
  build = genericF

data PairStruct
  = F (VarAnn, FieldAnn)
  | P PairStruct PairStruct
  deriving (Eq, Show, Data, Generic)

instance Buildable PairStruct where
  build = genericF

data CadrStruct
  = A
  | D
  deriving (Eq, Show, Data, Generic)

instance Buildable CadrStruct where
  build = genericF

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

instance Buildable Macro where
  build = genericF

deriveJSON defaultOptions ''ParsedOp
deriveJSON defaultOptions ''NopInstr
deriveJSON defaultOptions ''PrintComment
deriveJSON defaultOptions ''StackTypePattern
deriveJSON defaultOptions ''StackRef
deriveJSON defaultOptions ''InlineTest
deriveJSON defaultOptions ''PairStruct
deriveJSON defaultOptions ''CadrStruct
deriveJSON defaultOptions ''Macro
