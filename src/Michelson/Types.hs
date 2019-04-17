{-# OPTIONS_GHC -Wno-orphans #-}
-- pva701: ^ this is needed for Buildable Instr.
-- I couldn't define it in Utyped.Instr
-- because GHC doesn't know what particular type would be ExtU
-- and generates an error about overlapping instances,
-- when I try to use genericF.
-- Also it's not possible to implement it using deriving instances
-- because InstrAbstract isn't newtype.
{-# LANGUAGE DeriveDataTypeable, DerivingStrategies #-}

module Michelson.Types
  (
   -- * Rexported from Michelson.Untyped.Types
    U.Parameter
  , U.Storage
  , U.Contract' (..)
  , U.Contract
  , U.Value' (..)
  , U.Value
  , U.Elt (..)
  , U.InstrAbstract (..)
  , U.Instr
  , U.Op (..)
  , U.TypeAnn
  , U.FieldAnn
  , U.VarAnn
  , U.ann
  , U.noAnn
  , U.Type (..)
  , U.Comparable (..)
  , U.T (..)
  , U.CT (..)
  , U.Annotation (..)
  , U.InternalByteString(..)
  , U.unInternalByteString

  -- Parser types
  , CustomParserException (..)
  , Parser
  , Parsec
  , ParseErrorBundle
  , ParserException (..)
  , ParsedValue
  , LetEnv (..)
  , noLetEnv

  -- * Morley Parsed instruction types
  , ParsedInstr
  , ParsedOp (..)
  , ParsedUTestAssert
  , ParsedUExtInstr

  -- * Morley Expanded instruction types
  , U.ExpandedInstr
  , U.ExpandedOp (..)

  -- * Michelson Instructions and Instruction Macros
  , PairStruct (..)
  , CadrStruct (..)
  , Macro (..)

  --  * Let-block
  , U.StackFn(..)
  , U.Var (..)
  , U.TyVar (..)
  , U.varSet
  , LetMacro (..)
  , LetValue (..)
  , LetType (..)
  ) where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Data (Data(..))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import Fmt (Buildable(build), genericF, (+|), (|+))
import Text.Megaparsec (ParseErrorBundle, Parsec, ShowErrorComponent(..), errorBundlePretty)
import qualified Text.PrettyPrint.Leijen.Text as PP (empty)
import qualified Text.Show (show)

import Michelson.Printer (RenderDoc(..))
import qualified Michelson.Untyped as U
import Util.Default (Default(..))

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

type Parser = ReaderT LetEnv (Parsec CustomParserException T.Text)

instance Default a => Default (Parser a) where
  def = pure def

data ParserException =
  ParserException (ParseErrorBundle T.Text CustomParserException)
  deriving (Eq)

instance Show ParserException where
  show (ParserException bundle) = errorBundlePretty bundle

instance Exception ParserException where
  displayException (ParserException bundle) = errorBundlePretty bundle

instance Buildable ParserException where
  build = build @String . show

-- | The environment containing lets from the let-block
data LetEnv = LetEnv
  { letMacros :: Map Text LetMacro
  , letValues :: Map Text LetValue
  , letTypes  :: Map Text LetType
  } deriving (Show, Eq)

noLetEnv :: LetEnv
noLetEnv = LetEnv Map.empty Map.empty Map.empty

-------------------------------------
-- Types produced by parser
-------------------------------------

type ParsedUTestAssert = U.TestAssert ParsedOp

type ParsedUExtInstr = U.ExtInstrAbstract ParsedOp

type ParsedInstr = U.InstrAbstract ParsedOp

type ParsedValue = U.Value' ParsedOp

-- | Unexpanded instructions produced directly by the @ops@ parser, which
-- contains primitive Michelson Instructions, inline-able macros and sequences
data ParsedOp
  = Prim ParsedInstr -- ^ Primitive Michelson instruction
  | Mac Macro        -- ^ Built-in Michelson macro defined by the specification
  | LMac LetMacro    -- ^ User-defined macro with instructions to be inlined
  | Seq [ParsedOp]   -- ^ A sequence of instructions
  deriving (Eq, Show, Data, Generic)

-- dummy value
instance RenderDoc ParsedOp where
  renderDoc _ = PP.empty

instance Buildable ParsedOp where
  build (Prim parseInstr) = "<Prim: "+|parseInstr|+">"
  build (Mac macro)       = "<Mac: "+|macro|+">"
  build (LMac letMacro)   = "<LMac: "+|letMacro|+">"
  build (Seq parsedOps)   = "<Seq: "+|parsedOps|+">"

---------------------------------------------------

data PairStruct
  = F (U.VarAnn, U.FieldAnn)
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

-- | Built-in Michelson Macros defined by the specification
data Macro
  = CASE (NonEmpty [ParsedOp])
  | VIEW [ParsedOp]
  | VOID [ParsedOp]
  | CMP ParsedInstr U.VarAnn
  | IFX ParsedInstr [ParsedOp] [ParsedOp]
  | IFCMP ParsedInstr U.VarAnn [ParsedOp] [ParsedOp]
  | FAIL
  | PAPAIR PairStruct U.TypeAnn U.VarAnn
  | UNPAIR PairStruct
  | CADR [CadrStruct] U.VarAnn U.FieldAnn
  | SET_CADR [CadrStruct] U.VarAnn U.FieldAnn
  | MAP_CADR [CadrStruct] U.VarAnn U.FieldAnn [ParsedOp]
  | DIIP Integer [ParsedOp]
  | DUUP Integer U.VarAnn
  | ASSERT
  | ASSERTX ParsedInstr
  | ASSERT_CMP ParsedInstr
  | ASSERT_NONE
  | ASSERT_SOME
  | ASSERT_LEFT
  | ASSERT_RIGHT
  | IF_SOME [ParsedOp] [ParsedOp]
  | IF_RIGHT [ParsedOp] [ParsedOp]
  deriving (Eq, Show, Data, Generic)

instance Buildable Macro where
  build (CASE parsedInstrs) = "<CASE: "+|toList parsedInstrs|+">"
  build (VIEW code) = "<VIEW: "+|code|+">"
  build (VOID code) = "<VOID: "+|code|+">"
  build (CMP parsedInstr carAnn) = "<CMP: "+|parsedInstr|+", "+|carAnn|+">"
  build (IFX parsedInstr parsedOps1 parsedOps2) = "<IFX: "+|parsedInstr|+", "+|parsedOps1|+", "+|parsedOps2|+">"
  build (IFCMP parsedInstr varAnn parsedOps1 parsedOps2) = "<IFCMP: "+|parsedInstr|+", "+|varAnn|+", "+|parsedOps1|+", "+|parsedOps2|+">"
  build FAIL = "FAIL"
  build (PAPAIR pairStruct typeAnn varAnn) = "<PAPAIR: "+|pairStruct|+", "+|typeAnn|+", "+|varAnn|+">"
  build (UNPAIR pairStruct) = "<UNPAIR: "+|pairStruct|+">"
  build (CADR cadrStructs varAnn fieldAnn) = "<CADR: "+|cadrStructs|+", "+|varAnn|+", "+|fieldAnn|+">"
  build (SET_CADR cadrStructs varAnn fieldAnn) = "<SET_CADR: "+|cadrStructs|+", "+|varAnn|+", "+|fieldAnn|+">"
  build (MAP_CADR cadrStructs varAnn fieldAnn parsedOps) = "<MAP_CADR: "+|cadrStructs|+", "+|varAnn|+", "+|fieldAnn|+", "+|parsedOps|+">"
  build (DIIP integer parsedOps) = "<DIIP: "+|integer|+", "+|parsedOps|+">"
  build (DUUP integer varAnn) = "<DUUP: "+|integer|+", "+|varAnn|+">"
  build ASSERT = "ASSERT"
  build (ASSERTX parsedInstr) = "<ASSERTX: "+|parsedInstr|+">"
  build (ASSERT_CMP parsedInstr) = "<ASSERT_CMP: "+|parsedInstr|+">"
  build ASSERT_NONE  = "ASSERT_NONE"
  build ASSERT_SOME  = "ASSERT_SOME"
  build ASSERT_LEFT  = "ASSERT_LEFT"
  build ASSERT_RIGHT = "ASSERT_RIGHT"
  build (IF_SOME parsedOps1 parsedOps2) = "<IF_SOME: "+|parsedOps1|+", "+|parsedOps2|+">"
  build (IF_RIGHT parsedOps1 parsedOps2) = "<IF_RIGHT: "+|parsedOps1|+", "+|parsedOps2|+">"

---------------------------------------------------

-- | A programmer-defined macro
data LetMacro = LetMacro
  { lmName :: T.Text
  , lmSig :: U.StackFn
  , lmExpr :: [ParsedOp]
  } deriving (Eq, Show, Data, Generic)

instance Buildable LetMacro where
  build = genericF

-- | A programmer-defined constant
data LetValue = LetValue
  { lvName :: T.Text
  , lvSig :: U.Type
  , lvVal :: (U.Value' ParsedOp)
  } deriving (Eq, Show)

-- | A programmer-defined type-synonym
data LetType = LetType
  { ltName :: T.Text
  , ltSig :: U.Type
  } deriving (Eq, Show)

deriveJSON defaultOptions ''ParsedOp
deriveJSON defaultOptions ''LetMacro
deriveJSON defaultOptions ''LetValue
deriveJSON defaultOptions ''LetType
deriveJSON defaultOptions ''PairStruct
deriveJSON defaultOptions ''CadrStruct
deriveJSON defaultOptions ''Macro
