{-# OPTIONS_GHC -Wno-orphans #-}
-- pva701: ^ this is needed for Buildable Instr.
-- I couldn't define it in Utyped.Instr
-- because GHC doesn't know what particular type would be ExtU
-- and generates an error about overlapping instances,
-- when I try to use genericF.
-- Also it's not possible to implement it using deriving instances
-- because InstrAbstract isn't newtype.
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
  , Parsec
  , ParseErrorBundle
  , ParserException (..)
  , LetEnv (..)
  , noLetEnv

  , UExtInstrAbstract(..)

  -- * Morley Parsed instruction types
  , ParsedInstr
  , ParsedOp (..)
  , ParsedUTestAssert
  , ParsedUExtInstr

  -- * Morley Expanded instruction types
  , ExpandedInstr
  , ExpandedOp (..)
  , UExtInstr

  -- * Michelson Instructions and Instruction Macros
  , PairStruct (..)
  , CadrStruct (..)
  , Macro (..)

  -- * Morley Instructions
  , ExtInstr(..)
  , TestAssert (..)
  , UTestAssert (..)
  , PrintComment (..)
  , StackTypePattern (..)
  , StackRef(..)

  , MorleyLogs (..)
  , noMorleyLogs
  --  * Let-block
  , StackFn(..)
  , Var (..)
  , TyVar (..)
  , varSet
  , LetMacro (..)
  , LetValue (..)
  , LetType (..)
  ) where


import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Data (Data(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Fmt (Buildable(build), Builder, genericF, listF)
import Text.Megaparsec (ParseErrorBundle, Parsec, ShowErrorComponent(..), errorBundlePretty)
import qualified Text.Show (show)

import Michelson.Typed (instrToOps)
import qualified Michelson.Typed as T
import Michelson.Untyped
  (Annotation(..), CT(..), Comparable(..), Contract(..), Elt(..), ExtU, FieldAnn, Instr,
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

type Parser = ReaderT LetEnv (Parsec CustomParserException T.Text)

instance Default a => Default (Parser a) where
  def = pure def

data ParserException =
  ParserException (ParseErrorBundle T.Text CustomParserException)

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

-- TODO Move this to Morley.Untyped
-- | Implementation-specific instructions embedded in a @NOP@ primitive, which
-- mark a specific point during a contract's typechecking or execution.
--
-- These instructions are not allowed to modify the contract's stack, but may
-- impose additional constraints that can cause a contract to report errors in
-- type-checking or testing.
--
-- Additionaly, some implementation-specific language features such as
-- type-checking of @LetMacro@s are implemented using this mechanism
-- (specifically @FN@ and @FN_END@).
data UExtInstrAbstract op =
    STACKTYPE StackTypePattern -- ^ Matches current stack against a type-pattern
  | FN T.Text StackFn          -- ^ Begin a typed stack function (push a @TcExtFrame@)
  | FN_END                     -- ^ End a stack function (pop a @TcExtFrame@)
  | UTEST_ASSERT (UTestAssert op)   -- ^ Copy the current stack and run an inline assertion on it
  | UPRINT PrintComment         -- ^ Print a comment with optional embedded @StackRef@s
  deriving (Eq, Show, Data, Generic, Functor)

instance Buildable op => Buildable (UExtInstrAbstract op) where
  build = genericF

-- TODO replace ParsedOp in UExtInstr with op
-- to reflect Parsed, Epxanded and Flattened phase

type instance ExtU InstrAbstract = UExtInstrAbstract
type instance T.ExtT T.Instr = ExtInstr

---------------------------------------------------

type ParsedUTestAssert = UTestAssert ParsedOp

type ParsedUExtInstr = UExtInstrAbstract ParsedOp

type ParsedInstr = InstrAbstract ParsedOp

-- | Unexpanded instructions produced directly by the @ops@ parser, which
-- contains primitive Michelson Instructions, inline-able macros and sequences
data ParsedOp
  = PRIM ParsedInstr -- ^ Primitive Michelson instruction
  | MAC Macro        -- ^ Built-in Michelson macro defined by the specification
  | LMAC LetMacro    -- ^ User-defined macro with instructions to be inlined
  | SEQ [ParsedOp]   -- ^ A sequence of instructions
  deriving (Eq, Show, Data, Generic)

instance Buildable ParsedInstr where
  build = genericF

instance Buildable ParsedOp where
  build = genericF

-------------------------------------
-- Types after macroexpander
-------------------------------------

type ExpandedInstr = InstrAbstract ExpandedOp

data ExpandedOp
  = PRIM_EX ExpandedInstr
  | SEQ_EX [ExpandedOp]
  deriving stock (Eq, Show, Data, Generic)

instance Buildable ExpandedInstr where
  build = genericF

instance Buildable ExpandedOp where
  build = genericF

type UExtInstr = UExtInstrAbstract Op

instance Buildable Instr where
  build = genericF

---------------------------------------------------

data TestAssert where
  TestAssert
    :: (Typeable inp, Typeable out)
    => T.Text
    -> PrintComment
    -> T.Instr inp ('T.Tc 'CBool ': out)
    -> TestAssert

deriving instance Show TestAssert

data ExtInstr
  = TEST_ASSERT TestAssert
  | PRINT PrintComment

instance T.Conversible ExtInstr (UExtInstrAbstract Op) where
  convert (PRINT pc) = UPRINT pc
  convert (TEST_ASSERT (TestAssert nm pc i)) =
    UTEST_ASSERT $ UTestAssert nm pc (instrToOps i)

deriving instance Show ExtInstr

---------------------------------------------------

-- | Morley interpreter state
newtype MorleyLogs = MorleyLogs
  { unMorleyLogs :: [T.Text]
  } deriving stock (Eq, Show)
    deriving newtype (Default, Buildable)

noMorleyLogs :: MorleyLogs
noMorleyLogs = MorleyLogs []

---------------------------------------------------

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

-- | Built-in Michelson Macros defined by the specification
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

---------------------------------------------------

-- | A reference into the stack
newtype StackRef = StackRef Integer
  deriving (Eq, Show, Data, Generic)

instance Buildable StackRef where
  build (StackRef i) = "%[" <> build i <> "]"

newtype Var = Var T.Text deriving (Eq, Show, Ord, Data, Generic)

instance Buildable Var where
  build = genericF

-- | A type-variable or a type-constant
data TyVar =
    VarID Var
  | TyCon Type
  deriving (Eq, Show, Data, Generic)

instance Buildable TyVar where
  build = genericF

-- | A stack pattern-match
data StackTypePattern
 = StkEmpty
 | StkRest
 | StkCons TyVar StackTypePattern
  deriving (Eq, Show, Data, Generic)

-- | Convert 'StackTypePattern' to a list of types. Also returns
-- 'Bool' which is 'True' if the pattern is a fixed list of types and
-- 'False' if it's a pattern match on the head of the stack.
stackTypePatternToList :: StackTypePattern -> ([TyVar], Bool)
stackTypePatternToList StkEmpty = ([], True)
stackTypePatternToList StkRest = ([], False)
stackTypePatternToList (StkCons t pat) =
  first (t :) $ stackTypePatternToList pat

instance Buildable StackTypePattern where
  build = listF . pairToList . stackTypePatternToList
    where
      pairToList :: ([TyVar], Bool) -> [Builder]
      pairToList (types, fixed)
        | fixed = map build types
        | otherwise = map build types ++ ["..."]

-- | A stack function that expresses the type signature of a @LetMacro@
data StackFn = StackFn
  { quantifiedVars :: Maybe (Set Var)
  , inPattern :: StackTypePattern
  , outPattern :: StackTypePattern
  } deriving (Eq, Show, Data, Generic)

instance Buildable StackFn where
  build = genericF

-- | Get the set of variables in a stack pattern
varSet :: StackTypePattern -> Set Var
varSet StkEmpty = Set.empty
varSet StkRest = Set.empty
varSet (StkCons (VarID v) stk) = v `Set.insert` (varSet stk)
varSet (StkCons _ stk) = varSet stk

-- | A programmer-defined macro
data LetMacro = LetMacro
  { lmName :: T.Text
  , lmSig :: StackFn
  , lmExpr :: [ParsedOp]
  } deriving (Eq, Show, Data, Generic)

instance Buildable LetMacro where
  build = genericF

-- | A programmer-defined constant
data LetValue = LetValue
  { lvName :: T.Text
  , lvSig :: Type
  , lvVal :: (Value ParsedOp)
  } deriving (Eq, Show)

-- | A programmer-defined type-synonym
data LetType = LetType
  { ltName :: T.Text
  , ltSig :: Type
  } deriving (Eq, Show)

-- A print format with references into the stack
newtype PrintComment = PrintComment
  { unPrintComment :: [Either T.Text StackRef]
  } deriving (Eq, Show, Data, Generic)

instance Buildable PrintComment where
  build = foldMap (either build build) . unPrintComment

-- An inline test assertion
data UTestAssert op = UTestAssert
  { tassName :: T.Text
  , tassComment :: PrintComment
  , tassInstrs :: [op]
  } deriving (Eq, Show, Functor, Data, Generic)

instance Buildable code => Buildable (UTestAssert code) where
  build = genericF

deriveJSON defaultOptions ''ParsedOp
deriveJSON defaultOptions ''UExtInstrAbstract
deriveJSON defaultOptions ''PrintComment
deriveJSON defaultOptions ''StackTypePattern
deriveJSON defaultOptions ''StackRef
deriveJSON defaultOptions ''StackFn
deriveJSON defaultOptions ''Var
deriveJSON defaultOptions ''TyVar
deriveJSON defaultOptions ''LetMacro
deriveJSON defaultOptions ''LetValue
deriveJSON defaultOptions ''LetType
deriveJSON defaultOptions ''UTestAssert
deriveJSON defaultOptions ''PairStruct
deriveJSON defaultOptions ''CadrStruct
deriveJSON defaultOptions ''Macro
