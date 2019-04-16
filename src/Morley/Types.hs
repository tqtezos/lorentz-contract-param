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

  , UExtInstrAbstract(..)

  -- * Morley Parsed instruction types
  , ParsedInstr
  , ParsedOp (..)
  , ParsedUTestAssert
  , ParsedUExtInstr

  -- * Morley Expanded instruction types
  , U.ExpandedInstr
  , U.ExpandedOp (..)
  , ExpandedUExtInstr

  -- * Michelson Instructions and Instruction Macros
  , PairStruct (..)
  , CadrStruct (..)
  , Macro (..)

  -- * Morley Instructions
  , ExtInstr(..)
  , TestAssert (..)
  , UTestAssert (..)
  , PrintComment (..)
  , UPrintComment (..)
  , StackTypePattern (..)
  , stackTypePatternToList
  , StackRef(..)
  , UStackRef(..)
  , mkStackRef

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
import Data.Singletons (Sing, SingI(..))
import qualified Data.Text as T
import Fmt (Buildable(build), Builder, genericF, listF, (+|), (+||), (|+), (||+))
import Text.Megaparsec (ParseErrorBundle, Parsec, ShowErrorComponent(..), errorBundlePretty)
import qualified Text.PrettyPrint.Leijen.Text as PP (empty)
import qualified Text.Show (show)

import Michelson.EqParam (eqParam1, eqParam2)
import Michelson.Printer (RenderDoc(..))
import Michelson.Typed (instrToOps)
import qualified Michelson.Typed as T
import qualified Michelson.Untyped as U
import Morley.Default (Default(..))
import Util.Peano
  (KnownPeano(..), LongerThan, Peano, RequireLongerThan, ToPeano, requiredLongerThan)

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
  | UPRINT UPrintComment         -- ^ Print a comment with optional embedded @StackRef@s
  deriving (Eq, Show, Data, Generic, Functor)

instance Buildable op => Buildable (UExtInstrAbstract op) where
  build = genericF

-- TODO replace ParsedOp in ExpandedUExtInstr with op
-- to reflect Parsed, Expanded and Flattened phase

type instance U.ExtU U.InstrAbstract = UExtInstrAbstract
type instance T.ExtT T.Instr = ExtInstr

---------------------------------------------------

type ParsedUTestAssert = UTestAssert ParsedOp

type ParsedUExtInstr = UExtInstrAbstract ParsedOp

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

type ExpandedUExtInstr = UExtInstrAbstract U.ExpandedOp

---------------------------------------------------

data TestAssert (s :: [T.T]) where
  TestAssert
    :: (Typeable out)
    => T.Text
    -> PrintComment inp
    -> T.Instr inp ('T.Tc 'T.CBool ': out)
    -> TestAssert inp

deriving instance Show (TestAssert s)

instance Typeable s => Eq (TestAssert s) where
  TestAssert   name1 pattern1 instr1
    ==
    TestAssert name2 pattern2 instr2
    = and
    [ name1 == name2
    , pattern1 `eqParam1` pattern2
    , instr1 `eqParam2` instr2
    ]

data ExtInstr s
  = TEST_ASSERT (TestAssert s)
  | PRINT (PrintComment s)
  deriving (Show, Eq)

instance T.Conversible (ExtInstr s) (UExtInstrAbstract U.ExpandedOp) where
  convert (PRINT pc) = UPRINT (T.convert pc)
  convert (TEST_ASSERT (TestAssert nm pc i)) =
    UTEST_ASSERT $ UTestAssert nm (T.convert pc) (instrToOps i)

-- | Morley interpreter state
newtype MorleyLogs = MorleyLogs
  { unMorleyLogs :: [T.Text]
  } deriving stock (Eq, Show)
    deriving newtype (Default, Buildable)

noMorleyLogs :: MorleyLogs
noMorleyLogs = MorleyLogs []

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
  = CMP ParsedInstr U.VarAnn
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

-- | A reference into the stack.
newtype UStackRef = UStackRef Natural
  deriving (Eq, Show, Data, Generic)

instance Buildable UStackRef where
  build (UStackRef i) = "%[" <> show i <> "]"

-- | A reference into the stack of a given type.
data StackRef (st :: [T.T]) where
  -- | Keeps 0-based index to a stack element counting from the top.
  StackRef
    :: (KnownPeano idx, LongerThan st idx)
    => Sing (idx :: Peano) -> StackRef st

instance Eq (StackRef st) where
  StackRef snat1 == StackRef snat2 = peanoVal snat1 == peanoVal snat2

instance Show (StackRef st) where
  show (StackRef snat) = "StackRef {" +|| peanoVal snat ||+ "}"

instance Buildable (StackRef st) where
  build = build @UStackRef . T.convert

instance T.Conversible (StackRef s) UStackRef where
  convert (StackRef n) = UStackRef (peanoVal n)

-- | Create a stack reference, performing checks at compile time.
mkStackRef
  :: forall (gn :: Nat) st n.
      (n ~ ToPeano gn, SingI n, KnownPeano n, RequireLongerThan st n)
  => StackRef st
mkStackRef = requiredLongerThan @st @n $ StackRef $ sing @(ToPeano gn)

newtype Var = Var T.Text deriving (Eq, Show, Ord, Data, Generic)

instance Buildable Var where
  build = genericF

-- | A type-variable or a type-constant
data TyVar =
    VarID Var
  | TyCon U.Type
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
  , lvSig :: U.Type
  , lvVal :: (U.Value' ParsedOp)
  } deriving (Eq, Show)

-- | A programmer-defined type-synonym
data LetType = LetType
  { ltName :: T.Text
  , ltSig :: U.Type
  } deriving (Eq, Show)

newtype UPrintComment = UPrintComment
  { unUPrintComment :: [Either T.Text UStackRef]
  } deriving (Eq, Show, Data, Generic)

instance Buildable UPrintComment where
  build = foldMap (either build build) . unUPrintComment

-- | A print format with references into the stack
newtype PrintComment (st :: [T.T]) = PrintComment
  { unPrintComment :: [Either T.Text (StackRef st)]
  } deriving (Eq, Show, Generic)

instance Buildable (PrintComment st) where
  build = build @UPrintComment . T.convert

instance T.Conversible (PrintComment s) UPrintComment where
  convert (PrintComment pc) = UPrintComment $ map (second T.convert) pc

-- An inline test assertion
data UTestAssert op = UTestAssert
  { tassName :: T.Text
  , tassComment :: UPrintComment
  , tassInstrs :: [op]
  } deriving (Eq, Show, Functor, Data, Generic)

instance Buildable code => Buildable (UTestAssert code) where
  build = genericF

deriveJSON defaultOptions ''ParsedOp
deriveJSON defaultOptions ''UExtInstrAbstract
deriveJSON defaultOptions ''UPrintComment
deriveJSON defaultOptions ''StackTypePattern
deriveJSON defaultOptions ''UStackRef
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
