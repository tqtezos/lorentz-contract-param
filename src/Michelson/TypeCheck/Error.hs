module Michelson.TypeCheck.Error
  ( TCTypeError (..)
  , TCError (..)
  , ExtError (..)
  , StackSize (..)
  ) where


import Data.Data (TypeRep)
import Fmt (Buildable(..), pretty, (+|), (+||), (|+), (||+))
import qualified Text.Show (show)

import Michelson.TypeCheck.Types (SomeHST(..))
import qualified Michelson.Typed as T
import Michelson.Typed.Annotation (AnnConvergeError(..))
import Michelson.Typed.Extract (TypeConvergeError(..), toUType)
import qualified Michelson.Untyped as U

import Morley.Types

-- | Data type that represents various errors
-- which are related to type system.
-- These errors are used to specify info about type check errors
-- in @TCError@ data type.
data TCTypeError
  = AnnError AnnConvergeError
  -- ^ Annotation unify error
  | ExtractionTypeMismatch TypeConvergeError
  -- ^ Notes extraction error
  | TypeEqError TypeRep TypeRep
  -- ^ Type equality error
  | UnsupportedTypes [TypeRep]
  -- ^ Error that happens when some instruction doesn't
  -- have support for some types
  | UnknownType TypeRep
  -- ^ Error that happens when we meet unknown type
  deriving (Show, Eq)

instance Buildable TCTypeError where
  build (AnnError e)= build e
  build (ExtractionTypeMismatch e) = build e
  build (TypeEqError type1 type2) =
    "Types not equal: " +|| type1 ||+ " /= " +|| type2 ||+ ""
  build (UnsupportedTypes types) =
    "Unsupported types: " +| (intercalate ", " $ map show types) |+ ""
  build (UnknownType t) =
    "Unknown type" +|| t ||+ ""

-- | Type check error
data TCError
  = TCFailedOnInstr U.ExpandedInstr SomeHST Text (Maybe TCTypeError)
  | TCFailedOnValue U.Value T.T Text (Maybe TCTypeError)
  | TCContractError Text (Maybe TCTypeError)
  | TCUnreachableCode (NonEmpty U.ExpandedOp)
  | TCExtError SomeHST ExtError
  deriving (Eq)

instance Buildable TCError where
  build = \case
    TCFailedOnInstr instr (SomeHST t) custom mbTCTypeError ->
      "Error checking expression "
      +| instr |+ " against input stack type "
      +| t ||+ bool (": " +| custom |+ " ") "" (null custom)
      +| (maybe "" (\e -> " " +| e |+ "") mbTCTypeError)
    TCFailedOnValue v t custom mbTCTypeError ->
      "Error checking value "
      +| v |+ " against type "
      +| toUType t |+ bool (": " +| custom |+ " ") "" (null custom)
      +| (maybe "" (\e -> " " +| e |+ "") mbTCTypeError)
    TCContractError msg typeError ->
      "Error occured during contract typecheck: "
      +|| msg ||+ (maybe "" (\e -> " " +| e |+ "") typeError)
    TCUnreachableCode instrs ->
      "Unreachable code: " +| take 3 (toList instrs) |+ " ..."
    TCExtError (SomeHST t) e ->
      "Error occured during Morley extension typecheck: "
      +| e |+ " on stack " +| t ||+ ""

instance Buildable U.ExpandedInstr => Show TCError where
  show = pretty

instance Buildable U.ExpandedInstr => Exception TCError

newtype StackSize = StackSize Natural
  deriving (Show, Eq)

-- | Various type errors possible when checking Morley extension commands
data ExtError =
    LengthMismatch U.StackTypePattern
  | VarError Text StackFn
  | TypeMismatch U.StackTypePattern Int TCTypeError
  | TyVarMismatch Var Type U.StackTypePattern Int TCTypeError
  | FnEndMismatch (Maybe (U.ExpandedExtInstr, SomeHST))
  | StkRestMismatch U.StackTypePattern SomeHST SomeHST TCTypeError
  | UnexpectedUExt U.ExpandedExtInstr
  | TestAssertError Text
  | InvalidStackReference U.StackRef StackSize
  deriving (Eq)

instance Buildable ExtError where
  build = \case
    LengthMismatch stk ->
      "Unexpected length of stack: pattern "
      +| stk |+ " has length "
      +| (length . fst . U.stackTypePatternToList) stk |+ ""
    VarError t sf ->
      "In defenition of " +| t |+ ": VarError "
      +| sf |+ ""
    TypeMismatch stk i e ->
      "TypeMismatch: Pattern " +| stk |+ " at index "
      +| i |+ " with error: " +| e |+ ""
    TyVarMismatch v t stk i e ->
      "TyVarMismach: Variable " +| v |+ " is bound to type "
      +| t |+ " but pattern " +| stk |+ " failed at index "
      +| i |+ " with error: " +| e |+ ""
    FnEndMismatch n ->
      "FnEndMismatch " +| (maybe "" show n)
    StkRestMismatch stk (SomeHST r) (SomeHST r') e ->
      "StkRestMismatch in pattern " +| stk |+
      " against stacks " +| r ||+ " and " +| r' ||+
      " with error: " +| e |+ ""
    UnexpectedUExt expr ->
      "UnexpectedUExt " +| expr |+ ""
    TestAssertError t ->
      "TestAssertError: " +| t |+ ""
    InvalidStackReference i lhs ->
      "InvalidStackReference: reference is out of the stack: "
      +| i ||+ " >= " +| lhs ||+ ""
