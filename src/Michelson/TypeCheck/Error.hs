module Michelson.TypeCheck.Error
  ( TCTypeError (..)
  , TCError (..)
  , ExtError (..)
  , StackSize (..)
  ) where


import Fmt (Buildable(..), listF, pretty, (+|), (+||), (|+), (||+))
import qualified Text.Show (show)

import Michelson.ErrorPos (InstrCallStack)
import Michelson.TypeCheck.Types (SomeHST(..))
import qualified Michelson.Typed as T
import Michelson.Typed.Annotation (AnnConvergeError(..))
import Michelson.Typed.Extract (TypeConvergeError(..), toUType)
import Michelson.Typed.Print (buildStack)
import Michelson.Untyped (StackFn, Type, Var)
import qualified Michelson.Untyped as U

-- | Data type that represents various errors
-- which are related to type system.
-- These errors are used to specify info about type check errors
-- in @TCError@ data type.
data TCTypeError
  = AnnError AnnConvergeError
  -- ^ Annotation unify error
  | ExtractionTypeMismatch TypeConvergeError
  -- ^ Notes extraction error
  | TypeEqError T.T T.T
  -- ^ Type equality error
  | StackEqError [T.T] [T.T]
  -- ^ Stacks equality error
  | UnsupportedTypes [T.T]
  -- ^ Error that happens when some instruction doesn't
  -- have support for some types
  | UnknownType T.T
  -- ^ Error that happens when we meet unknown type
  deriving (Show, Eq)

instance Buildable TCTypeError where
  build (AnnError e) = build e
  build (ExtractionTypeMismatch e) = build e
  build (TypeEqError type1 type2) =
    "Types not equal: " +| type1 |+ " /= " +| type2 |+ ""
  build (StackEqError st1 st2) =
    "Stacks not equal: " +| buildStack st1 |+ " /= " +| buildStack st2 |+ ""
  build (UnsupportedTypes types) =
    "Unsupported types: " +| listF types |+ ""
  build (UnknownType t) =
    "Unknown type `" +| t |+ "`"

-- | Type check error
data TCError
  = TCFailedOnInstr U.ExpandedInstr SomeHST Text InstrCallStack (Maybe TCTypeError)
  | TCFailedOnValue U.Value T.T Text InstrCallStack (Maybe TCTypeError)
  | TCContractError Text (Maybe TCTypeError)
  | TCUnreachableCode InstrCallStack (NonEmpty U.ExpandedOp)
  | TCExtError SomeHST InstrCallStack ExtError
  deriving (Eq)

-- TODO pva701: an instruction position should be used in
-- Buildable instance within TM-151.
instance Buildable TCError where
  build = \case
    TCFailedOnInstr instr (SomeHST t) custom _ mbTCTypeError ->
      "Error checking expression "
      +| instr |+ " against input stack type "
      +| t ||+ bool (": " +| custom |+ " ") "" (null custom)
      +| (maybe "" (\e -> " " +| e |+ "") mbTCTypeError)
    TCFailedOnValue v t custom _ mbTCTypeError ->
      "Error checking value "
      +| v |+ " against type "
      +| toUType t |+ bool (": " +| custom |+ " ") "" (null custom)
      +| (maybe "" (\e -> " " +| e |+ "") mbTCTypeError)
    TCContractError msg typeError ->
      "Error occured during contract typecheck: "
      +|| msg ||+ (maybe "" (\e -> " " +| e |+ "") typeError)
    TCUnreachableCode _ instrs ->
      "Unreachable code: " +| take 3 (toList instrs) |+ " ..."
    TCExtError (SomeHST t) _ e ->
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
  | StkRestMismatch U.StackTypePattern SomeHST SomeHST TCTypeError
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
    StkRestMismatch stk (SomeHST r) (SomeHST r') e ->
      "StkRestMismatch in pattern " +| stk |+
      " against stacks " +| r ||+ " and " +| r' ||+
      " with error: " +| e |+ ""
    TestAssertError t ->
      "TestAssertError: " +| t |+ ""
    InvalidStackReference i lhs ->
      "InvalidStackReference: reference is out of the stack: "
      +| i ||+ " >= " +| lhs ||+ ""
