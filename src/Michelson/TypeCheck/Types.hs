module Michelson.TypeCheck.Types
    ( HST (..)
    , (-:&)
    , SomeHST (..)
    , SomeInstrOut (..)
    , SomeInstr (..)
    , SomeValue (..)
    , SomeContract (..)
    , SomeCValue (..)
    , BoundVars (..)
    , TcExtFrames
    , noBoundVars
    ) where

import Data.Singletons (SingI)
import Prelude hiding (EQ, GT, LT)
import qualified Text.Show
import qualified Data.Map.Lazy as Map

import Michelson.EqParam (eqParam1)
import Michelson.Untyped (Var, Type, noAnn)
import Michelson.Typed
  (HasNoBigMap, HasNoOp, Notes(..), Sing(..), BigMapConstraint, T(..), fromSingT)
import qualified Michelson.Typed as T
import Michelson.Typed.Instr
import Michelson.Typed.Value

import Michelson.Untyped.Annotation (VarAnn)

-- | Data type holding type information for stack (Heterogeneous Stack Type).
--
-- This data type is used along with instruction data type @Instr@
-- to carry information about its input and output stack types.
--
-- That is, if there is value @instr :: Instr inp out@, along with this
-- @instr@ one may carry @inpHST :: HST inp@ and @outHST :: HST out@ which will
-- contain whole information about input and output stack types for @instr@.
--
-- Data type @HST@ is very similar to @Data.Vinyl.Rec@,
-- but is specialized for a particular purpose.
-- In particular, definition of @HST (t1 ': t2 ': ... tn ': '[])@ requires
-- constraints @(Typeable t1, Typeable t2, ..., Typeable tn)@ as well as
-- constraints @(Typeable '[ t1 ], Typeable '[ t1, t2 ], ...)@.
-- These applications of @Typeable@ class are required for convenient usage
-- of type encoded by @HST ts@ with some functions from @Data.Typeable@.
--
-- Data type @HST@ (Heterogeneous Stack Type) is a heterogenuous list of triples.
-- First element of triple is a type singleton which is due to main motivation
-- behind @HST@, namely for it to be used as representation of @Instr@ type
-- data for pattern-matching.
-- Second element of triple is a structure, holding field and type annotations
-- for a given type.
-- Third element of triple is an optional variable annotation for the stack
-- element.
data HST (ts :: [T])  where
  SNil :: HST '[]
  (::&) :: (Typeable xs, Typeable x, SingI x)
        => (Sing x, Notes x, VarAnn)
        -> HST xs
        -> HST (x ': xs)

instance Show (HST ts) where
  show SNil = "[]"
  show (r ::& rs) = "[ " <> showDo (r ::& rs) <> " ]"
    where
      showDo :: HST (t ': ts_) -> String
      showDo ((a, _notes, _vn) ::& (b ::& c)) =
          show (fromSingT a) <> ", " <> showDo (b ::& c)
      showDo ((a, _notes, _vn) ::& SNil) = show (fromSingT a)

infixr 7 ::&

instance Eq (HST ts) where
  SNil == SNil = True
  (_, n1, a1) ::& h1 == (_, n2, a2) ::& h2 =
    n1 == n2 && a1 == a2 && h1 == h2

-- | Append a type to 'HST', assuming that notes and annotations
-- for this type are unknown.
(-:&)
  :: (Typeable xs, Typeable x, SingI x)
  => Sing x
  -> HST xs
  -> HST (x ': xs)
s -:& hst = (s, NStar, noAnn) ::& hst
infixr 7 -:&

-- | No-argument type wrapper for @HST@ data type.
data SomeHST where
  SomeHST :: Typeable ts => HST ts -> SomeHST

deriving instance Show SomeHST

instance Eq SomeHST where
  SomeHST hst1 == SomeHST hst2 = hst1 `eqParam1` hst2

-- | This data type keeps part of type check result - instruction and
-- corresponding output stack.
data SomeInstrOut inp where
  -- | Type-check result with concrete output stack, most common case.
  --
  -- Output stack type is wrapped inside the type and @Typeable@
  -- constraint is provided to allow convenient unwrapping.
  (:::)
    :: (Typeable out)
    => Instr inp out
    -> HST out
    -> SomeInstrOut inp

  -- | Type-check result which matches against arbitrary output stack.
  -- Information about annotations in the output stack is absent.
  --
  -- This case is only possible when the corresponding code terminates
  -- with @FAILWITH@ instruction in all possible executions.
  -- The opposite may be not true though (example: you push always-failing
  -- lambda and immediatelly execute it - stack type is known).
  AnyOutInstr
    :: (forall out. Instr inp out)
    -> SomeInstrOut inp
infix 9 :::

instance Show (ExtInstr inp) => Show (SomeInstrOut inp) where
  show (i ::: out) = show i <> " :: " <> show out
  show (AnyOutInstr i) = show i <> " :: *"

-- | Data type keeping the whole type check result: instruction and
-- type representations of instruction's input and output.
data SomeInstr inp where
  (:/) :: HST inp -> SomeInstrOut inp -> SomeInstr inp
infix 8 :/

instance Show (ExtInstr inp) => Show (SomeInstr inp) where
  show (inp :/ out) = show inp <> " -> " <> show out

-- | Data type, holding strictly-typed Michelson value along with its
-- type singleton.
data SomeValue where
    (::::) :: (SingI t, Typeable t)
           => T.Value t
           -> (Sing t, Notes t)
           -> SomeValue

-- | Data type, holding strictly-typed Michelson value along with its
-- type singleton.
data SomeCValue where
    (:--:) :: (SingI t, Typeable t)
           => CValue t -> Sing t -> SomeCValue

data SomeContract where
  SomeContract
    :: (Each [Typeable, SingI, HasNoOp] [st, cp]
       , HasNoBigMap cp, BigMapConstraint st)
    => Contract cp st
    -> HST (ContractInp cp st)
    -> HST (ContractOut st)
    -> SomeContract

deriving instance Show SomeContract

-- | Set of variables defined in a let-block.
data BoundVars = BoundVars (Map Var Type) (Maybe SomeHST)

noBoundVars :: BoundVars
noBoundVars = BoundVars Map.empty Nothing

-- | State for type checking @nop@
type TcExtFrames = [BoundVars]
