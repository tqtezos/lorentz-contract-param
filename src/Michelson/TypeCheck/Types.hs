module Michelson.TypeCheck.Types
    ( HST (..)
    , SomeHST (..)
    , SomeInstr (..)
    , SomeVal (..)
    , SomeContract (..)
    , SomeValC (..)
    , TCError (..)
    , ExtC
    , TcInstrHandler
    , TcExtHandler
    , TcExtFrames
    , TcResult
    , TypeCheckEnv (..)
    , TypeCheckT
    , runTypeCheckT
    ) where

import Data.Singletons (SingI)
import Fmt (Buildable(..), pretty, (+|), (|+), (||+))
import Prelude hiding (EQ, GT, LT)
import qualified Text.Show

import Michelson.Typed (ConversibleExt, Notes(..), Sing(..), T(..), fromSingT)
import Michelson.Typed.Extract (toUType)
import Michelson.Typed.Instr
import Michelson.Typed.Value

import qualified Michelson.Untyped as U
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

-- | No-argument type wrapper for @HST@ data type.
data SomeHST where
  SomeHST :: Typeable ts => HST ts -> SomeHST

deriving instance Show SomeHST

-- | Data type holding both instruction and
-- type representations of instruction's input and output.
--
-- Intput and output stack types are wrapped inside the type and @Typeable@
-- constraints are provided to allow convenient unwrapping.
data SomeInstr where
  (:::) :: (Typeable inp, Typeable out)
        => Instr inp out
        -> (HST inp, HST out)
        -> SomeInstr
  SiFail :: SomeInstr

  -- TODO use this constructor (to have closer reflection of expression)
  -- SiFail :: Typeable inp => Instr cp inp out -> HST inp -> SomeInstr cp

instance Show InstrExtT => Show SomeInstr where
  show (i ::: (inp, out)) = show i <> " :: " <> show inp <> " -> " <> show out
  show SiFail = "failed"

-- | Data type, holding strictly-typed Michelson value along with its
-- type singleton.
data SomeVal where
    (::::) :: (SingI t, Typeable t)
           => Val Instr t
           -> (Sing t, Notes t)
           -> SomeVal

-- | Data type, holding strictly-typed Michelson value along with its
-- type singleton.
data SomeValC where
    (:--:) :: (SingI t, Typeable t)
           => CVal t -> Sing t -> SomeValC

data SomeContract where
  SomeContract
    :: (Typeable st, SingI st, SingI cp, Typeable cp)
    => Contract cp st
    -> HST (ContractInp cp st)
    -> HST (ContractOut st)
    -> SomeContract

deriving instance Show InstrExtT => Show SomeContract

-- | Type check error
data TCError =
    TCFailedOnInstr U.ExpandedInstr SomeHST Text
  | TCFailedOnValue U.UntypedValue T Text
  | TCOtherError Text

instance Buildable TCError where
  build = \case
    TCFailedOnInstr instr (SomeHST t) custom ->
      "Error checking expression " +| instr
          |+ " against input stack type " +| t
          ||+ bool (": " +| custom |+ "") "" (null custom)
    TCFailedOnValue v t custom ->
      "Error checking value " +| v
          |+ " against type " +| toUType t
          |+ bool (": " +| custom |+ "") "" (null custom)
    TCOtherError e ->
      "Error occurred during type check: " +| e |+ ""

instance Buildable U.ExpandedInstr => Show TCError where
  show = pretty

instance Buildable U.ExpandedInstr => Exception TCError

-- | State for type checking @nop@
type TcExtFrames = [(U.ExpandedInstrExtU, SomeHST)]

-- | Constraints on InstrExtT and untyped Instr
-- which are required for type checking
type ExtC
   = ( Show InstrExtT
     , Eq U.ExpandedInstrExtU
     , Typeable InstrExtT
     , Buildable U.ExpandedInstr
     , ConversibleExt
     )

type TypeCheckT a =
  ExceptT TCError
    (State TypeCheckEnv) a

-- | Function for typeChecking a @nop@ and updating state
-- TypeCheckT is used because inside
-- inside of TEST_ASSERT could be PRINT/STACKTYPE/etc extended instructions.
type TcExtHandler
  = U.ExpandedInstrExtU -> TcExtFrames -> SomeHST -> TypeCheckT (TcExtFrames, Maybe InstrExtT)

-- | The typechecking state
data TypeCheckEnv = TypeCheckEnv
  { tcExtHandler    :: TcExtHandler
  , tcExtFrames     :: TcExtFrames
  , tcContractParam :: U.Type
  }

runTypeCheckT :: TcExtHandler -> U.Type -> TypeCheckT a -> Either TCError a
runTypeCheckT nh param act = evaluatingState (TypeCheckEnv nh [] param) $ runExceptT act

type TcResult = Either TCError SomeInstr

type TcInstrHandler
   = U.ExpandedInstr
    -> SomeHST
      -> TypeCheckT SomeInstr
