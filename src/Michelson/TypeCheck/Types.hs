module Michelson.TypeCheck.Types
    (
      IT (..)
    , SomeIT (..)
    , SomeInstr (..)
    , SomeVal (..)
    , SomeContract (..)
    , SomeValC (..)
    , TCError (..)
    , TcInstrHandler
    , TcNopHandler
    , TypeCheckEnv (..)
    , TypeCheckT
    , runTypeCheckT
    ) where

import Data.Singletons (SingI)
import Fmt (Buildable(..), pretty, (+|), (|+), (||+))
import Prelude hiding (EQ, GT, LT)
import qualified Text.Show

import Michelson.Typed (Notes(..), Sing(..), T(..), fromSingT)
import Michelson.Typed.Extract (toMType)
import Michelson.Typed.Instr
import Michelson.Typed.Value

import qualified Michelson.Untyped as Untyped
import Michelson.Untyped.Annotation (VarAnn)

-- | Data type holding type information for stack.
--
-- This data type is used along with instruction data type @Instr@
-- to carry information about its input and output stack types.
--
-- That is, if there is value @instr :: Instr cp inp out@, along with this
-- @instr@ one may carry @inpIT :: IT inp@ and @outIT :: IT out@ which will
-- contain whole information about input and output stack types for @instr@.
--
-- Data type @IT@ is very similar to @Data.Vinyl.Rec@,
-- but is specialized for a particular purpose.
-- In particular, definition of @IT (t1 ': t2 ': ... tn ': '[])@ requires
-- constraints @(Typeable t1, Typeable t2, ..., Typeable tn)@ as well as
-- constraints @(Typeable '[ t1 ], Typeable '[ t1, t2 ], ...)@.
-- These applications of @Typeable@ class are required for convenient usage
-- of type encoded by @IT ts@ with some functions from @Data.Typeable@.
--
-- Data type @IT@ is a heterogenuous list of triples.
-- First element of triple is a type singleton which is due to main motivation
-- behind @IT@, namely for it to be used as representation of @Instr@ type
-- data for pattern-matching.
-- Second element of triple is a structure, holding field and type annotations
-- for a given type.
-- Third element of triple is an optional variable annotation for the stack
-- element.
data IT (ts :: [T])  where
  INil :: IT '[]
  (::&) :: (Typeable xs, Typeable x)
        => (Sing x, Notes x, VarAnn) -> IT xs -> IT (x ': xs)

instance Show (IT ts) where
  show INil = "[]"
  show (r ::& rs) = "[ " <> showDo (r ::& rs) <> " ]"
    where
      showDo :: IT (t ': ts_) -> String
      showDo ((a, _notes, _vn) ::& (b ::& c)) =
          show (fromSingT a) <> ", " <> showDo (b ::& c)
      showDo ((a, _notes, _vn) ::& INil) = show (fromSingT a)

infixr 7 ::&

-- | No-argument type wrapper for @IT@ data type.
data SomeIT where
  SomeIT :: Typeable ts => IT ts -> SomeIT

-- | Data type holding both instruction and
-- type representations of instruction's input and output.
--
-- Intput and output stack types are wrapped inside the type and @Typeable@
-- constraints are provided to allow convenient unwrapping.
data SomeInstr cp where
  (:::) :: (Typeable inp, Typeable out)
        => Instr cp inp out -> (IT inp, IT out) -> SomeInstr cp
  SiFail :: SomeInstr cp

  -- TODO use this constructor (to have closer reflection of expression)
  -- SiFail :: Typeable inp => Instr cp inp out -> IT inp -> SomeInstr cp

instance Show (SomeInstr cp) where
  show (i ::: (inp, out)) = show i <> " :: " <> show inp <> " -> " <> show out
  show SiFail = "failed"

-- | Data type, holding strictly-typed Michelson value along with its
-- type singleton.
data SomeVal cp where
    (::::) :: (SingI t, Typeable t)
           => Val (Instr cp) t -> (Sing t, Notes t) -> SomeVal cp

-- | Data type, holding strictly-typed Michelson value along with its
-- type singleton.
data SomeValC where
    (:--:) :: (SingI t, Typeable t) => CVal t -> Sing t -> SomeValC

data SomeContract where
  SomeContract
    :: (Typeable st, Typeable cp, SingI cp, SingI st)
    => Contract cp st
    -> IT (ContractInp cp st)
    -> IT (ContractOut st)
    -> SomeContract

deriving instance Show SomeContract

-- | Type check error
data TCError nop =
    TCFailedOnInstr (Untyped.Instr nop) SomeIT Text
  | TCFailedOnValue (Untyped.Value (Untyped.Op nop)) T Text
  | TCOtherError Text

instance Buildable nop => Buildable (TCError nop) where
  build = \case
    TCFailedOnInstr instr (SomeIT t) custom ->
      "Error checking expression " +| instr
          |+ " against input stack type " +| t
          ||+ bool (": " +| custom |+ "") "" (null custom)
    TCFailedOnValue v t custom ->
      "Error checking value " +| v
          |+ " against type " +| toMType t
          |+ bool (": " +| custom |+ "") "" (null custom)
    TCOtherError e ->
      "Error occurred during type check: " +| e |+ ""

instance Buildable nop => Show (TCError nop) where
  show = pretty

instance (Buildable nop, Typeable nop) => Exception (TCError nop)

type TcNopHandler nop = nop -> SomeIT -> Either (TCError nop) ()

data TypeCheckEnv nop = TypeCheckEnv {
  tcNopHandler :: TcNopHandler nop
-- TODO should be probably filled with other fields
}

type TypeCheckT cp nop a =
  ExceptT (TCError nop)
    (Reader (TypeCheckEnv nop)) a

runTypeCheckT :: TcNopHandler nop -> TypeCheckT cp nop a -> Either (TCError nop) a
runTypeCheckT nh act = usingReader (TypeCheckEnv nh) $ runExceptT act

type TcInstrHandler cp nop
   = Untyped.Instr nop
    -> SomeIT
      -> TypeCheckT cp nop (SomeInstr cp)
