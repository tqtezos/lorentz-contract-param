module Lorentz.ADT
  ( access_
  , get_
  , set_
  , modify_
  , construct
  , constructT
  , fieldCtor
  , wrap_
  , case_
  , caseT
  , (/->)

    -- * Useful re-exports
  , Rec (..)
  , (:!)
  , (:?)
  ) where

import Data.Constraint (Dict(..))
import qualified Data.Kind as Kind
import Data.Vinyl.Core (RMap(..), Rec(..))
import Data.Vinyl.Derived (Label)
import GHC.TypeLits (AppendSymbol)
import Named ((:!), (:?))

import Lorentz.Base
import Lorentz.Instr
import Michelson.Typed.Haskell.Instr
import Michelson.Typed.Haskell.Value
import Util.TypeTuple

-- | Extract a field of a datatype.
--
-- For this and the following functions you have to specify field name
-- which is either record name or name attached with @(:!)@ operator.
access_
  :: forall dt name fieldTy st path.
     InstrGetC dt name fieldTy path
  => Label name -> dt & st :-> fieldTy & st
access_ = I . instrGet @dt

-- | Extract a field of a datatype, leaving the original datatype on stack.
get_
  :: forall dt name fieldTy st path.
     InstrGetC dt name fieldTy path
  => Label name -> dt & st :-> fieldTy & dt ': st
get_ l = dup # access_ @dt l

-- | Set a field of a datatype.
set_
  :: forall dt name fieldTy st path.
     InstrSetC dt name fieldTy path
  => Label name -> (fieldTy ': dt ': st) :-> (dt ': st)
set_ = I . instrSet @dt

-- | Apply given modifier to a datatype field.
modify_
  :: forall dt name fieldTy st path.
     ( InstrGetC dt name fieldTy path
     , InstrSetC dt name fieldTy path
     )
  => Label name
  -> (forall st0. (fieldTy ': st0) :-> (fieldTy ': st0))
  -> dt & st :-> dt & st
modify_ l i = get_ @dt l # i # set_ @dt l

-- | Make up a datatype. You provide a pack of individual fields constructors.
--
-- Each element of the accepted record should be an instruction wrapped with
-- 'fieldCtor' function. This instruction will have access to the stack at
-- the moment of calling @construct@.
-- Instructions have to output fields of the built datatype, one per instruction;
-- instructions order is expected to correspond to the order of fields in the
-- datatype.
construct
  :: forall dt st.
     ( InstrConstructC dt
     , RMap (ConstructorFieldTypes dt)
     )
  => Rec (FieldConstructor st) (ConstructorFieldTypes dt)
  -> st :-> dt & st
construct fctors =
  I $ instrConstruct @dt $
  rmap (\(FieldConstructor i) -> FieldConstructor i) fctors

-- | Version of 'construct' which accepts tuple of field constructors.
constructT
  :: forall dt fctors st.
     ( InstrConstructC dt
     , RMap (ConstructorFieldTypes dt)
     , fctors ~ Rec (FieldConstructor st) (ConstructorFieldTypes dt)
     , RecFromTuple fctors
     )
  => IsoRecTuple fctors
  -> st :-> dt & st
constructT = construct . recFromTuple

-- | Lift an instruction to field constructor.
fieldCtor :: (st :-> f & st) -> FieldConstructor st f
fieldCtor (I i) = FieldConstructor i

-- | Wrap entry in constructor. Useful for sum types.
wrap_
  :: forall dt name st.
     InstrWrapC dt name
  => Label name -> (AppendCtorField (GetCtorField dt name) st) :-> dt & st
wrap_ =
  case appendCtorFieldAxiom @(GetCtorField dt name) @st of
    Dict -> I . instrWrap @dt

-- | Lorentz analogy of 'CaseClause', it works on plain 'Kind.Type' types.
data CaseClauseL (inp :: [Kind.Type]) (out :: [Kind.Type]) (param :: CaseClauseParam) where
  CaseClauseL :: AppendCtorField x inp :-> out -> CaseClauseL inp out ('CaseClauseParam ctor x)

-- | Lift an instruction to case clause.
--
-- You should write out constructor name corresponding to the clause
-- explicitly. Prefix constructor name with "c" letter, otherwise
-- your label will not be recognized by Haskell parser.
-- Passing constructor name can be circumvented but doing so is not recomended
-- as mentioning contructor name improves readability and allows avoiding
-- some mistakes.
(/->)
  :: Label ("c" `AppendSymbol` ctor)
  -> AppendCtorField x inp :-> out
  -> CaseClauseL inp out ('CaseClauseParam ctor x)
(/->) _ = CaseClauseL
infixr 0 /->

-- | Pattern match on the given sum type.
--
-- You have to provide a 'Rec' containing case branches.
-- To construct a case branch use '/->' operator.
case_
  :: forall dt out inp.
     ( InstrCaseC dt inp out
     , RMap (CaseClauses dt)
     )
  => Rec (CaseClauseL inp out) (CaseClauses dt) -> dt & inp :-> out
case_ = I . instrCase @dt . rmap coerceCaseClause
  where
    coerceCaseClause
      :: forall clauses.
         CaseClauseL inp out clauses -> CaseClause (ToTs inp) (ToTs out) clauses
    coerceCaseClause (CaseClauseL (I cc)) =
      CaseClause $ case Proxy @clauses of
        (_ :: Proxy ('CaseClauseParam ctor cc)) ->
          case appendCtorFieldAxiom @cc @inp of Dict -> cc

-- | Like 'case_', accepts a tuple of clauses, which may be more convenient.
caseT
  :: forall dt out inp clauses.
     ( InstrCaseC dt inp out
     , RMap (CaseClauses dt)
     , RecFromTuple clauses
     , clauses ~ Rec (CaseClauseL inp out) (CaseClauses dt)
     )
  => IsoRecTuple clauses -> dt & inp :-> out
caseT = case_ @dt . recFromTuple
