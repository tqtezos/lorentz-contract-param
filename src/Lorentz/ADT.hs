module Lorentz.ADT
  ( access_
  , get_
  , set_
  , modify_
  , wrap_
  , case_
  , caseT
  , (/->)

    -- * Useful re-exports
  , (:!)
  , (:?)
  ) where

import Data.Constraint (Dict(..))
import qualified Data.Kind as Kind
import Data.Vinyl.Core (RMap(..), Rec)
import Data.Vinyl.Derived (Label)
import GHC.TypeLits (AppendSymbol)
import Named ((:!), (:?))

import Lorentz.Base
import Lorentz.Instr
import Michelson.Typed.Haskell.Instr
import Michelson.Typed.Haskell.Value
import Michelson.Typed.Instr ()
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

-- | Wrap entry in constructor. Useful for sum types.
wrap_
  :: forall dt name entry st path.
     ( InstrWrapC dt name entry st path
     , AppendCtorFieldAxiom entry st
     )
  => Label name -> (AppendCtorField entry st) :-> dt & st
wrap_ = I . instrWrap @dt

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
infixr 8 /->

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
      :: forall ccp. ()
      => CaseClauseL inp out ccp -> CaseClause (ToTs inp) (ToTs out) ccp
    coerceCaseClause (CaseClauseL (I cc)) =
      CaseClause $ case Proxy @ccp of
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
