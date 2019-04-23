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

import Data.Vinyl.Core (Rec)
import Data.Vinyl.Derived (Label)
import Named ((:!), (:?))

import Michelson.Typed.Haskell.Instr
import Michelson.Typed.Haskell.Value
import Michelson.Typed.Instr
import Util.TypeTuple

-- | Extract a field of a datatype.
--
-- For this and the following functions you have to specify field name
-- which is either record name or name attached with @(:!)@ operator.
access_
  :: forall dt name fieldTy st path.
     InstrGetC dt name fieldTy st path
  => Label name -> (ToT dt ': st) :+> (ToT fieldTy ': st)
access_ = instrGet @dt

-- | Extract a field of a datatype, leaving the original datatype on stack.
get_
  :: forall dt name fieldTy st path.
     InstrGetC dt name fieldTy st path
  => Label name -> (ToT dt ': st) :+> (ToT fieldTy ': ToT dt ': st)
get_ l = DUP # access_ @dt l

-- | Set a field of a datatype.
set_
  :: forall dt name fieldTy st path.
     InstrSetC dt name fieldTy st path
  => Label name -> (ToT fieldTy ': ToT dt ': st) :+> (ToT dt ': st)
set_ = instrSet @dt

-- | Apply given modifier to a datatype field.
modify_
  :: forall dt name fieldTy st path.
     ( InstrGetC dt name fieldTy st path
     , InstrSetC dt name fieldTy st path
     )
  => Label name
  -> (forall st0. (ToT fieldTy ': st0) :+> (ToT fieldTy ': st0))
  -> (ToT dt ': st) :+> (ToT dt ': st)
modify_ l i = get_ @dt l # i # set_ @dt l

-- | Wrap entry in constructor. Useful for sum types.
wrap_
  :: forall dt name entry st path.
     InstrWrapC dt name entry st path
  => Label name -> (AppendCtorField entry st) :+> (ToT dt ': st)
wrap_ = instrWrap @dt

-- | Pattern match on the given sum type.
--
-- You have to provide a 'Rec' containing case branches.
-- To construct a case branch use '/->' operator.
case_
  :: forall dt out inp.
     InstrCaseC dt inp out
  => Rec (CaseClause inp out) (CaseClauses dt) -> (ToT dt ': inp) :+> out
case_ = instrCase @dt

-- | Like 'case_', accepts a tuple of clauses, which may be more convenient.
caseT
  :: forall dt out inp clauses.
     ( InstrCaseC dt inp out
     , RecFromTuple clauses
     , clauses ~ Rec (CaseClause inp out) (CaseClauses dt)
     )
  => IsoRecTuple clauses -> (ToT dt ': inp) :+> out
caseT = case_ @dt . recFromTuple
