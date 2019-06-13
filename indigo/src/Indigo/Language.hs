-- | High level statements of Indigo language.

module Indigo.Language
  ( newVar
  , setVar
  , setField_
  , Indigo.Language.if_
  , ifJust
  , while
  , failUsing_
  , Indigo.Language.assert

  , fromLorentzFun2Args
  , fromLorentzFun1ArgVoid
  ) where

import Lorentz hiding (get, return, (>>))
import Prelude hiding (return, (>>), (>>=))

import Data.Vinyl.Derived (Label)

import Indigo.Expr (Expr(..), IsExpr, ValidValue, compileExpr, toExpr, IsExpr)
import Indigo.State
  (DestroyPrefix(..), GenCode(..), IndigoM(..), MetaData(..), Var, VarActions(..), iget, lookupVar,
  makeTopVar, popNoRefMd, pushNoRefMd, pushRefMd, return, (>>), (>>=))
import qualified Lorentz.Instr as L
import qualified Lorentz.Macro as L
import Michelson.Typed.Haskell.Instr.Product (GetFieldType, InstrSetFieldC)

-- | Create a new variable with passed expression as an initial value.
newVar :: (IsExpr ex x, Typeable x) => ex -> IndigoM inp (x & inp) (Var x)
newVar e = compileExpr (toExpr e) >> makeTopVar

-- | Set to variable new value.
setVar :: (IsExpr ex x, Typeable x) => Var x -> ex -> IndigoM inp inp ()
setVar v e = do
  MetaData s _ <- iget
  let setter = vaSet $ lookupVar v s
  compileExpr (toExpr e)
  IndigoM (\md -> ((), GenCode (popNoRefMd md) setter))

setField_ ::
  forall dt name inp ex . (InstrSetFieldC dt name, Typeable dt, IsExpr ex (GetFieldType dt name))
  => Var dt -> Label name -> ex -> IndigoM inp inp ()
setField_ v l e = do
  MetaData s _ <- iget
  let setFl = vaSetField $ lookupVar v s
  compileExpr (toExpr e)
  IndigoM (\md -> ((), GenCode (popNoRefMd md) (setFl l)))
-- Can be implemented in this way:
-- setField_ v l newVal = setVar v (SetField (V v) l newVal)
-- but it takes 2x more instructions.

-- | If statement. It's supposed that all created
-- variables inside branches will be released after execution
-- leaves a scope where they were created.
if_ :: forall inp xs ys .
  ( DestroyPrefix xs inp
  , DestroyPrefix ys inp
  )
  => Expr Bool
  -> IndigoM inp xs ()
  -> IndigoM inp ys ()
  -> IndigoM inp inp ()
if_ e t f = compileExpr e >> ifImpl
  where
    ifImpl :: IndigoM (Bool & inp) inp ()
    ifImpl = IndigoM $ \md ->
      let mdNoBool = popNoRefMd md in
      let ((), gc1) = fmap (destroyPrefix @xs @inp) $ runIndigoM t mdNoBool in
      let ((), gc2) = fmap (destroyPrefix @ys @inp) $ runIndigoM f mdNoBool in
      ((), GenCode mdNoBool (L.if_ (gcCode gc1) (gcCode gc2)))

-- | If which works like case for Maybe.
ifJust :: forall inp xs ys x .
  ( DestroyPrefix xs inp
  , DestroyPrefix ys inp
  , Typeable x
  )
  => Expr (Maybe x) -> (Var x -> IndigoM (x & inp) xs ()) -> IndigoM inp ys ()  -> IndigoM inp inp ()
ifJust e t f = compileExpr e >> ifImpl
  where
    ifImpl :: IndigoM (Maybe x & inp) inp ()
    ifImpl = IndigoM $ \md ->
      let md1 = popNoRefMd md in
      let (v, mdJust) = pushRefMd md1 in
      let ((), gc1) = fmap (destroyPrefix @xs @inp) $ runIndigoM (t v) mdJust in
      let ((), gc2) = fmap (destroyPrefix @ys @inp) $ runIndigoM f md1 in
      ((), GenCode md1 (L.ifSome (gcCode gc1) (gcCode gc2)))

-- | While statement. The same rule about releasing.
while :: forall inp xs . DestroyPrefix xs inp => Expr Bool -> IndigoM inp xs () -> IndigoM inp inp ()
while e body = IndigoM $ \md ->
  let ((), GenCode _ expCd) = runIndigoM (compileExpr e) md in
  let ((), GenCode _ bodyCode) = fmap (destroyPrefix @xs @inp) $ runIndigoM body md in
  ((), GenCode md $ expCd # L.loop (bodyCode # expCd))

failUsing_ :: ValidValue x => x -> IndigoM s s ()
failUsing_ x = IndigoM $ \md -> ((), GenCode md (L.failUsing x))

assert :: (ValidValue x, IsExpr ex Bool) => x -> ex -> IndigoM s s ()
assert err e = compileExpr (toExpr e) >> assertImpl
  where
    assertImpl :: IndigoM (Bool & s) s ()
    assertImpl = IndigoM $ \md ->
      let mdNoBool = popNoRefMd md in
      let ((), gc) = runIndigoM (failUsing_ err) mdNoBool in
      ((), GenCode mdNoBool (L.if_ nop (gcCode gc)))

-- | Convert Lorentz binary function to IndigoM.
-- Will be removed when all Lorentz code is translated in Indigo.
fromLorentzFun2Args
  :: (Typeable ret, IsExpr ex1 a, IsExpr ex2 b)
  => a & b & s :-> ret & s
  -> ex1 -> ex2 -> IndigoM s (ret & s) (Var ret)
fromLorentzFun2Args fun e1 e2 = do
  compileExpr (toExpr e2)
  compileExpr (toExpr e1)
  IndigoM (\md -> ((), GenCode (pushNoRefMd $ popNoRefMd $ popNoRefMd md) fun))
  makeTopVar

fromLorentzFun1ArgVoid
  :: IsExpr ex a
  => a & s :-> s
  -> ex -> IndigoM s s ()
fromLorentzFun1ArgVoid fun e = do
  compileExpr (toExpr e)
  IndigoM (\md -> ((), GenCode (popNoRefMd md) fun))
