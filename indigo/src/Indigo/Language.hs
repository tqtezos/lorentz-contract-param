-- | High level statements of Indigo language.

module Indigo.Language
  ( newVar
  , Indigo.Language.if_
  , setVar
  , while
  ) where

import Lorentz hiding (get, return, (>>))
import Prelude hiding (return, (>>), (>>=))

import Indigo.Expr (Expr, compileExpr)
import Indigo.State
  (DestroyPrefix(..), GenCode(..), IndigoM(..), MetaData(..), Var, VarActions(..), iget, lookupVar,
  makeTopVar, popNoRefMd, return, (>>), (>>=))
import qualified Lorentz.Instr as L

-- | Create a new variable with passed expression as an initial value.
newVar :: Typeable x => Expr x -> IndigoM inp (x & inp) (Var x)
newVar e = compileExpr e >> makeTopVar

-- | Set to variable new value.
setVar :: Typeable x => Var x -> Expr x -> IndigoM inp inp ()
setVar v e = do
  MetaData s _ <- iget
  let setter = vaSet $ lookupVar v s
  compileExpr e
  IndigoM (\md -> ((), GenCode (popNoRefMd md) setter))

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

-- | While statement. The same rule about releasing.
while :: forall inp xs . DestroyPrefix xs inp => Expr Bool -> IndigoM inp xs () -> IndigoM inp inp ()
while e body = IndigoM $ \md ->
  let ((), GenCode _ expCd) = runIndigoM (compileExpr e) md in
  let ((), GenCode _ bodyCode) = fmap (destroyPrefix @xs @inp) $ runIndigoM body md in
  ((), GenCode md $ expCd # L.loop (bodyCode # expCd))
