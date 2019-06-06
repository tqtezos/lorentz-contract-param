module Indigo.Examples
  ( varTest
  , ifTest

  , contractVarLorentz
  , contractIfLorentz
  , contractWhileLorentz
  ) where

import Lorentz hiding (if_, return, (>>))
import Prelude hiding (return, (>>), (>>=))

import Indigo.Expr (Expr(..))
import Indigo.Language (if_, setVar, newVar, while)
import Indigo.State (IndigoM, Var, compileIndigoContract, return, (>>), (>>=))

contractVarLorentz :: Contract Integer Integer
contractVarLorentz = compileIndigoContract varTest

contractIfLorentz :: Contract Integer Integer
contractIfLorentz = compileIndigoContract ifTest

varTest :: Var Integer -> Var Integer -> IndigoM [Integer, Integer] [Integer, Integer] ()
varTest _param _st = if_ ((5 :: Integer) `Lt` (10 :: Integer))
  (do
      _a <- newVar $ (10 :: Integer)
      return ()
  )
  ( return () )

ifTest :: Var Integer -> Var Integer -> IndigoM [Integer, Integer] [Bool, Integer, Integer, Integer] ()
ifTest param st = do
  a <- newVar ((5 :: Integer) `Add` (param `Add` (2 :: Integer)))
  if_ (param `Lt` a)
    (do
        _c <- newVar st
        return ()
    )
    (return ())
  _c <- newVar (param `Lt` st)
  return ()

contractWhileLorentz :: Contract Integer Integer
contractWhileLorentz = compileIndigoContract $ \param st -> do
  i <- newVar @Integer 0
  s <- newVar @Integer 0
  while (i `Lt` st) $ do
    if_ ((i `Mod` param) `Eq'` (0 :: Natural))
      (setVar s (s `Add` i))
      (return ())
    setVar i (i `Add` (1 :: Integer))
  setVar st s
