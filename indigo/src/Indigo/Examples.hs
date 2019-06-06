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
varTest _param _st = if_ (C (5 :: Integer) `Lt` C (10 :: Integer))
  (do
      _a <- newVar $ C (10 :: Integer)
      return ()
  )
  ( return () )

ifTest :: Var Integer -> Var Integer -> IndigoM [Integer, Integer] [Bool, Integer, Integer, Integer] ()
ifTest param st = do
  a <- newVar (C (5 :: Integer) `Add` (V param `Add` C (2 :: Integer)))
  if_ (V param `Lt` V a)
    (do
        _c <- newVar (V st)
        return ()
    )
    (return ())
  _c <- newVar (V param `Lt` V st)
  return ()

contractWhileLorentz :: Contract Integer Integer
contractWhileLorentz = compileIndigoContract $ \param st -> do
  i <- newVar @Integer (C 0)
  s <- newVar @Integer (C 0)
  while (V i `Lt` V st) $ do
    if_ ((V i `Mod` V param) `Eq'` C (0 :: Natural))
      (setVar s (V s `Add` V i))
      (return ())
    setVar i (V i `Add` C (1 :: Integer))
  setVar st (V s)
