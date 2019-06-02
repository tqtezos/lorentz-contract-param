-- | Expressions supported in Indigo language and
-- it's compilation to Lorentz code.
module Indigo.Expr
  ( Expr (..)
  , compileExpr
  ) where

import Lorentz hiding (get, return, (>>))
import Prelude hiding (return, (>>), (>>=))

import qualified Lorentz.Instr as L
import qualified Lorentz.Macro as L
import qualified Michelson.Typed.Arith as M
import Indigo.State
  (GenCode(..), IndigoM (..), MetaData(..), Var, VarActions (..), iget, iput, lookupVar,
  popNoRefMd, pushNoRefMd, return, (>>), (>>=))

type ValidValue t = (KnownValue t, NoOperation t, NoBigMap t, IsoValue t)

data Expr a where
  C   :: ValidValue a => a -> Expr a
  V   :: Typeable a => Var a -> Expr a
  Add :: ArithOpHs M.Add n m => Expr n -> Expr m -> Expr (ArithResHs M.Add n m)
  Sub :: ArithOpHs M.Sub n m => Expr n -> Expr m -> Expr (ArithResHs M.Sub n m)
  Mul :: ArithOpHs M.Mul n m => Expr n -> Expr m -> Expr (ArithResHs M.Mul n m)
  Div :: EDivOpHs n m => Expr n -> Expr m -> Expr (EDivOpResHs n m)
  Mod :: EDivOpHs n m => Expr n -> Expr m -> Expr (EModOpResHs n m)

  Eq' :: (ArithOpHs M.Compare n m, UnaryArithOpHs M.Eq' (ArithResHs M.Compare n m))
      => Expr n -> Expr m -> Expr (UnaryArithResHs M.Eq' (ArithResHs M.Compare n m))

  Neq :: (ArithOpHs M.Compare n m, UnaryArithOpHs M.Neq (ArithResHs M.Compare n m))
      => Expr n -> Expr m -> Expr (UnaryArithResHs M.Neq (ArithResHs M.Compare n m))

  Le :: (ArithOpHs M.Compare n m, UnaryArithOpHs M.Le (ArithResHs M.Compare n m))
     => Expr n -> Expr m -> Expr (UnaryArithResHs M.Le (ArithResHs M.Compare n m))

  Lt :: (ArithOpHs M.Compare n m, UnaryArithOpHs M.Lt (ArithResHs M.Compare n m))
     => Expr n -> Expr m -> Expr (UnaryArithResHs M.Lt (ArithResHs M.Compare n m))

  Ge :: (ArithOpHs M.Compare n m, UnaryArithOpHs M.Ge (ArithResHs M.Compare n m))
     => Expr n -> Expr m -> Expr (UnaryArithResHs M.Ge (ArithResHs M.Compare n m))

  Gt :: (ArithOpHs M.Compare n m, UnaryArithOpHs M.Gt (ArithResHs M.Compare n m))
     => Expr n -> Expr m -> Expr (UnaryArithResHs M.Gt (ArithResHs M.Compare n m))

  Or :: ArithOpHs M.Or n m => Expr n -> Expr m -> Expr (ArithResHs M.Or n m)
  And :: ArithOpHs M.And n m => Expr n -> Expr m -> Expr (ArithResHs M.And n m)
  Not :: UnaryArithOpHs M.Not n => Expr n -> Expr (UnaryArithResHs M.Not n)

data ArithError = ZeroDivision
  deriving stock Generic
  deriving anyclass IsoValue

compileExpr :: forall a inp . Expr a -> IndigoM inp (a & inp) ()
compileExpr (C a) = do
  md <- iget
  iput $ GenCode (pushNoRefMd md) (push a)
compileExpr (V a) = do
  md@(MetaData s _) <- iget
  iput $ GenCode (pushNoRefMd md) (vaGet $ lookupVar a s)
compileExpr (Add e1 e2) = binaryOp e1 e2 add
compileExpr (Sub e1 e2)  = binaryOp e1 e2 sub
compileExpr (Mul e1 e2) = binaryOp e1 e2 mul
compileExpr (Div e1 e2) = binaryOp e1 e2 (ediv # L.ifSome L.car (failUsing ZeroDivision))
compileExpr (Mod e1 e2) = binaryOp e1 e2 (ediv # L.ifSome L.cdr (failUsing ZeroDivision))
compileExpr (Eq' e1 e2) = binaryOp e1 e2 eq
compileExpr (Neq e1 e2) = binaryOp e1 e2 neq
compileExpr (Lt e1 e2) = binaryOp e1 e2 lt
compileExpr (Le e1 e2) = binaryOp e1 e2 le
compileExpr (Gt e1 e2) = binaryOp e1 e2 gt
compileExpr (Ge e1 e2) = binaryOp e1 e2 ge
compileExpr (And e1 e2) = binaryOp e1 e2 L.and
compileExpr (Or e1 e2) = binaryOp e1 e2 L.or
compileExpr (Not e1) = do
  compileExpr e1
  IndigoM $ \md -> ((), GenCode (pushNoRefMd $ popNoRefMd md) L.not)

binaryOp :: forall res n m inp .
  Expr n -> Expr m -> n & m & inp :-> res & inp -> IndigoM inp (res & inp) ()
binaryOp e1 e2 opCode = do
  compileExpr e2
  compileExpr e1
  IndigoM $ \md -> ((), GenCode (pushNoRefMd $ popNoRefMd $ popNoRefMd md) opCode)
