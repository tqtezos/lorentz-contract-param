-- | Expressions supported in Indigo language and
-- it's compilation to Lorentz code.
module Indigo.Expr
  ( Expr (..)
  , ValidValue
  , IsExpr
  , toExpr
  , compileExpr
  , (.!)
  ) where

import Lorentz hiding (get, return, (>>))
import Prelude hiding (return, (>>), (>>=))

import GHC.TypeLits (KnownSymbol)
import qualified Data.Kind as Kind
import Data.Vinyl.Derived (Label)
import Data.Vinyl.Core (RMap(..), Rec(..))

import Indigo.State
  (GenCode(..), IndigoM(..), MetaData(..), Var, VarActions(..), iget, iput, lookupVar, popNoRefMd,
  pushNoRefMd, return, (>>), (>>=))
import Lorentz.ADT (InstrConstructC, fieldCtor)
import qualified Lorentz.Instr as L
import qualified Lorentz.Macro as L
import Lorentz.Store (storeGet)
import qualified Michelson.Typed.Arith as M
import Michelson.Typed.Haskell.Instr.Product (GetFieldType, InstrGetFieldC, InstrSetFieldC)

type ValidValue t = (KnownValue t, NoOperation t, NoBigMap t, IsoValue t)
type IsExpr op n = (ToExpr op, ExprType op ~ n)
type AreExprs ex1 ex2 n m = (IsExpr ex1 n, IsExpr ex2 m)

data Expr a where
  C   :: ValidValue a => a -> Expr a
  V   :: Typeable a => Var a -> Expr a
  Add :: (AreExprs ex1 ex2 n m, ArithOpHs M.Add n m) => ex1 -> ex2 -> Expr (ArithResHs M.Add n m)
  Sub :: (AreExprs ex1 ex2 n m, ArithOpHs M.Sub n m) => ex1 -> ex2 -> Expr (ArithResHs M.Sub n m)
  Mul :: (AreExprs ex1 ex2 n m, ArithOpHs M.Mul n m) => ex1 -> ex2 -> Expr (ArithResHs M.Mul n m)
  Div :: (AreExprs ex1 ex2 n m, EDivOpHs n m) => ex1 -> ex2 -> Expr (EDivOpResHs n m)
  Mod :: (AreExprs ex1 ex2 n m, EDivOpHs n m) => ex1 -> ex2 -> Expr (EModOpResHs n m)

  Eq' :: ( AreExprs ex1 ex2 n m
         , ArithOpHs M.Compare n m, UnaryArithOpHs M.Eq' (ArithResHs M.Compare n m))
      => ex1 -> ex2 -> Expr (UnaryArithResHs M.Eq' (ArithResHs M.Compare n m))

  Neq :: ( AreExprs ex1 ex2 n m
         , ArithOpHs M.Compare n m, UnaryArithOpHs M.Neq (ArithResHs M.Compare n m))
      => ex1 -> ex2 -> Expr (UnaryArithResHs M.Neq (ArithResHs M.Compare n m))

  Le :: ( AreExprs ex1 ex2 n m
        , ArithOpHs M.Compare n m, UnaryArithOpHs M.Le (ArithResHs M.Compare n m))
     => ex1 -> ex2 -> Expr (UnaryArithResHs M.Le (ArithResHs M.Compare n m))

  Lt :: ( AreExprs ex1 ex2 n m
        , ArithOpHs M.Compare n m, UnaryArithOpHs M.Lt (ArithResHs M.Compare n m))
     => ex1 -> ex2 -> Expr (UnaryArithResHs M.Lt (ArithResHs M.Compare n m))

  Ge :: ( AreExprs ex1 ex2 n m
        , ArithOpHs M.Compare n m, UnaryArithOpHs M.Ge (ArithResHs M.Compare n m))
     => ex1 -> ex2 -> Expr (UnaryArithResHs M.Ge (ArithResHs M.Compare n m))

  Gt :: ( AreExprs ex1 ex2 n m
        , ArithOpHs M.Compare n m, UnaryArithOpHs M.Gt (ArithResHs M.Compare n m))
     => ex1 -> ex2 -> Expr (UnaryArithResHs M.Gt (ArithResHs M.Compare n m))

  Or :: (AreExprs ex1 ex2 n m, ArithOpHs M.Or n m) => ex1 -> ex2 -> Expr (ArithResHs M.Or n m)
  And :: (AreExprs ex1 ex2 n m, ArithOpHs M.And n m) => ex1 -> ex2 -> Expr (ArithResHs M.And n m)
  Not :: (IsExpr op n, UnaryArithOpHs M.Not n) => op -> Expr (UnaryArithResHs M.Not n)

  Fst :: IsExpr op (n, m) => op -> Expr n
  Snd :: IsExpr op (n, m) => op -> Expr m

  Lookup :: (StoreGetC store name, IsExpr exKey (GetStoreKey store name), IsExpr exStore (Store store))
         => Label name -> exKey -> exStore -> Expr (Maybe $ GetStoreValue store name)
  InsertNew
    :: ( StoreInsertC store name, KnownSymbol name, KnownValue err
       , NoOperation err, NoBigMap err, IsoValue err
       , IsExpr exKey (GetStoreKey store name)
       , IsExpr exVal (GetStoreValue store name)
       , IsExpr exStore (Store store)
       )
    => Label name -> err
    -> exKey -> exVal -> exStore -> Expr (Store store)
  Insert
    :: ( StoreInsertC store name, KnownSymbol name
       , IsExpr exKey (GetStoreKey store name)
       , IsExpr exVal (GetStoreValue store name)
       , IsExpr exStore (Store store)
       )
    => Label name
    -> exKey -> exVal -> exStore -> Expr (Store store)

  ToField :: (InstrGetFieldC dt name, IsExpr exDt dt)
       => exDt -> Label name -> Expr (GetFieldType dt name)
  SetField :: (InstrSetFieldC dt name, IsExpr exDt dt, IsExpr exFld (GetFieldType dt name))
       => exDt -> Label name -> exFld -> Expr dt
  Construct ::
    ( InstrConstructC dt
    , RMap (ConstructorFieldTypes dt)
    )
    => Rec Expr (ConstructorFieldTypes dt) -> Expr dt

  Sender :: Expr Address

infixl 8 .!
(.!) :: (InstrGetFieldC dt name, IsExpr exDt dt) => exDt -> Label name -> Expr (GetFieldType dt name)
(.!) = ToField

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
compileExpr (Not e) = unaryOp e L.not

compileExpr (Fst e) = unaryOp e L.car
compileExpr (Snd e) = unaryOp e L.cdr

compileExpr (InsertNew l err k v store) = do
  compileExpr (toExpr store)
  compileExpr (toExpr v)
  compileExpr (toExpr k)
  IndigoM $ \md -> ((), GenCode (popNoRefMd $ popNoRefMd md) (storeInsertNew l (L.drop # L.push err)))
compileExpr (Insert l k v store) = do
  compileExpr (toExpr store)
  compileExpr (toExpr v)
  compileExpr (toExpr k)
  IndigoM $ \md -> ((), GenCode (popNoRefMd $ popNoRefMd md) (storeInsert l))
compileExpr (Lookup l ekey estore) = binaryOp ekey estore (storeGet l)

compileExpr (ToField e l) = do
  compileExpr (toExpr e)
  IndigoM (\md -> ((), GenCode (pushNoRefMd $ popNoRefMd md) (toField l)))
compileExpr (SetField ev l ef) = do
  compileExpr (toExpr ev)
  compileExpr (toExpr ef)
  IndigoM (\md -> ((), GenCode (popNoRefMd md) (setField l)))
compileExpr (Construct fields) = IndigoM $ \md ->
  let cd = construct $ rmap (\e -> fieldCtor $ gcCode $ snd $ runIndigoM (compileExpr e) md) fields in
  ((), GenCode (pushNoRefMd md) cd)
compileExpr Sender = IndigoM $ \md -> ((), GenCode (pushNoRefMd md) sender)

binaryOp :: forall n m ex1 ex2 res inp . (AreExprs ex1 ex2 n m)
  => ex1 -> ex2 -> n & m & inp :-> res & inp -> IndigoM inp (res & inp) ()
binaryOp (toExpr -> e1) (toExpr -> e2) opCode = do
  compileExpr e2
  compileExpr e1
  IndigoM $ \md -> ((), GenCode (pushNoRefMd $ popNoRefMd $ popNoRefMd md) opCode)

unaryOp :: forall n ex res inp . (IsExpr ex n)
  => ex -> n & inp :-> res & inp -> IndigoM inp (res & inp) ()
unaryOp (toExpr -> e) opCode = do
  compileExpr e
  IndigoM $ \md -> ((), GenCode (pushNoRefMd $ popNoRefMd md) opCode)

--------- Abstract Expr -------

type ExprType a = ExprType' (Decide a) a

toExpr :: forall a . ToExpr a => a -> Expr (ExprType a)
toExpr = toExpr' @(Decide a) @a

class ToExpr' (Decide x) x => ToExpr x
instance ToExpr' (Decide x) x => ToExpr x

-- This type class is needed to cope with overlapping instances.
class ToExpr' decision c where
  type family ExprType' decision c :: Kind.Type
  toExpr' :: c -> Expr (ExprType' decision c)

instance Typeable (a :: Kind.Type) => ToExpr' 'VarD (Var a) where
  type instance ExprType' 'VarD (Var a) = a
  toExpr' = V

instance ValidValue a => ToExpr' 'ValD a where
  type instance ExprType' 'ValD a = a
  toExpr' = C

instance ToExpr' 'ExprD (Expr a) where
  type instance ExprType' 'ExprD (Expr a) = a
  toExpr' = id

data Decision = VarD | ValD | ExprD

type family Decide x :: Decision where
  Decide (Var _) = 'VarD
  Decide (Expr _) = 'ExprD
  Decide _ = 'ValD
