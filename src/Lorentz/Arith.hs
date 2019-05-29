-- | Type families from 'Michelson.Typed.Arith' lifted to Haskell types.
module Lorentz.Arith
  ( ArithOpHs (..)
  , UnaryArithOpHs (..)
  ) where

import qualified Data.Kind as Kind

import Lorentz.Value
import Michelson.Typed.Arith
import Michelson.Typed.Haskell.Value
import Michelson.Typed.T

-- | Lifted 'AithOp'.
class ( ArithOp aop (ToCT n) (ToCT m)
      , IsComparable n, IsComparable m
      , Typeable (ToCT n), Typeable (ToCT m)
      , ToT (ArithResHs aop n m) ~ 'Tc (ArithRes aop (ToCT n) (ToCT m))
      ) => ArithOpHs (aop :: Kind.Type) (n :: Kind.Type) (m :: Kind.Type) where
  type ArithResHs aop n m :: Kind.Type

-- | Lifted 'UnaryAithOp'.
class ( UnaryArithOp aop (ToCT n)
      , IsComparable n
      , Typeable (ToCT n)
      , ToT (UnaryArithResHs aop n) ~ 'Tc (UnaryArithRes aop (ToCT n))
      ) => UnaryArithOpHs (aop :: Kind.Type) (n :: Kind.Type) where
  type UnaryArithResHs aop n :: Kind.Type

instance ArithOpHs Add Natural Integer where
  type ArithResHs Add Natural Integer = Integer
instance ArithOpHs Add Integer Natural where
  type ArithResHs Add Integer Natural = Integer
instance ArithOpHs Add Natural Natural where
  type ArithResHs Add Natural Natural = Natural
instance ArithOpHs Add Integer Integer where
  type ArithResHs Add Integer Integer = Integer
instance ArithOpHs Add Timestamp Integer where
  type ArithResHs Add Timestamp Integer = Timestamp
instance ArithOpHs Add Integer Timestamp where
  type ArithResHs Add Integer Timestamp = Timestamp
instance ArithOpHs Add Mutez Mutez where
  type ArithResHs Add Mutez Mutez = Mutez

instance ArithOpHs Sub Natural Integer where
  type ArithResHs Sub Natural Integer = Integer
instance ArithOpHs Sub Integer Natural where
  type ArithResHs Sub Integer Natural = Integer
instance ArithOpHs Sub Natural Natural where
  type ArithResHs Sub Natural Natural = Integer
instance ArithOpHs Sub Integer Integer where
  type ArithResHs Sub Integer Integer = Integer
instance ArithOpHs Sub Timestamp Integer where
  type ArithResHs Sub Timestamp Integer = Timestamp
instance ArithOpHs Sub Timestamp Timestamp where
  type ArithResHs Sub Timestamp Timestamp = Integer
instance ArithOpHs Sub Mutez Mutez where
  type ArithResHs Sub Mutez Mutez = Mutez

instance ArithOpHs Mul Natural Integer where
  type ArithResHs Mul Natural Integer = Integer
instance ArithOpHs Mul Integer Natural where
  type ArithResHs Mul Integer Natural = Integer
instance ArithOpHs Mul Natural Natural where
  type ArithResHs Mul Natural Natural = Natural
instance ArithOpHs Mul Integer Integer where
  type ArithResHs Mul Integer Integer = Integer
instance ArithOpHs Mul Natural Mutez where
  type ArithResHs Mul Natural Mutez = Mutez
instance ArithOpHs Mul Mutez Natural where
  type ArithResHs Mul Mutez Natural = Mutez

instance UnaryArithOpHs Abs Integer where
  type UnaryArithResHs Abs Integer = Natural

instance UnaryArithOpHs Neg Integer where
  type UnaryArithResHs Neg Integer = Integer
instance UnaryArithOpHs Neg Natural where
  type UnaryArithResHs Neg Natural = Integer

instance ArithOpHs Or Natural Natural where
  type ArithResHs Or Natural Natural = Natural
instance ArithOpHs Or Bool Bool where
  type ArithResHs Or Bool Bool = Bool

instance ArithOpHs And Integer Natural where
  type ArithResHs And Integer Natural = Integer
instance ArithOpHs And Natural Natural where
  type ArithResHs And Natural Natural = Natural
instance ArithOpHs And Bool Bool where
  type ArithResHs And Bool Bool = Bool

instance ArithOpHs Xor Natural Natural where
  type ArithResHs Xor Natural Natural = Natural
instance ArithOpHs Xor Bool Bool where
  type ArithResHs Xor Bool Bool = Bool

instance ArithOpHs Lsl Natural Natural where
  type ArithResHs Lsl Natural Natural = Natural

instance ArithOpHs Lsr Natural Natural where
  type ArithResHs Lsr Natural Natural = Natural

instance UnaryArithOpHs Not Integer where
  type UnaryArithResHs Not Integer = Integer
instance UnaryArithOpHs Not Natural where
  type UnaryArithResHs Not Natural = Integer
instance UnaryArithOpHs Not Bool where
  type UnaryArithResHs Not Bool = Bool

instance ArithOpHs Compare Bool Bool where
  type ArithResHs Compare Bool Bool = Integer
instance ArithOpHs Compare Address Address where
  type ArithResHs Compare Address Address = Integer
instance ArithOpHs Compare Natural Natural where
  type ArithResHs Compare Natural Natural = Integer
instance ArithOpHs Compare Integer Integer where
  type ArithResHs Compare Integer Integer = Integer
instance ArithOpHs Compare Text Text where
  type ArithResHs Compare Text Text = Integer
instance ArithOpHs Compare ByteString ByteString where
  type ArithResHs Compare ByteString ByteString = Integer
instance ArithOpHs Compare Timestamp Timestamp where
  type ArithResHs Compare Timestamp Timestamp = Integer
instance ArithOpHs Compare Mutez Mutez where
  type ArithResHs Compare Mutez Mutez = Integer
instance ArithOpHs Compare KeyHash KeyHash where
  type ArithResHs Compare KeyHash KeyHash = Integer

instance UnaryArithOpHs Eq' Integer where
  type UnaryArithResHs Eq' Integer = Bool

instance UnaryArithOpHs Neq Integer where
  type UnaryArithResHs Neq Integer = Bool

instance UnaryArithOpHs Lt Integer where
  type UnaryArithResHs Lt Integer = Bool

instance UnaryArithOpHs Gt Integer where
  type UnaryArithResHs Gt Integer = Bool

instance UnaryArithOpHs Le Integer where
  type UnaryArithResHs Le Integer = Bool

instance UnaryArithOpHs Ge Integer where
  type UnaryArithResHs Ge Integer = Bool
