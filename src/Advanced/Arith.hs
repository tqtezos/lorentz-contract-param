{-# LANGUAGE DataKinds, MultiParamTypeClasses, TypeFamilies #-}

-- | Module, containing some boilerplate for support of
-- arithmetic operations in Michelson language.

module Advanced.Arith
  ( ArithOp (..)
  , Add
  ) where

import Advanced.CValue (CVal(..))
import Advanced.Type (CT(..))

-- | Class for binary arithmetic operation.
--
-- Takes binary operation marker as @op@ parameter,
-- types of left operand @n@ and right operand @m@.
class ArithOp op (n :: CT) (m :: CT) where

  -- | Type family @ArithResT@ denotes the type resulting from
  -- computing operation @op@ from operands of types @n@ and @m@.
  --
  -- For instance, adding integer to natural produces integer,
  -- which is reflected in following instance of type fanily:
  -- @ArithResT Add T_nat T_int = T_int@.
  type ArithResT op n m :: CT

  -- | Evaluate arithmetic operation on given operands.
  evalOp :: proxy op -> CVal n -> CVal m -> CVal (ArithResT op n m)

-- | Marker data type for add operation.
data Add

instance ArithOp Add 'T_nat 'T_int where
  type ArithResT Add 'T_nat 'T_int = 'T_int
  evalOp _ (CvNat i) (CvInt j) = CvInt (toInteger i + j)
instance ArithOp Add 'T_int 'T_nat where
  type ArithResT Add 'T_int 'T_nat = 'T_int
  evalOp _ (CvInt i) (CvNat j) = CvInt (i + toInteger j)
instance ArithOp Add 'T_nat 'T_nat where
  type ArithResT Add 'T_nat 'T_nat = 'T_nat
  evalOp _ (CvNat i) (CvNat j) = CvNat (i + j)
instance ArithOp Add 'T_int 'T_int where
  type ArithResT Add 'T_int 'T_int = 'T_int
  evalOp _ (CvInt i) (CvInt j) = CvInt (i + j)

