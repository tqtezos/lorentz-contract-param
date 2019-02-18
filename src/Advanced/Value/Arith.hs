{-# LANGUAGE DataKinds, MultiParamTypeClasses, TypeFamilies #-}

-- | Module, containing some boilerplate for support of
-- arithmetic operations in Michelson language.

module Advanced.Value.Arith
  ( ArithOp (..)
  , UnaryArithOp (..)
  , Add
  , Sub
  , Mul
  -- , Div
  , Abs
  , Neg
  , Or
  , And
  , Xor
  , Not
  , Lsl
  , Lsr
  , Compare
  , Eq'
  , Neq
  , Lt
  , Gt
  , Le
  , Ge
  ) where

import Advanced.Type (CT(..))
import Advanced.Value.CValue (CVal(..))

import Data.Bits (complement, shift, xor, (.&.), (.|.))
import Data.Time.Clock (addUTCTime, diffUTCTime)

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
class UnaryArithOp op (n :: CT) where
  type UnaryArithResT op n :: CT
  evalUnaryArithOp :: proxy op -> CVal n -> CVal (UnaryArithResT op n)

-- class EDivOp (n :: CT) (m :: CT) where
--   type EDivResT op n m :: M.T
--   evalDivOp :: proxy op -> CVal n -> CVal m -> M.T_option (M.T_pair (M.T_comparable n) (M.T_comparable m))

data Add
data Sub
data Mul
-- data Ediv
-- data Mod
data Abs
data Neg

data Or
data And
data Xor
data Not
data Lsl
data Lsr

data Compare
data Eq'
data Neq
data Lt
data Gt
data Le
data Ge

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
instance ArithOp Add 'T_timestamp 'T_int where
  type ArithResT Add 'T_timestamp 'T_int = 'T_timestamp
  evalOp _ (CvTimestamp i) (CvInt j) = CvTimestamp (addUTCTime (fromInteger $ j) i)
instance ArithOp Add 'T_int 'T_timestamp where
  type ArithResT Add 'T_int 'T_timestamp = 'T_timestamp
  evalOp _ (CvInt i) (CvTimestamp j) = CvTimestamp (addUTCTime (fromInteger $ i) j)
instance ArithOp Add 'T_mutez 'T_mutez where
  type ArithResT Add 'T_mutez 'T_mutez = 'T_mutez
  evalOp _ (CvMutez i) (CvMutez j) = CvMutez (i + j)

instance ArithOp Sub 'T_nat 'T_int where
  type ArithResT Sub 'T_nat 'T_int = 'T_int
  evalOp _ (CvNat i) (CvInt j) = CvInt (toInteger i - j)
instance ArithOp Sub 'T_int 'T_nat where
  type ArithResT Sub 'T_int 'T_nat = 'T_int
  evalOp _ (CvInt i) (CvNat j) = CvInt (i - toInteger j)
instance ArithOp Sub 'T_nat 'T_nat where
  type ArithResT Sub 'T_nat 'T_nat = 'T_int
  evalOp _ (CvNat i) (CvNat j) = CvInt (toInteger i - toInteger j)
instance ArithOp Sub 'T_int 'T_int where
  type ArithResT Sub 'T_int 'T_int = 'T_int
  evalOp _ (CvInt i) (CvInt j) = CvInt (i - j)
instance ArithOp Sub 'T_timestamp 'T_int where
  type ArithResT Sub 'T_timestamp 'T_int = 'T_timestamp
  evalOp _ (CvTimestamp i) (CvInt j) = CvTimestamp (addUTCTime (fromInteger $ -j) i)
instance ArithOp Sub 'T_timestamp 'T_timestamp where
  type ArithResT Sub 'T_timestamp 'T_timestamp = 'T_int
  evalOp _ (CvTimestamp i) (CvTimestamp j) = CvInt (floor $ toRational $ diffUTCTime i j * 1e-12)
-- Todo add condition when x - y < 0
instance ArithOp Sub 'T_mutez 'T_mutez where
  type ArithResT Sub 'T_mutez 'T_mutez = 'T_mutez
  evalOp _ (CvMutez i) (CvMutez j) = CvMutez (i - j)

instance ArithOp Mul 'T_nat 'T_int where
  type ArithResT Mul 'T_nat 'T_int = 'T_int
  evalOp _ (CvNat i) (CvInt j) = CvInt (toInteger i * j)
instance ArithOp Mul 'T_int 'T_nat where
  type ArithResT Mul 'T_int 'T_nat = 'T_int
  evalOp _ (CvInt i) (CvNat j) = CvInt (i * toInteger j)
instance ArithOp Mul 'T_nat 'T_nat where
  type ArithResT Mul 'T_nat 'T_nat = 'T_nat
  evalOp _ (CvNat i) (CvNat j) = CvNat (i * j)
instance ArithOp Mul 'T_int 'T_int where
  type ArithResT Mul 'T_int 'T_int = 'T_int
  evalOp _ (CvInt i) (CvInt j) = CvInt (i * j)
-- Todo check if there is overflow
instance ArithOp Mul 'T_nat 'T_mutez where
  type ArithResT Mul 'T_nat 'T_mutez = 'T_mutez
  evalOp _ (CvNat i) (CvMutez j) = CvMutez
    (fromInteger $ (toInteger i) * (toInteger j))
instance ArithOp Mul 'T_mutez 'T_nat where
  type ArithResT Mul 'T_mutez 'T_nat = 'T_mutez
  evalOp _ (CvMutez i) (CvNat j) = CvMutez
    (fromInteger $ (toInteger i) * (toInteger j))

instance UnaryArithOp Abs 'T_int where
  type UnaryArithResT Abs 'T_int = 'T_nat
  evalUnaryArithOp _ (CvInt i) = CvNat (fromInteger $ abs i)

instance UnaryArithOp Neg 'T_int where
  type UnaryArithResT Neg 'T_int = 'T_int
  evalUnaryArithOp _ (CvInt i) = CvInt (-i)

-- Todo
-- instance EDivOp Ediv 'T_nat 'T_int where
--   type EDivResT Ediv 'T_nat 'T_int = 'T_option('T_pair ('T_c 'T_int) ('T_c 'T_int))
--   evalDivOp _ (CvNat i) (CvInt j) = СvOption (toInteger i * j)
-- instance DivOp Ediv 'T_int 'T_nat where
--   type DivResT Ediv 'T_int 'T_nat = 'T_int
--   evalDivOp _ (CvInt i) (CvNat j) = CvInt (i * toInteger j)
-- instance DivOp Ediv 'T_nat 'T_nat where
--   type DivResT Ediv 'T_nat 'T_nat = 'T_nat
--   evalDivOp _ (CvNat i) (CvNat j) = CvNat (i * j)
-- instance EDivOp Ediv 'T_int 'T_int where
--   type EDivResT Ediv 'T_int 'T_int = 'T_option ('T_pair 'T_int) ('T_pair 'T_int)
--   evalDivOp _ (CvInt i) (CvInt j) = undefined

instance ArithOp Or 'T_nat 'T_nat where
  type ArithResT Or 'T_nat 'T_nat = 'T_nat
  evalOp _ (CvNat i) (CvNat j) = CvNat (i .|. j)
instance ArithOp Or 'T_bool 'T_bool where
  type ArithResT Or 'T_bool 'T_bool = 'T_bool
  evalOp _ (CvBool i) (CvBool j) = CvBool (i .|. j)

instance ArithOp And 'T_int 'T_nat where
  type ArithResT And 'T_int 'T_nat = 'T_int
  evalOp _ (CvInt i) (CvNat j) = CvInt (i .&. fromIntegral j)
instance ArithOp And 'T_nat 'T_nat where
  type ArithResT And 'T_nat 'T_nat = 'T_nat
  evalOp _ (CvNat i) (CvNat j) = CvNat (i .&. j)
instance ArithOp And 'T_bool 'T_bool where
  type ArithResT And 'T_bool 'T_bool = 'T_bool
  evalOp _ (CvBool i) (CvBool j) = CvBool (i .&. j)

instance ArithOp Xor 'T_nat 'T_nat where
  type ArithResT Xor 'T_nat 'T_nat = 'T_nat
  evalOp _ (CvNat i) (CvNat j) = CvNat (i `xor` j)
instance ArithOp Xor 'T_bool 'T_bool where
  type ArithResT Xor 'T_bool 'T_bool = 'T_bool
  evalOp _ (CvBool i) (CvBool j) = CvBool (i `xor` j)

-- Todo add condition when shift >= 256
instance ArithOp Lsl 'T_nat 'T_nat where
  type ArithResT Lsl 'T_nat 'T_nat = 'T_nat
  evalOp _ (CvNat i) (CvNat j) =
    CvNat (fromInteger $ shift (toInteger i) (fromIntegral j))

instance ArithOp Lsr 'T_nat 'T_nat where
  type ArithResT Lsr 'T_nat 'T_nat = 'T_nat
  evalOp _ (CvNat i) (CvNat j) =
    CvNat (fromInteger $ shift (toInteger i) (-(fromIntegral j)))

instance UnaryArithOp Not 'T_int where
  type UnaryArithResT Not 'T_int = 'T_int
  evalUnaryArithOp _ (CvInt i) = CvInt (complement i)
instance UnaryArithOp Not 'T_nat where
  type UnaryArithResT Not 'T_nat = 'T_int
  evalUnaryArithOp _ (CvNat i) = CvInt (complement $ toInteger i)
instance UnaryArithOp Not 'T_bool where
  type UnaryArithResT Not 'T_bool = 'T_bool
  evalUnaryArithOp _ (CvBool i) = CvBool (not i)

instance ArithOp Compare 'T_bool 'T_bool where
  type ArithResT Compare 'T_bool 'T_bool = 'T_int
  evalOp _ (CvBool i) (CvBool j) = CvInt
    (toInteger $ fromEnum (compare i j) - 1)
instance ArithOp Compare 'T_address 'T_address where
  type ArithResT Compare 'T_address 'T_address = 'T_int
  evalOp _ (CvAddress i) (CvAddress j) = CvInt
    (toInteger $ fromEnum (compare i j) - 1)
instance ArithOp Compare 'T_nat 'T_nat where
  type ArithResT Compare 'T_nat 'T_nat = 'T_int
  evalOp _ (CvNat i) (CvNat j) = CvInt
    (toInteger $ fromEnum (compare i j) - 1)
instance ArithOp Compare 'T_int 'T_int where
  type ArithResT Compare 'T_int 'T_int = 'T_int
  evalOp _ (CvInt i) (CvInt j) = CvInt
    (toInteger $ fromEnum (compare i j) - 1)
instance ArithOp Compare 'T_string 'T_string where
  type ArithResT Compare 'T_string 'T_string = 'T_int
  evalOp _ (CvString i) (CvString j) = CvInt
    (toInteger $ fromEnum (compare i j) - 1)
instance ArithOp Compare 'T_bytes 'T_bytes where
  type ArithResT Compare 'T_bytes 'T_bytes = 'T_int
  evalOp _ (CvBytes i) (CvBytes j) = CvInt
    (toInteger $ fromEnum (compare i j) - 1)
instance ArithOp Compare 'T_timestamp 'T_timestamp where
  type ArithResT Compare 'T_timestamp 'T_timestamp = 'T_int
  evalOp _ (CvTimestamp i) (CvTimestamp j) = CvInt
    (toInteger $ fromEnum (compare i j) - 1)
instance ArithOp Compare 'T_mutez 'T_mutez where
  type ArithResT Compare 'T_mutez 'T_mutez = 'T_int
  evalOp _ (CvMutez i) (CvMutez j) = CvInt
    (toInteger $ fromEnum (compare i j) - 1)
instance ArithOp Compare 'T_key_hash 'T_key_hash where
  type ArithResT Compare 'T_key_hash 'T_key_hash = 'T_int
  evalOp _ (CvKeyHash i) (CvKeyHash j) = CvInt
    (toInteger $ fromEnum (compare i j) - 1)

instance UnaryArithOp Eq' 'T_int where
  type UnaryArithResT Eq' 'T_int = 'T_bool
  evalUnaryArithOp _ (CvInt i) = CvBool (i == 0)

instance UnaryArithOp Neq 'T_int where
  type UnaryArithResT Neq 'T_int = 'T_bool
  evalUnaryArithOp _ (CvInt i) = CvBool (i /= 0)


instance UnaryArithOp Lt 'T_int where
  type UnaryArithResT Lt 'T_int = 'T_bool
  evalUnaryArithOp _ (CvInt i) = CvBool (i < 0)

instance UnaryArithOp Gt 'T_int where
  type UnaryArithResT Gt 'T_int = 'T_bool
  evalUnaryArithOp _ (CvInt i) = CvBool (i > 0)

instance UnaryArithOp Le 'T_int where
  type UnaryArithResT Le 'T_int = 'T_bool
  evalUnaryArithOp _ (CvInt i) = CvBool (i <= 0)

instance UnaryArithOp Ge 'T_int where
  type UnaryArithResT Ge 'T_int = 'T_bool
  evalUnaryArithOp _ (CvInt i) = CvBool (i >= 0)
