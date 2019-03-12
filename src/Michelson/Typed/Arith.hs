{-# LANGUAGE DataKinds, MultiParamTypeClasses, TypeFamilies #-}

-- | Module, containing some boilerplate for support of
-- arithmetic operations in Michelson language.

module Michelson.Typed.Arith
  ( ArithOp (..)
  , UnaryArithOp (..)
  , ArithError (..)
  , ArithErrorType (..)
  , Add
  , Sub
  , Mul
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

import Data.Bits (complement, shift, xor, (.&.), (.|.))
import Fmt (Buildable(build))

import Michelson.Typed.CValue (CVal(..))
import Michelson.Typed.T (CT(..))
import Tezos.Core (addMutez, mulMutez, subMutez, timestampFromSeconds, timestampToSeconds)

-- | Class for binary arithmetic operation.
--
-- Takes binary operation marker as @op@ parameter,
-- types of left operand @n@ and right operand @m@.
class ArithOp aop (n :: CT) (m :: CT) where

  -- | Type family @ArithRes@ denotes the type resulting from
  -- computing operation @op@ from operands of types @n@ and @m@.
  --
  -- For instance, adding integer to natural produces integer,
  -- which is reflected in following instance of type family:
  -- @ArithRes Add T_nat T_int = T_int@.
  type ArithRes aop n m :: CT

  -- | Evaluate arithmetic operation on given operands.
  evalOp :: proxy aop -> CVal n -> CVal m -> Either (ArithError (CVal n) (CVal m)) (CVal (ArithRes aop n m))

-- | Denotes the error type occured in the arithmetic operation.
data ArithErrorType
  = AddOverflow
  | MulOverflow
  | SubUnderflow
  deriving (Show, Eq, Ord)

-- | Represents an arithmetic error of the operation.
data ArithError n m
  = MutezArithError ArithErrorType n m
  deriving (Show, Eq, Ord)

-- | Marker data type for add operation.
class UnaryArithOp aop (n :: CT) where
  type UnaryArithRes aop n :: CT
  evalUnaryArithOp :: proxy aop -> CVal n -> CVal (UnaryArithRes aop n)

data Add
data Sub
data Mul
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
  type ArithRes Add 'T_nat 'T_int = 'T_int
  evalOp _ (CvNat i) (CvInt j) = Right $ CvInt (toInteger i + j)
instance ArithOp Add 'T_int 'T_nat where
  type ArithRes Add 'T_int 'T_nat = 'T_int
  evalOp _ (CvInt i) (CvNat j) = Right $ CvInt (i + toInteger j)
instance ArithOp Add 'T_nat 'T_nat where
  type ArithRes Add 'T_nat 'T_nat = 'T_nat
  evalOp _ (CvNat i) (CvNat j) = Right $ CvNat (i + j)
instance ArithOp Add 'T_int 'T_int where
  type ArithRes Add 'T_int 'T_int = 'T_int
  evalOp _ (CvInt i) (CvInt j) = Right $ CvInt (i + j)
instance ArithOp Add 'T_timestamp 'T_int where
  type ArithRes Add 'T_timestamp 'T_int = 'T_timestamp
  evalOp _ (CvTimestamp i) (CvInt j) =
    Right $ CvTimestamp $ timestampFromSeconds $ timestampToSeconds i + j
instance ArithOp Add 'T_int 'T_timestamp where
  type ArithRes Add 'T_int 'T_timestamp = 'T_timestamp
  evalOp _ (CvInt i) (CvTimestamp j) =
    Right $ CvTimestamp $ timestampFromSeconds $ timestampToSeconds j + i
instance ArithOp Add 'T_mutez 'T_mutez where
  type ArithRes Add 'T_mutez 'T_mutez = 'T_mutez
  evalOp _ n@(CvMutez i) m@(CvMutez j) = res
    where
      res = maybe (Left $ MutezArithError AddOverflow n m) (Right . CvMutez) $ i `addMutez` j

instance ArithOp Sub 'T_nat 'T_int where
  type ArithRes Sub 'T_nat 'T_int = 'T_int
  evalOp _ (CvNat i) (CvInt j) = Right $ CvInt (toInteger i - j)
instance ArithOp Sub 'T_int 'T_nat where
  type ArithRes Sub 'T_int 'T_nat = 'T_int
  evalOp _ (CvInt i) (CvNat j) = Right $ CvInt (i - toInteger j)
instance ArithOp Sub 'T_nat 'T_nat where
  type ArithRes Sub 'T_nat 'T_nat = 'T_int
  evalOp _ (CvNat i) (CvNat j) = Right $ CvInt (toInteger i - toInteger j)
instance ArithOp Sub 'T_int 'T_int where
  type ArithRes Sub 'T_int 'T_int = 'T_int
  evalOp _ (CvInt i) (CvInt j) = Right $ CvInt (i - j)
instance ArithOp Sub 'T_timestamp 'T_int where
  type ArithRes Sub 'T_timestamp 'T_int = 'T_timestamp
  evalOp _ (CvTimestamp i) (CvInt j) =
    Right $ CvTimestamp $ timestampFromSeconds $ timestampToSeconds i - j
instance ArithOp Sub 'T_timestamp 'T_timestamp where
  type ArithRes Sub 'T_timestamp 'T_timestamp = 'T_int
  evalOp _ (CvTimestamp i) (CvTimestamp j) =
    Right $ CvInt $ timestampToSeconds i - timestampToSeconds j
instance ArithOp Sub 'T_mutez 'T_mutez where
  type ArithRes Sub 'T_mutez 'T_mutez = 'T_mutez
  evalOp _ n@(CvMutez i) m@(CvMutez j) = res
    where
      res = maybe (Left $ MutezArithError SubUnderflow n m) (Right . CvMutez) $ i `subMutez` j

instance ArithOp Mul 'T_nat 'T_int where
  type ArithRes Mul 'T_nat 'T_int = 'T_int
  evalOp _ (CvNat i) (CvInt j) = Right $ CvInt (toInteger i * j)
instance ArithOp Mul 'T_int 'T_nat where
  type ArithRes Mul 'T_int 'T_nat = 'T_int
  evalOp _ (CvInt i) (CvNat j) = Right $ CvInt (i * toInteger j)
instance ArithOp Mul 'T_nat 'T_nat where
  type ArithRes Mul 'T_nat 'T_nat = 'T_nat
  evalOp _ (CvNat i) (CvNat j) = Right $ CvNat (i * j)
instance ArithOp Mul 'T_int 'T_int where
  type ArithRes Mul 'T_int 'T_int = 'T_int
  evalOp _ (CvInt i) (CvInt j) = Right $ CvInt (i * j)
instance ArithOp Mul 'T_nat 'T_mutez where
  type ArithRes Mul 'T_nat 'T_mutez = 'T_mutez
  evalOp _ n@(CvNat i) m@(CvMutez j) = res
    where
      res = maybe (Left $ MutezArithError MulOverflow n m) (Right . CvMutez) $ j `mulMutez` i
instance ArithOp Mul 'T_mutez 'T_nat where
  type ArithRes Mul 'T_mutez 'T_nat = 'T_mutez
  evalOp _ n@(CvMutez i) m@(CvNat j) = res
    where
      res = maybe (Left $ MutezArithError MulOverflow n m) (Right . CvMutez) $ i `mulMutez` j

instance UnaryArithOp Abs 'T_int where
  type UnaryArithRes Abs 'T_int = 'T_nat
  evalUnaryArithOp _ (CvInt i) = CvNat (fromInteger $ abs i)

instance UnaryArithOp Neg 'T_int where
  type UnaryArithRes Neg 'T_int = 'T_int
  evalUnaryArithOp _ (CvInt i) = CvInt (-i)

instance ArithOp Or 'T_nat 'T_nat where
  type ArithRes Or 'T_nat 'T_nat = 'T_nat
  evalOp _ (CvNat i) (CvNat j) = Right $ CvNat (i .|. j)
instance ArithOp Or 'T_bool 'T_bool where
  type ArithRes Or 'T_bool 'T_bool = 'T_bool
  evalOp _ (CvBool i) (CvBool j) = Right $ CvBool (i .|. j)

instance ArithOp And 'T_int 'T_nat where
  type ArithRes And 'T_int 'T_nat = 'T_int
  evalOp _ (CvInt i) (CvNat j) = Right $ CvInt (i .&. fromIntegral j)
instance ArithOp And 'T_nat 'T_nat where
  type ArithRes And 'T_nat 'T_nat = 'T_nat
  evalOp _ (CvNat i) (CvNat j) = Right $ CvNat (i .&. j)
instance ArithOp And 'T_bool 'T_bool where
  type ArithRes And 'T_bool 'T_bool = 'T_bool
  evalOp _ (CvBool i) (CvBool j) = Right $ CvBool (i .&. j)

instance ArithOp Xor 'T_nat 'T_nat where
  type ArithRes Xor 'T_nat 'T_nat = 'T_nat
  evalOp _ (CvNat i) (CvNat j) = Right $ CvNat (i `xor` j)
instance ArithOp Xor 'T_bool 'T_bool where
  type ArithRes Xor 'T_bool 'T_bool = 'T_bool
  evalOp _ (CvBool i) (CvBool j) = Right $ CvBool (i `xor` j)

-- Todo add condition when shift >= 256
instance ArithOp Lsl 'T_nat 'T_nat where
  type ArithRes Lsl 'T_nat 'T_nat = 'T_nat
  evalOp _ (CvNat i) (CvNat j) =
    Right $ CvNat (fromInteger $ shift (toInteger i) (fromIntegral j))

instance ArithOp Lsr 'T_nat 'T_nat where
  type ArithRes Lsr 'T_nat 'T_nat = 'T_nat
  evalOp _ (CvNat i) (CvNat j) =
    Right $ CvNat (fromInteger $ shift (toInteger i) (-(fromIntegral j)))

instance UnaryArithOp Not 'T_int where
  type UnaryArithRes Not 'T_int = 'T_int
  evalUnaryArithOp _ (CvInt i) = CvInt (complement i)
instance UnaryArithOp Not 'T_nat where
  type UnaryArithRes Not 'T_nat = 'T_int
  evalUnaryArithOp _ (CvNat i) = CvInt (complement $ toInteger i)
instance UnaryArithOp Not 'T_bool where
  type UnaryArithRes Not 'T_bool = 'T_bool
  evalUnaryArithOp _ (CvBool i) = CvBool (not i)

instance ArithOp Compare 'T_bool 'T_bool where
  type ArithRes Compare 'T_bool 'T_bool = 'T_int
  evalOp _ (CvBool i) (CvBool j) =
    Right $ CvInt $ toInteger $ fromEnum (compare i j) - 1
instance ArithOp Compare 'T_address 'T_address where
  type ArithRes Compare 'T_address 'T_address = 'T_int
  evalOp _ (CvAddress i) (CvAddress j) =
    Right $ CvInt $ toInteger $ fromEnum (compare i j) - 1
instance ArithOp Compare 'T_nat 'T_nat where
  type ArithRes Compare 'T_nat 'T_nat = 'T_int
  evalOp _ (CvNat i) (CvNat j) =
    Right $ CvInt $ toInteger $ fromEnum (compare i j) - 1
instance ArithOp Compare 'T_int 'T_int where
  type ArithRes Compare 'T_int 'T_int = 'T_int
  evalOp _ (CvInt i) (CvInt j) =
    Right $ CvInt $ toInteger $ fromEnum (compare i j) - 1
instance ArithOp Compare 'T_string 'T_string where
  type ArithRes Compare 'T_string 'T_string = 'T_int
  evalOp _ (CvString i) (CvString j) =
    Right $ CvInt $ toInteger $ fromEnum (compare i j) - 1
instance ArithOp Compare 'T_bytes 'T_bytes where
  type ArithRes Compare 'T_bytes 'T_bytes = 'T_int
  evalOp _ (CvBytes i) (CvBytes j) =
    Right $ CvInt $ toInteger $ fromEnum (compare i j) - 1
instance ArithOp Compare 'T_timestamp 'T_timestamp where
  type ArithRes Compare 'T_timestamp 'T_timestamp = 'T_int
  evalOp _ (CvTimestamp i) (CvTimestamp j) =
    Right $ CvInt $ toInteger $ fromEnum (compare i j) - 1
instance ArithOp Compare 'T_mutez 'T_mutez where
  type ArithRes Compare 'T_mutez 'T_mutez = 'T_int
  evalOp _ (CvMutez i) (CvMutez j) = Right $
    CvInt $ toInteger $ fromEnum (compare i j) - 1
instance ArithOp Compare 'T_key_hash 'T_key_hash where
  type ArithRes Compare 'T_key_hash 'T_key_hash = 'T_int
  evalOp _ (CvKeyHash i) (CvKeyHash j) =
    Right $ CvInt $ toInteger $ fromEnum (compare i j) - 1

instance UnaryArithOp Eq' 'T_int where
  type UnaryArithRes Eq' 'T_int = 'T_bool
  evalUnaryArithOp _ (CvInt i) = CvBool (i == 0)

instance UnaryArithOp Neq 'T_int where
  type UnaryArithRes Neq 'T_int = 'T_bool
  evalUnaryArithOp _ (CvInt i) = CvBool (i /= 0)


instance UnaryArithOp Lt 'T_int where
  type UnaryArithRes Lt 'T_int = 'T_bool
  evalUnaryArithOp _ (CvInt i) = CvBool (i < 0)

instance UnaryArithOp Gt 'T_int where
  type UnaryArithRes Gt 'T_int = 'T_bool
  evalUnaryArithOp _ (CvInt i) = CvBool (i > 0)

instance UnaryArithOp Le 'T_int where
  type UnaryArithRes Le 'T_int = 'T_bool
  evalUnaryArithOp _ (CvInt i) = CvBool (i <= 0)

instance UnaryArithOp Ge 'T_int where
  type UnaryArithRes Ge 'T_int = 'T_bool
  evalUnaryArithOp _ (CvInt i) = CvBool (i >= 0)


instance Buildable ArithErrorType where
  build AddOverflow = "add overflow"
  build MulOverflow = "mul overflow"
  build SubUnderflow = "sub overflow"

instance (Show n, Show m) => Buildable (ArithError n m) where
  build (MutezArithError errType n m) = "Mutez "
    <> build errType <> " with " <> show n <> ", " <> show m
