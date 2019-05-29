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

import Michelson.Typed.CValue (CValue(..))
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
  -- @ArithRes Add CNat CInt = CInt@.
  type ArithRes aop n m :: CT

  -- | Evaluate arithmetic operation on given operands.
  evalOp :: proxy aop -> CValue n -> CValue m -> Either (ArithError (CValue n) (CValue m)) (CValue (ArithRes aop n m))

-- | Denotes the error type occured in the arithmetic operation.
data ArithErrorType
  = AddOverflow
  | MulOverflow
  | SubUnderflow
  | LslOverflow
  | LsrUnderflow
  deriving (Show, Eq, Ord)

-- | Represents an arithmetic error of the operation.
data ArithError n m
  = MutezArithError ArithErrorType n m
  | ShiftArithError ArithErrorType n m
  deriving (Show, Eq, Ord)

-- | Marker data type for add operation.
class UnaryArithOp aop (n :: CT) where
  type UnaryArithRes aop n :: CT
  evalUnaryArithOp :: proxy aop -> CValue n -> CValue (UnaryArithRes aop n)

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

instance ArithOp Add 'CNat 'CInt where
  type ArithRes Add 'CNat 'CInt = 'CInt
  evalOp _ (CvNat i) (CvInt j) = Right $ CvInt (toInteger i + j)
instance ArithOp Add 'CInt 'CNat where
  type ArithRes Add 'CInt 'CNat = 'CInt
  evalOp _ (CvInt i) (CvNat j) = Right $ CvInt (i + toInteger j)
instance ArithOp Add 'CNat 'CNat where
  type ArithRes Add 'CNat 'CNat = 'CNat
  evalOp _ (CvNat i) (CvNat j) = Right $ CvNat (i + j)
instance ArithOp Add 'CInt 'CInt where
  type ArithRes Add 'CInt 'CInt = 'CInt
  evalOp _ (CvInt i) (CvInt j) = Right $ CvInt (i + j)
instance ArithOp Add 'CTimestamp 'CInt where
  type ArithRes Add 'CTimestamp 'CInt = 'CTimestamp
  evalOp _ (CvTimestamp i) (CvInt j) =
    Right $ CvTimestamp $ timestampFromSeconds $ timestampToSeconds i + j
instance ArithOp Add 'CInt 'CTimestamp where
  type ArithRes Add 'CInt 'CTimestamp = 'CTimestamp
  evalOp _ (CvInt i) (CvTimestamp j) =
    Right $ CvTimestamp $ timestampFromSeconds $ timestampToSeconds j + i
instance ArithOp Add 'CMutez 'CMutez where
  type ArithRes Add 'CMutez 'CMutez = 'CMutez
  evalOp _ n@(CvMutez i) m@(CvMutez j) = res
    where
      res = maybe (Left $ MutezArithError AddOverflow n m) (Right . CvMutez) $ i `addMutez` j

instance ArithOp Sub 'CNat 'CInt where
  type ArithRes Sub 'CNat 'CInt = 'CInt
  evalOp _ (CvNat i) (CvInt j) = Right $ CvInt (toInteger i - j)
instance ArithOp Sub 'CInt 'CNat where
  type ArithRes Sub 'CInt 'CNat = 'CInt
  evalOp _ (CvInt i) (CvNat j) = Right $ CvInt (i - toInteger j)
instance ArithOp Sub 'CNat 'CNat where
  type ArithRes Sub 'CNat 'CNat = 'CInt
  evalOp _ (CvNat i) (CvNat j) = Right $ CvInt (toInteger i - toInteger j)
instance ArithOp Sub 'CInt 'CInt where
  type ArithRes Sub 'CInt 'CInt = 'CInt
  evalOp _ (CvInt i) (CvInt j) = Right $ CvInt (i - j)
instance ArithOp Sub 'CTimestamp 'CInt where
  type ArithRes Sub 'CTimestamp 'CInt = 'CTimestamp
  evalOp _ (CvTimestamp i) (CvInt j) =
    Right $ CvTimestamp $ timestampFromSeconds $ timestampToSeconds i - j
instance ArithOp Sub 'CTimestamp 'CTimestamp where
  type ArithRes Sub 'CTimestamp 'CTimestamp = 'CInt
  evalOp _ (CvTimestamp i) (CvTimestamp j) =
    Right $ CvInt $ timestampToSeconds i - timestampToSeconds j
instance ArithOp Sub 'CMutez 'CMutez where
  type ArithRes Sub 'CMutez 'CMutez = 'CMutez
  evalOp _ n@(CvMutez i) m@(CvMutez j) = res
    where
      res = maybe (Left $ MutezArithError SubUnderflow n m) (Right . CvMutez) $ i `subMutez` j

instance ArithOp Mul 'CNat 'CInt where
  type ArithRes Mul 'CNat 'CInt = 'CInt
  evalOp _ (CvNat i) (CvInt j) = Right $ CvInt (toInteger i * j)
instance ArithOp Mul 'CInt 'CNat where
  type ArithRes Mul 'CInt 'CNat = 'CInt
  evalOp _ (CvInt i) (CvNat j) = Right $ CvInt (i * toInteger j)
instance ArithOp Mul 'CNat 'CNat where
  type ArithRes Mul 'CNat 'CNat = 'CNat
  evalOp _ (CvNat i) (CvNat j) = Right $ CvNat (i * j)
instance ArithOp Mul 'CInt 'CInt where
  type ArithRes Mul 'CInt 'CInt = 'CInt
  evalOp _ (CvInt i) (CvInt j) = Right $ CvInt (i * j)
instance ArithOp Mul 'CNat 'CMutez where
  type ArithRes Mul 'CNat 'CMutez = 'CMutez
  evalOp _ n@(CvNat i) m@(CvMutez j) = res
    where
      res = maybe (Left $ MutezArithError MulOverflow n m) (Right . CvMutez) $ j `mulMutez` i
instance ArithOp Mul 'CMutez 'CNat where
  type ArithRes Mul 'CMutez 'CNat = 'CMutez
  evalOp _ n@(CvMutez i) m@(CvNat j) = res
    where
      res = maybe (Left $ MutezArithError MulOverflow n m) (Right . CvMutez) $ i `mulMutez` j

instance UnaryArithOp Abs 'CInt where
  type UnaryArithRes Abs 'CInt = 'CNat
  evalUnaryArithOp _ (CvInt i) = CvNat (fromInteger $ abs i)

instance UnaryArithOp Neg 'CInt where
  type UnaryArithRes Neg 'CInt = 'CInt
  evalUnaryArithOp _ (CvInt i) = CvInt (-i)
instance UnaryArithOp Neg 'CNat where
  type UnaryArithRes Neg 'CNat = 'CInt
  evalUnaryArithOp _ (CvNat i) = CvInt (- fromIntegral i)

instance ArithOp Or 'CNat 'CNat where
  type ArithRes Or 'CNat 'CNat = 'CNat
  evalOp _ (CvNat i) (CvNat j) = Right $ CvNat (i .|. j)
instance ArithOp Or 'CBool 'CBool where
  type ArithRes Or 'CBool 'CBool = 'CBool
  evalOp _ (CvBool i) (CvBool j) = Right $ CvBool (i .|. j)

instance ArithOp And 'CInt 'CNat where
  type ArithRes And 'CInt 'CNat = 'CInt
  evalOp _ (CvInt i) (CvNat j) = Right $ CvInt (i .&. fromIntegral j)
instance ArithOp And 'CNat 'CNat where
  type ArithRes And 'CNat 'CNat = 'CNat
  evalOp _ (CvNat i) (CvNat j) = Right $ CvNat (i .&. j)
instance ArithOp And 'CBool 'CBool where
  type ArithRes And 'CBool 'CBool = 'CBool
  evalOp _ (CvBool i) (CvBool j) = Right $ CvBool (i .&. j)

instance ArithOp Xor 'CNat 'CNat where
  type ArithRes Xor 'CNat 'CNat = 'CNat
  evalOp _ (CvNat i) (CvNat j) = Right $ CvNat (i `xor` j)
instance ArithOp Xor 'CBool 'CBool where
  type ArithRes Xor 'CBool 'CBool = 'CBool
  evalOp _ (CvBool i) (CvBool j) = Right $ CvBool (i `xor` j)

instance ArithOp Lsl 'CNat 'CNat where
  type ArithRes Lsl 'CNat 'CNat = 'CNat
  evalOp _ n@(CvNat i) m@(CvNat j) =
    if j > 256
    then Left $ ShiftArithError LslOverflow n m
    else Right $ CvNat (fromInteger $ shift (toInteger i) (fromIntegral j))

instance ArithOp Lsr 'CNat 'CNat where
  type ArithRes Lsr 'CNat 'CNat = 'CNat
  evalOp _ n@(CvNat i) m@(CvNat j) =
    if j > 256
    then Left $ ShiftArithError LsrUnderflow n m
    else Right $ CvNat (fromInteger $ shift (toInteger i) (-(fromIntegral j)))

instance UnaryArithOp Not 'CInt where
  type UnaryArithRes Not 'CInt = 'CInt
  evalUnaryArithOp _ (CvInt i) = CvInt (complement i)
instance UnaryArithOp Not 'CNat where
  type UnaryArithRes Not 'CNat = 'CInt
  evalUnaryArithOp _ (CvNat i) = CvInt (complement $ toInteger i)
instance UnaryArithOp Not 'CBool where
  type UnaryArithRes Not 'CBool = 'CBool
  evalUnaryArithOp _ (CvBool i) = CvBool (not i)

instance ArithOp Compare 'CBool 'CBool where
  type ArithRes Compare 'CBool 'CBool = 'CInt
  evalOp _ (CvBool i) (CvBool j) =
    Right $ CvInt $ toInteger $ fromEnum (compare i j) - 1
instance ArithOp Compare 'CAddress 'CAddress where
  type ArithRes Compare 'CAddress 'CAddress = 'CInt
  evalOp _ (CvAddress i) (CvAddress j) =
    Right $ CvInt $ toInteger $ fromEnum (compare i j) - 1
instance ArithOp Compare 'CNat 'CNat where
  type ArithRes Compare 'CNat 'CNat = 'CInt
  evalOp _ (CvNat i) (CvNat j) =
    Right $ CvInt $ toInteger $ fromEnum (compare i j) - 1
instance ArithOp Compare 'CInt 'CInt where
  type ArithRes Compare 'CInt 'CInt = 'CInt
  evalOp _ (CvInt i) (CvInt j) =
    Right $ CvInt $ toInteger $ fromEnum (compare i j) - 1
instance ArithOp Compare 'CString 'CString where
  type ArithRes Compare 'CString 'CString = 'CInt
  evalOp _ (CvString i) (CvString j) =
    Right $ CvInt $ toInteger $ fromEnum (compare i j) - 1
instance ArithOp Compare 'CBytes 'CBytes where
  type ArithRes Compare 'CBytes 'CBytes = 'CInt
  evalOp _ (CvBytes i) (CvBytes j) =
    Right $ CvInt $ toInteger $ fromEnum (compare i j) - 1
instance ArithOp Compare 'CTimestamp 'CTimestamp where
  type ArithRes Compare 'CTimestamp 'CTimestamp = 'CInt
  evalOp _ (CvTimestamp i) (CvTimestamp j) =
    Right $ CvInt $ toInteger $ fromEnum (compare i j) - 1
instance ArithOp Compare 'CMutez 'CMutez where
  type ArithRes Compare 'CMutez 'CMutez = 'CInt
  evalOp _ (CvMutez i) (CvMutez j) = Right $
    CvInt $ toInteger $ fromEnum (compare i j) - 1
instance ArithOp Compare 'CKeyHash 'CKeyHash where
  type ArithRes Compare 'CKeyHash 'CKeyHash = 'CInt
  evalOp _ (CvKeyHash i) (CvKeyHash j) =
    Right $ CvInt $ toInteger $ fromEnum (compare i j) - 1

instance UnaryArithOp Eq' 'CInt where
  type UnaryArithRes Eq' 'CInt = 'CBool
  evalUnaryArithOp _ (CvInt i) = CvBool (i == 0)

instance UnaryArithOp Neq 'CInt where
  type UnaryArithRes Neq 'CInt = 'CBool
  evalUnaryArithOp _ (CvInt i) = CvBool (i /= 0)


instance UnaryArithOp Lt 'CInt where
  type UnaryArithRes Lt 'CInt = 'CBool
  evalUnaryArithOp _ (CvInt i) = CvBool (i < 0)

instance UnaryArithOp Gt 'CInt where
  type UnaryArithRes Gt 'CInt = 'CBool
  evalUnaryArithOp _ (CvInt i) = CvBool (i > 0)

instance UnaryArithOp Le 'CInt where
  type UnaryArithRes Le 'CInt = 'CBool
  evalUnaryArithOp _ (CvInt i) = CvBool (i <= 0)

instance UnaryArithOp Ge 'CInt where
  type UnaryArithRes Ge 'CInt = 'CBool
  evalUnaryArithOp _ (CvInt i) = CvBool (i >= 0)


instance Buildable ArithErrorType where
  build AddOverflow = "add overflow"
  build MulOverflow = "mul overflow"
  build SubUnderflow = "sub overflow"
  build LslOverflow = "lsl overflow"
  build LsrUnderflow = "lsr underflow"

instance (Show n, Show m) => Buildable (ArithError n m) where
  build (MutezArithError errType n m) = "Mutez "
    <> build errType <> " with " <> show n <> ", " <> show m
  build (ShiftArithError errType n m) =
    build errType <> " with " <> show n <> ", " <> show m
