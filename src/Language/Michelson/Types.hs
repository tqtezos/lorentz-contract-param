{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude             #-}

module Language.Michelson.Types where

import           Data.Natural
import           Data.Maybe
import           Data.Sequence as Seq
import qualified Data.Text     as T
import qualified Data.ByteString     as B
import Prelude (Show, Integer, (.))


-- smart contract
data Contract = Contract Parameter Storage Code deriving Show

-- parameter
data Parameter = Parameter Type deriving Show

data Storage = Storage Type deriving Show

data Code = Code Ops deriving Show

-- element of a map
data Element = Element Data Data deriving Show

-- data
data Data where
  Int        :: Integer -> Data
  String     :: T.Text -> Data
  Bytes      :: B.ByteString -> Data
  Unit       :: Data
  True       :: Data
  False      :: Data
  Pair       :: Data -> Data -> Data
  Left       :: Data -> Data
  Right      :: Data -> Data
  Some       :: Data -> Data
  None       :: Data
  Seq        :: Seq Data -> Data
  Map        :: Seq Element -> Data
  DataOps    :: Ops -> Data
  deriving Show

-- instruction sequence
data Ops = Ops { ops :: Seq Op } deriving Show

opsConcat :: Ops -> Ops -> Ops
opsConcat x y = Ops ((ops x) Seq.>< (ops y))

(><) :: Ops -> Ops -> Ops
infixr 9 ><
(><) = opsConcat

opsLappend :: Op -> Ops -> Ops
opsLappend x y = Ops (x Seq.<| (ops y))

(<|) :: Op -> Ops -> Ops
infixr 9 <|
(<|) = opsLappend

opsRappend :: Ops -> Op -> Ops
opsRappend x y = Ops ((ops x) Seq.|> y)

(|>) :: Ops -> Op -> Ops
infixr 9 |>
(|>) = opsRappend

noOps :: Ops
noOps = Ops Seq.empty

opsFromList :: [Op] -> Ops
opsFromList = Ops . Seq.fromList

(|:) :: [Op] -> Ops
infixr 9 |:
(|:) = opsFromList

opsSingleton :: Op -> Ops
opsSingleton x = opsFromList [x]

-- instruction
data Op where
  OpsSeq            :: Ops -> Op
  DROP              :: Op
  DUP               :: VarAnnotation -> Op
  SWAP              :: Op
  PUSH              :: VarAnnotation -> Type -> Data -> Op
  SOME              :: TypeAnnotation
                       -> VarAnnotation
                       -> FieldAnnotation
                       -> Op
  NONE              :: TypeAnnotation
                       -> VarAnnotation
                       -> FieldAnnotation
                       -> Type
                       -> Op
  UNIT              :: TypeAnnotation -> Op
  IF_NONE           :: Ops -> Ops -> Op
  PAIR              :: TypeAnnotation
                       -> VarAnnotation
                       -> FieldAnnotation
                       -> FieldAnnotation
                       -> Op
  CAR               :: VarAnnotation -> FieldAnnotation -> Op
  CDR               :: VarAnnotation -> FieldAnnotation -> Op
  LEFT              :: TypeAnnotation
                       -> VarAnnotation
                       -> FieldAnnotation
                       -> FieldAnnotation
                       -> Type
                       -> Op
  RIGHT             :: TypeAnnotation
                       -> VarAnnotation
                       -> FieldAnnotation
                       -> FieldAnnotation
                       -> Type
                       -> Op
  IF_LEFT           :: Ops -> Ops -> Op
  IF_RIGHT          :: Ops -> Ops -> Op
  NIL               :: TypeAnnotation -> VarAnnotation -> Type -> Op
  CONS              :: VarAnnotation -> Op
  IF_CONS           :: Ops -> Ops -> Op
  SIZE              :: VarAnnotation -> Op
  EMPTY_SET         :: TypeAnnotation -> VarAnnotation -> ComparableType -> Op
  EMPTY_MAP         :: TypeAnnotation
                       -> VarAnnotation
                       -> ComparableType
                       -> Type
                       -> Op
  MAP               :: VarAnnotation -> Ops -> Op
  ITER              :: VarAnnotation -> Ops -> Op
  MEM               :: VarAnnotation -> Op
  GET               :: VarAnnotation -> Op
  UPDATE            :: Op
  IF                :: Ops -> Ops -> Op
  LOOP              :: Ops -> Op
  LOOP_LEFT         :: Ops -> Op
  LAMBDA            :: VarAnnotation -> Type -> Type -> Ops -> Op
  EXEC              :: VarAnnotation -> Op
  DIP               :: Ops -> Op
  FAILWITH          :: Op
  CAST              :: TypeAnnotation -> VarAnnotation -> Op
  RENAME            :: VarAnnotation -> Op
  CONCAT            :: VarAnnotation -> Op
  SLICE             :: Op
  PACK              :: Op
  UNPACK            :: Op
  ADD               :: VarAnnotation -> Op
  SUB               :: VarAnnotation -> Op
  MUL               :: VarAnnotation -> Op
  EDIV              :: VarAnnotation -> Op
  ABS               :: VarAnnotation -> Op
  NEG               :: Op
  MOD               :: Op
  LSL               :: VarAnnotation -> Op
  LSR               :: VarAnnotation -> Op
  OR                :: VarAnnotation -> Op
  AND               :: VarAnnotation -> Op
  NOT               :: VarAnnotation -> Op
  COMPARE           :: VarAnnotation -> Op
  EQ                :: VarAnnotation -> Op
  NEQ               :: VarAnnotation -> Op
  LT                :: VarAnnotation -> Op
  GT                :: VarAnnotation -> Op
  LE                :: VarAnnotation -> Op
  GE                :: VarAnnotation -> Op
  INT               :: VarAnnotation -> Op
  SELF              :: VarAnnotation -> Op
  TRANSFER_TOKENS   :: Op
  SET_DELEGATE      :: Op
  CREATE_ACCOUNT    :: VarAnnotation -> VarAnnotation -> Op
  CREATE_CONTRACT   :: VarAnnotation -> VarAnnotation -> Op
  CREATE_CONTRACT2  :: VarAnnotation -> VarAnnotation -> Ops -> Op
  IMPLICIT_ACCOUNT  :: VarAnnotation -> Op
  NOW               :: VarAnnotation -> Op
  AMOUNT            :: VarAnnotation -> Op
  BALANCE           :: VarAnnotation -> Op
  CHECK_SIGNATURE   :: VarAnnotation -> Op
  BLAKE2B           :: VarAnnotation -> Op
  HASH_KEY          :: VarAnnotation -> Op
  STEPS_TO_QUOTA    :: VarAnnotation -> Op
  SOURCE            :: VarAnnotation -> Op
  SENDER            :: VarAnnotation -> Op
  ADDRESS           :: VarAnnotation -> Op
  deriving Show

-- type
data Type where
  T_comparable :: TypeAnnotation -> ComparableType -> Type
  T_key        :: TypeAnnotation -> Type
  T_unit       :: TypeAnnotation -> Type
  T_signature  :: TypeAnnotation -> Type
  T_option     :: TypeAnnotation -> FieldAnnotation -> Type -> Type
  T_list       :: TypeAnnotation -> Type -> Type
  T_set        :: TypeAnnotation -> ComparableType -> Type
  T_operation  :: TypeAnnotation -> Type
  T_contract   :: TypeAnnotation -> Type -> Type
  T_pair       :: TypeAnnotation
                  -> FieldAnnotation
                  -> FieldAnnotation
                  -> Type
                  -> Type
                  -> Type
  T_or         :: TypeAnnotation
                  -> FieldAnnotation
                  -> FieldAnnotation
                  -> Type
                  -> Type
                  -> Type
  T_lambda     :: TypeAnnotation -> Type -> Type -> Type
  T_map        :: TypeAnnotation -> ComparableType -> Type -> Type
  T_big_map    :: TypeAnnotation -> ComparableType -> Type -> Type
  deriving Show

-- comparable type
data ComparableType where
  T_int       :: ComparableType
  T_nat       :: ComparableType
  T_string    :: ComparableType
  T_bytes     :: ComparableType
  T_mutez     :: ComparableType
  T_bool      :: ComparableType
  T_key_hash  :: ComparableType
  T_timestamp :: ComparableType
  deriving Show

-- Annotation type
data TypeAnnotation = TypeAnnotation (Maybe T.Text) deriving Show-- Type Annotation
data FieldAnnotation = FieldAnnotation (Maybe T.Text) deriving Show-- Field Annotation
data VarAnnotation = VarAnnotation (Maybe T.Text) deriving Show-- Variable Annotation


