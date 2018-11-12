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

data Code = Code Instructions deriving Show

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
  DataOps    :: Instructions -> Data
  deriving Show

-- instruction sequence
data Instructions = Instructions { instructions :: Seq Op } deriving Show

noInstructions :: Instructions
noInstructions = Instructions Seq.empty

instructionsFromList :: [Op] -> Instructions
instructionsFromList = Instructions . Seq.fromList

-- instruction
data Op where
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
  IF_NONE           :: Instructions -> Instructions -> Op
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
  IF_LEFT           :: Instructions -> Instructions -> Op
  IF_RIGHT          :: Instructions -> Instructions -> Op
  NIL               :: TypeAnnotation -> VarAnnotation -> Type -> Op
  CONS              :: VarAnnotation -> Op
  IF_CONS           :: Instructions -> Instructions -> Op
  SIZE              :: VarAnnotation -> Op
  EMPTY_SET         :: TypeAnnotation -> VarAnnotation -> ComparableType -> Op
  EMPTY_MAP         :: TypeAnnotation
                       -> VarAnnotation
                       -> ComparableType
                       -> Type
                       -> Op
  MAP               :: VarAnnotation -> Instructions -> Op
  ITER              :: VarAnnotation -> Instructions -> Op
  MEM               :: VarAnnotation -> Op
  GET               :: VarAnnotation -> Op
  UPDATE            :: Op
  IF                :: Instructions -> Instructions -> Op
  LOOP              :: Instructions -> Op
  LOOP_LEFT         :: Instructions -> Op
  LAMBDA            :: VarAnnotation -> Type -> Type -> Instructions -> Op
  EXEC              :: VarAnnotation -> Op
  DIP               :: Instructions -> Op
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
  CREATE_CONTRACT2  :: VarAnnotation -> VarAnnotation -> Instructions -> Op
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
