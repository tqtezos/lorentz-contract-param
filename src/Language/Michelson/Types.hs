{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Michelson.Types where

import           Data.Natural
import           Data.Sequence
import qualified Data.Text     as T
import           Prelude       (Integer, Show)

data SC = SC Param Storage Code deriving Show

data Param = Param T deriving Show

data Storage = Storage T deriving Show

data Code = Code ISeq deriving Show

data Elt = Elt D D deriving Show

data D where
  LInt        :: Integer -> D
  LNat        :: Natural -> D
  LString     :: T.Text -> D
  LTimestamp  :: T.Text -> D
  LSignature  :: T.Text -> D
  LKey        :: T.Text -> D
  LKey_HASH   :: T.Text -> D
  LTez        :: T.Text -> D
  LContract   :: T.Text -> D
  DUnit       :: D
  DTrue       :: D
  DFalse      :: D
  DPair       :: D -> D -> D
  DLeft       :: D -> D
  DRight      :: D -> D
  DSome       :: D -> D
  DNone       :: D
  DList       :: Seq D -> D
  DMap        :: Seq Elt -> D
  DInst       :: I -> D
  deriving Show

data ISeq = ISeq (Seq I) deriving Show

data I where
  DROP              :: I
  DUP               :: I
  SWAP              :: I
  PUSH              :: T -> D -> I
  SOME              :: I
  NONE              :: T -> I
  UNIT              :: I
  IF_NONE           :: ISeq -> ISeq -> I
  PAIR              :: I
  CAR               :: I
  CDR               :: I
  LEFT              :: T -> I
  RIGHT             :: T -> I
  IF_LEFT           :: ISeq -> ISeq -> I
  IF_RIGHT          :: ISeq -> ISeq -> I
  NIL               :: T -> I
  CONS              :: I
  IF_CONS           :: ISeq -> ISeq -> I
  EMPTY_SET         :: CT -> I
  EMPTY_MAP         :: CT -> T -> I
  MAP               :: ISeq -> I
  ITER              :: ISeq -> I
  MEM               :: I
  GET               :: I
  UPDATE            :: I
  IF                :: ISeq -> ISeq -> I
  LOOP              :: ISeq -> I
  LOOP_LEFT         :: ISeq -> I
  LAMBDA            :: T -> T -> ISeq -> I
  EXEC              :: I
  DIP               :: ISeq -> I
  FAILWITH          :: D -> I
  CAST              :: I
  RENAME            :: I
  CONCAT            :: I
  SLICE             :: I
  PACK              :: I
  UNPACK            :: I
  ADD               :: I
  SUB               :: I
  MUL               :: I
  DIV               :: I
  ABS               :: I
  NEG               :: I
  MOD               :: I
  LSL               :: I
  LSR               :: I
  OR                :: I
  AND               :: I
  NOT               :: I
  COMPARE           :: I
  EQ                :: I
  NEQ               :: I
  LT                :: I
  GT                :: I
  LE                :: I
  GE                :: I
  INT               :: I
  SELF              :: I
  TRANSFER_TOKENS   :: I
  SET_DELEGATE      :: I
  CREATE_ACCOUNT    :: I
  CREATE_CONTRACT   :: I
  CREATE_CONTRACT2  :: ISeq -> I
  IMPLICIT_ACCOUNT  :: I
  NOW               :: I
  AMOUNT            :: I
  BALANCE           :: I
  CHECK_SIGNATURE   :: I
  BLAKE2B           :: I
  HASH_KEY          :: I
  STEPS_TO_QUOTA    :: I
  SOURCE            :: I
  SENDER            :: I
  ADDRESS           :: I
  deriving Show

data T where
  T_comparable :: CT -> T
  T_key        :: T
  T_unit       :: T
  T_signature  :: T
  T_option     :: T -> T
  T_list       :: T -> T
  T_set        :: CT -> T
  T_contract   :: T -> T
  T_pair       :: T -> T -> T
  T_or         :: T -> T -> T
  T_lambda     :: T -> T -> T
  T_map        :: CT -> T -> T
  T_big_map    :: CT -> T -> T
  deriving Show

data CT where
  T_int       :: CT
  T_nat       :: CT
  T_string    :: CT
  T_bytes     :: CT
  T_mutez     :: CT
  T_bool      :: CT
  T_key_hash  :: CT
  T_timestamp :: CT
  deriving Show
