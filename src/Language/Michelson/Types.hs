{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Michelson.Types where

import Data.Sequence
import qualified Data.Text as T
import Prelude (Integer, Show)
import Data.Natural

{-
type Prim = K | D | I | T

data K where
  K_parameter :: K
  K_storage   :: K
  K_code      :: K
-}

data Elt = Elt D D deriving Show

data D where
  LInt        :: Integer -> D
  LNat        :: Natural -> D
  LString     :: T.Text -> D
  LTimestamp  :: T.Text -> D
  LSignature  :: T.Text -> D
  LTez        :: T.Text -> D
  LKey        :: T.Text -> D
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
  DSet        :: Seq D -> D
  DMap        :: Seq Elt -> D
  DInst       :: I -> D
  deriving Show

data I where
  ISeq              :: Seq I -> I
  DROP              :: I
  DUP               :: I
  SWAP              :: I
  PUSH              :: T -> D -> I
  SOME              :: I
  NONE              :: T -> I
  IF_NONE           :: I -> I -> I
  PAIR              :: I
  CAR               :: I
  CDR               :: I
  LEFT              :: T -> I
  RIGHT             :: T -> I
  IF_LEFT           :: I -> I -> I
  NIL               :: T -> I
  CONS              :: I
  IF_CONS           :: I-> I -> I
  EMPTY_SET         :: T -> I
  EMPTY_MAP         :: CT -> T -> I
  MAP               :: I -> I
  ITER              :: I -> I
  MEM               :: I
  GET               :: I
  UPDATE            :: I
  IF                :: I -> I -> I
  LOOP              :: I -> I
  LOOP_LEFT         :: I -> I
  LAMBDA            :: T -> T -> I -> I
  EXEC              :: I
  DIP               :: I -> I
  FAILWITH          :: I
  CAST              :: I
  RENAME            :: I
  CONCAT            :: I
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
  deriving Show

data T where
  T_comparable :: CT -> T
  T_key        :: T
  T_unit       :: T
  T_signature  :: T
  T_option     :: T -> T
  T_list       :: T -> T
  T_set        :: CT -> T
  T_contract   :: T -> T -> T
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
  T_tez       :: CT
  T_bool      :: CT
  T_key_hash  :: CT
  T_timestamp :: CT
  deriving Show




