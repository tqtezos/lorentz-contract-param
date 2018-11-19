{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude             #-}

module Language.Michelson.Types where

import           Data.Natural
import           Data.Maybe
import           Data.Sequence as Seq
import qualified Data.Text     as T
import qualified Data.ByteString     as B
import Text.Show
import Prelude (Integer, (.), (++), Eq, Ord)


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

data PairStruct = Leaf | Nest PairStruct PairStruct deriving Show
data CadrStruct = A | D deriving Show

-- instruction
data Op where
  CMP_MAC           :: Op -> Op
  IF_MAC            :: Op -> Ops -> Ops -> Op
  IFCMP_MAC         :: Op -> Ops -> Ops ->Op
  FAIL_MAC          :: Op
  PAIR_MAC          :: PairStruct -> VarNote -> [FieldNote] -> Op
  UNPAIR_MAC        :: PairStruct -> [VarNote] -> [FieldNote] -> Op
  CADR_MAC          :: [CadrStruct] -> VarNote -> FieldNote -> Op
  SET_CADR_MAC      :: [CadrStruct] -> VarNote -> FieldNote -> Op
  MAP_CADR_MAC      :: [CadrStruct] -> VarNote -> FieldNote -> Ops -> Op
  DIP_MAC           :: Integer -> Ops -> Op
  DUP_MAC           :: Integer -> VarNote -> Op
  ASSERT_MAC        :: Op
  ASSERTX_MAC       :: Op -> Op
  ASSERT_CMP_MAC    :: Op -> Op
  ASSERT_NONE_MAC   :: Op
  ASSERT_SOME_MAC   :: Op
  ASSERT_LEFT_MAC   :: Op
  ASSERT_RIGHT_MAC  :: Op
  IF_SOME_MAC       :: Ops -> Ops -> Op
  OpsSeq            :: Ops -> Op
  DROP              :: Op
  DUP               :: VarNote -> Op
  SWAP              :: Op
  PUSH              :: VarNote -> Type -> Data -> Op
  SOME              :: TypeNote -> VarNote -> FieldNote
                       -> Op
  NONE              :: TypeNote -> VarNote -> FieldNote
                       -> Type -> Op
  UNIT              :: TypeNote -> VarNote -> Op
  IF_NONE           :: Ops -> Ops -> Op
  PAIR              :: TypeNote -> VarNote -> FieldNote -> FieldNote
                       -> Op
  CAR               :: VarNote -> FieldNote -> Op
  CDR               :: VarNote -> FieldNote -> Op
  LEFT              :: TypeNote -> VarNote -> FieldNote -> FieldNote
                       -> Type -> Op
  RIGHT             :: TypeNote -> VarNote -> FieldNote -> FieldNote
                       -> Type -> Op
  IF_LEFT           :: Ops -> Ops -> Op
  IF_RIGHT          :: Ops -> Ops -> Op
  NIL               :: TypeNote -> VarNote -> Type -> Op
  CONS              :: VarNote -> Op
  IF_CONS           :: Ops -> Ops -> Op
  SIZE              :: VarNote -> Op
  EMPTY_SET         :: TypeNote -> VarNote -> Comparable -> Op
  EMPTY_MAP         :: TypeNote -> VarNote
                       -> Comparable -> Type -> Op
  MAP               :: VarNote -> Ops -> Op
  ITER              :: VarNote -> Ops -> Op
  MEM               :: VarNote -> Op
  GET               :: VarNote -> Op
  UPDATE            :: Op
  IF                :: Ops -> Ops -> Op
  LOOP              :: Ops -> Op
  LOOP_LEFT         :: Ops -> Op
  LAMBDA            :: VarNote -> Type -> Type -> Ops -> Op
  EXEC              :: VarNote -> Op
  DIP               :: Ops -> Op
  FAILWITH          :: Op
  CAST              :: TypeNote -> VarNote -> Op
  RENAME            :: VarNote -> Op
  PACK              :: VarNote -> Op
  UNPACK            :: VarNote -> Type -> Op
  CONCAT            :: VarNote -> Op
  SLICE             :: VarNote -> Op
  ISNAT             :: Op
  ADD               :: VarNote -> Op
  SUB               :: VarNote -> Op
  MUL               :: VarNote -> Op
  EDIV              :: VarNote -> Op
  ABS               :: VarNote -> Op
  NEG               :: Op
  MOD               :: Op
  LSL               :: VarNote -> Op
  LSR               :: VarNote -> Op
  OR                :: VarNote -> Op
  AND               :: VarNote -> Op
  XOR               :: VarNote -> Op
  NOT               :: VarNote -> Op
  COMPARE           :: VarNote -> Op
  EQ                :: VarNote -> Op
  NEQ               :: VarNote -> Op
  LT                :: VarNote -> Op
  GT                :: VarNote -> Op
  LE                :: VarNote -> Op
  GE                :: VarNote -> Op
  INT               :: VarNote -> Op
  SELF              :: VarNote -> Op
  CONTRACT          :: Type -> Op
  TRANSFER_TOKENS   :: VarNote -> Op
  SET_DELEGATE      :: Op
  CREATE_ACCOUNT    :: VarNote -> VarNote -> Op
  CREATE_CONTRACT   :: VarNote -> VarNote -> Op
  CREATE_CONTRACT2  :: VarNote -> VarNote -> Contract -> Op
  IMPLICIT_ACCOUNT  :: VarNote -> Op
  NOW               :: VarNote -> Op
  AMOUNT            :: VarNote -> Op
  BALANCE           :: VarNote -> Op
  CHECK_SIGNATURE   :: VarNote -> Op
  SHA256            :: VarNote -> Op
  SHA512            :: VarNote -> Op
  BLAKE2B           :: VarNote -> Op
  HASH_KEY          :: VarNote -> Op
  STEPS_TO_QUOTA    :: VarNote -> Op
  SOURCE            :: VarNote -> Op
  SENDER            :: VarNote -> Op
  ADDRESS           :: VarNote -> Op
  deriving Show

-- type

data TypeNote = TypeNote (Maybe T.Text) deriving (Show, Eq, Ord)
data FieldNote = FieldNote (Maybe T.Text) deriving (Show, Eq, Ord)
data VarNote = VarNote (Maybe T.Text) deriving (Show, Eq, Ord)

data Type = Type T TypeNote FieldNote deriving Show

--instance Show Type where
--  show (Type (T_comparable ct) tn fn) = (show ct) ++ (show tn) ++ (show fn)
--  show (Type t tn fn) = (show t) ++ (show tn) ++ (show fn)

data T where
  T_comparable :: CT -> T
  T_key        :: T
  T_unit       :: T
  T_signature  :: T
  T_option     :: Type -> T
  T_list       :: Type -> T
  T_set        :: Comparable -> T
  T_operation  :: T
  T_contract   :: Type -> T
  T_pair       :: Type -> Type -> T
  T_or         :: Type -> Type -> T
  T_lambda     :: Type -> Type -> T
  T_map        :: Comparable -> Type -> T
  T_big_map    :: Comparable -> Type -> T
  deriving Show

-- comparable type
data Comparable = Comparable CT TypeNote deriving Show

-- instance Show Comparable where
--   show (Comparable t tn) = (show t) ++ (show tn)

data CT where
  T_int       :: CT
  T_nat       :: CT
  T_string    :: CT
  T_bytes     :: CT
  T_mutez     :: CT
  T_bool      :: CT
  T_key_hash  :: CT
  T_timestamp :: CT
  T_address   :: CT
  deriving Show

-- Note type

--instance Show TypeNote where
--  show (TypeNote Nothing) = ""
--  show (TypeNote (Just tn)) = ':' : (T.unpack tn)
--
--instance Show FieldNote where
--  show (FieldNote Nothing) = ""
--  show (FieldNote (Just fn)) = '%' : (T.unpack fn)
--
--instance Show VarNote where
--  show (VarNote Nothing) = ""
--  show (VarNote (Just vn)) = '@' : (T.unpack vn)

noTN = TypeNote Nothing
noFN = FieldNote Nothing
noVN = VarNote Nothing

