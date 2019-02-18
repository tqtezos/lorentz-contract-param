-- | Module, containing data types for Michelson value
-- and instruction / instrunction sequence.

module Advanced.Value.Value
  ( Val (..)
  , Instr (..)
  , (#)
  , EDivOp (..)
  , ModOp (..)
  , MemOp (..)
  , MapOp (..)
  , IterOp (..)
  , SizeOp
  , GetOp (..)
  , UpdOp (..)
  , SliceOp
  , ConcatOp
  ) where

import qualified Data.Map as M
import qualified Data.Set as S

import Advanced.Type (CT(..), T(..))
import Advanced.Value.Arith
import Advanced.Value.CValue (Address, CVal(..))
import Tezos.Crypto (PublicKey, Signature)

-- | Representation of Michelson value.
--
-- Type parameter @op@ states for intrinsic operation
-- (list of which is returned by the contract to be executed
-- afterwards).
--
-- Type parameter @ref@ states for big map reference (as big map is
-- normally not fully loaded into memory).
data Val op cp t where
  VC :: CVal t -> Val op cp ('T_c t)
  VKey :: PublicKey -> Val op cp 'T_key
  VUnit :: Val op cp 'T_unit
  VSignature :: Signature -> Val op cp 'T_signature
  VOption :: Maybe (Val op cp t) -> Val op cp ('T_option t)
  VList :: [Val op cp t] -> Val op cp ('T_list t)
  VSet :: Set (CVal t) -> Val op cp ('T_set t)
  VOp :: op -> Val op cp 'T_operation
  VContract :: Address -> Val op cp ('T_contract p)
  VPair :: (Val op cp l, Val op cp r) -> Val op cp ('T_pair l r)
  VOr :: Either (Val op cp l) (Val op cp r) -> Val op cp ('T_or l r)
  VLam :: Instr op cp (inp ': '[]) (out ': '[]) -> Val op cp ('T_lambda inp out)
  VMap :: Map (CVal k) (Val op cp v) -> Val op cp ('T_map k v)
  VBigMap :: Map (CVal k) (Val op cp v) -> Val op cp ('T_big_map k v)

-- TODO: actually we should handle big maps with something close
-- to following:
--
--  VBigMap :: BigMap op ref k v -> Val op cp ('T_big_map k v)

deriving instance Show op => Show (Val op cp t)

-- data ValueOp v
--     = New v
--     | Upd v
--     | Rem
--     | NotExisted
--
-- data BigMap op ref k v = BigMap
--  { bmRef :: ref k v, bmChanges :: Map (CVal k) (ValueOp (Val op cp v)) }

-- | Infix version of @Seq@ constructor.
--
-- One can represent sequence of Michelson opertaions as follows:
-- @SWAP; DROP; DUP;@ -> @SWAP # DROP # DUP@.
(#) :: Instr op cp a b -> Instr op cp b c -> Instr op cp a c
(#) = Seq

infixl 0 #

class EDivOp op cp (n :: CT) (m :: CT) where
  type EDivOpRes n m :: CT
  type EModOpRes n m :: CT
  evalEDivOp :: CVal n
             -> CVal m
             -> Val op cp ('T_option ('T_pair ('T_c (EDivOpRes n m)) ('T_c (EModOpRes n m))))

instance EDivOp op cp 'T_int 'T_int where
  type EDivOpRes 'T_int 'T_int = 'T_int
  type EModOpRes 'T_int 'T_int = 'T_nat
  evalEDivOp (CvInt i) (CvInt j) =
    if j == 0
      then VOption $ Nothing
      else VOption $ Just $
        VPair (VC $ CvInt (div i j), VC $ CvNat $ fromInteger (mod i j))
instance EDivOp op cp 'T_int 'T_nat where
  type EDivOpRes 'T_int 'T_nat = 'T_int
  type EModOpRes 'T_int 'T_nat = 'T_nat
  evalEDivOp (CvInt i) (CvNat j) =
    if j == 0
      then VOption $ Nothing
      else VOption $ Just $
        VPair (VC $ CvInt (div i (toInteger j)), VC $ CvNat $ (mod (fromInteger i) j))
instance EDivOp op cp 'T_nat 'T_int where
  type EDivOpRes 'T_nat 'T_int = 'T_int
  type EModOpRes 'T_nat 'T_int = 'T_nat
  evalEDivOp (CvNat i) (CvInt j) =
    if j == 0
      then VOption $ Nothing
      else VOption $ Just $
        VPair (VC $ CvInt (div (toInteger i) j), VC $ CvNat $ (mod i (fromInteger j)))
instance EDivOp op cp 'T_nat 'T_nat where
  type EDivOpRes 'T_nat 'T_nat = 'T_nat
  type EModOpRes 'T_nat 'T_nat = 'T_nat
  evalEDivOp (CvNat i) (CvNat j) =
    if j == 0
      then VOption $ Nothing
      else VOption $ Just $
        VPair (VC $ CvNat (div i j), VC $ CvNat $ (mod i j))
instance EDivOp op cp 'T_mutez 'T_mutez where
  type EDivOpRes 'T_mutez 'T_mutez = 'T_mutez
  type EModOpRes 'T_mutez 'T_mutez = 'T_mutez
  evalEDivOp (CvMutez i) (CvMutez j) =
    if j == 0
      then VOption $ Nothing
      else VOption $ Just $
        VPair (VC $ CvMutez (div i j), VC $ CvMutez $ (mod i j))
instance EDivOp op cp 'T_mutez 'T_nat where
  type EDivOpRes 'T_mutez 'T_nat = 'T_nat
  type EModOpRes 'T_mutez 'T_nat = 'T_mutez
  evalEDivOp (CvMutez i) (CvNat j) =
    if j == 0
      then VOption $ Nothing
      else VOption $ Just $
        VPair (VC $ CvNat (div (fromIntegral i) j), VC $ CvMutez $ (mod i (fromIntegral j)))

class ModOp op cp (n :: CT) (m :: CT) where
  type ModOpRes n m :: CT
  evalModOp :: CVal n
             -> CVal m
             -> Val op cp ('T_option ('T_c (ModOpRes n m)))

instance ModOp op cp 'T_int 'T_int where
  type ModOpRes 'T_int 'T_int = 'T_nat
  evalModOp (CvInt i) (CvInt j) =
    if j == 0
      then VOption $ Nothing
      else VOption $ (Just . VC . CvNat . fromInteger) (mod i j)
instance ModOp op cp 'T_int 'T_nat where
  type ModOpRes 'T_int 'T_nat = 'T_nat
  evalModOp (CvInt i) (CvNat j) =
    if j == 0
      then VOption $ Nothing
      else VOption $ Just $ VC $ CvNat (mod (fromInteger i) j)
instance ModOp op cp 'T_nat 'T_int where
  type ModOpRes 'T_nat 'T_int = 'T_nat
  evalModOp (CvNat i) (CvInt j) =
    if j == 0
      then VOption $ Nothing
      else VOption $ Just $ VC $ CvNat (mod i (fromInteger j))
instance ModOp op cp 'T_nat 'T_nat where
  type ModOpRes 'T_nat 'T_nat = 'T_nat
  evalModOp (CvNat i) (CvNat j) =
    if j == 0
      then VOption $ Nothing
      else VOption $ Just $ VC $ CvNat (mod i j)


-- TODO support add operation for mutez
-- don't forget to throw arith exception (async exception)

-- | Representation of Michelson instruction or sequence
-- of instructions.
--
-- Each Michelson instruction is represented by exactly one
-- constructor of this data type. Sequence of instructions
-- is represented with use of @Seq@ constructor in following
-- way: @SWAP; DROP ; DUP;@ -> @SWAP `Seq` DROP `Seq` DUP@.
-- Special case where there are no instructions is represented
-- by constructor @Nop@, e.g. @IF_NONE {} { SWAP; DROP; }@ ->
-- @IF_NONE Nop (SWAP `Seq` DROP)@.
--
-- Type parameter @op@ states for intrinsic operation
-- (list of which is returned by the contract to be executed
-- afterwards).
--
-- Type parameter @inp@ states for input stack type. That is,
-- type of the stack that is required for operation to execute.
--
-- Type parameter @out@ states for output stack type or type
-- of stack that will be left after instruction's execution.
data Instr op cp (inp :: [T]) (out :: [T]) where
  Seq :: Instr op cp a b -> Instr op cp b c -> Instr op cp a c
  Nop :: Instr op cp s s
  -- ^ Nop operation. Missing in Michelson spec, added to parse construction
  -- like  `IF {} { SWAP; DROP; }`.

  DROP :: Instr op cp (a ': s) s
  DUP  :: Instr op cp (a ': s) (a ': a ': s)
  SWAP :: Instr op cp (a ': b ': s) (b ': a ': s)
  PUSH :: Val op cp t -> Instr op cp s (t ': s)
  SOME :: Instr op cp (a ': s) ('T_option a ': s)
  NONE :: forall a s op cp. Instr op cp s ('T_option a ': s)
  UNIT :: Instr op cp s ('T_unit ': s)
  IF_NONE
    :: Instr op cp s s' -> Instr op cp (a ': s) s' -> Instr op cp ('T_option a ': s) s'
  PAIR :: Instr op cp (a ': b ': s) ('T_pair a b ': s)
  CAR :: Instr op cp ('T_pair a b ': s) (a ': s)
  CDR :: Instr op cp ('T_pair a b ': s) (b ': s)
  LEFT :: Instr op cp (a ': s) ('T_or a b ': s)
  RIGHT :: Instr op cp (b ': s) ('T_or a b ': s)
  IF_LEFT :: Instr op cp (a ': s) s'
          -> Instr op cp (b ': s) s'
          -> Instr op cp ('T_or a b ': s) s'
  IF_RIGHT :: Instr op cp (b ': s) s'
          -> Instr op cp (a ': s) s'
          -> Instr op cp ('T_or a b ': s) s'
  NIL :: Instr op cp s ('T_list p ': s)
  CONS :: Instr op cp (a ': 'T_list a ': s) ('T_list a ': s)
  IF_CONS :: Instr op cp (a ': 'T_list a ': s) s'
          -> Instr op cp s s'
          -> Instr op cp ('T_list a ': s) s'
  SIZE :: SizeOp c => Instr op cp (c ': s) ('T_c 'T_nat ': s)
  EMPTY_SET :: Instr op cp s ('T_set e ': s)
  EMPTY_MAP :: Instr op cp s ('T_map a b ': s)
  MAP :: MapOp c => Instr op cp (MapOpInp c ': s) (b ': s)
      -> Instr op cp (c ': s) (MapOpRes c b ': s)
  ITER :: IterOp c => Instr op cp (IterOpEl c ': s) s -> Instr op cp (c ': s) s
  MEM :: MemOp c => Instr op cp ('T_c (MemOpKey c) ': c ': s) ('T_c 'T_bool ': s)
  GET :: GetOp c => Instr op cp ('T_c (GetOpKey c) ': c ': s) ('T_option (GetOpVal c) ': s)
  UPDATE :: UpdOp c => Instr op cp ('T_c (UpdOpKey c) ': UpdOpParams c ': c ': s) (c ': s)
  IF :: Instr op cp s s'
     -> Instr op cp s s'
     -> Instr op cp ('T_c 'T_bool ': s) s'
  LOOP :: Instr op cp s ('T_c 'T_bool ': s)
       -> Instr op cp ('T_c 'T_bool ': s) s
  LOOP_LEFT :: Instr op cp (a ': s) ('T_or a b ': s)
            -> Instr op cp ('T_or a b ': s) (b ': s)
  LAMBDA :: Val op cp ('T_lambda i o) -> Instr op cp s ('T_lambda i o ': s)
  EXEC :: Instr op cp (t1 ': 'T_lambda t1 t2 ': s) (t2 ': s)
  DIP :: Instr op cp a c -> Instr op cp (b ': a) (b ': c)
  FAILWITH :: Instr op cp s t
  CAST :: Instr op cp (a ': s) (a ': s)
  RENAME :: Instr op cp (a ': s) (a ': s)
  PACK :: Instr op cp (a ': s) ('T_c 'T_bytes ': s)
  UNPACK :: Instr op cp ('T_c 'T_bytes ': s) ('T_option a ': s)
  CONCAT :: ConcatOp c => Instr op cp (c ': c ': s) (c ': s)
  SLICE :: SliceOp c => Instr op cp ('T_c 'T_nat ': 'T_c 'T_nat ': c ': s) ('T_option c ': s)
  ISNAT :: Instr op cp ('T_c 'T_int ': s) ('T_option ('T_c 'T_nat) ': s)
  ADD :: ArithOp Add n m => Instr op cp ('T_c n ': 'T_c m ': s) ('T_c (ArithResT Add n m) ': s)
  SUB :: ArithOp Sub n m => Instr op cp ('T_c n ': 'T_c m ': s) ('T_c (ArithResT Sub n m) ': s)
  MUL :: ArithOp Mul n m => Instr op cp ('T_c n ': 'T_c m ': s) ('T_c (ArithResT Mul n m) ': s)
  EDIV :: EDivOp op cp n m => Instr op cp ('T_c n ': 'T_c m ': s)
    (('T_option ('T_pair ('T_c (EDivOpRes n m)) ('T_c (EModOpRes n m)))) ': s)
  ABS :: UnaryArithOp Abs n => Instr op cp ('T_c n ': s) ('T_c (UnaryArithResT Abs n) ': s)
  NEG :: UnaryArithOp Neg n => Instr op cp ('T_c n ': s) ('T_c (UnaryArithResT Neg n) ': s)
  MOD :: ModOp op cp n m => Instr op cp ('T_c n ': 'T_c m ': s) ('T_option ('T_c (ModOpRes n m)) ': s)
  LSL :: ArithOp Lsl n m => Instr op cp ('T_c n ': 'T_c m ': s) ('T_c (ArithResT Lsl n m) ': s)
  LSR :: ArithOp Lsr n m => Instr op cp ('T_c n ': 'T_c m ': s) ('T_c (ArithResT Lsr n m) ': s)
  OR :: ArithOp Or n m => Instr op cp ('T_c n ': 'T_c m ': s) ('T_c (ArithResT Or n m) ': s)
  AND :: ArithOp And n m => Instr op cp ('T_c n ': 'T_c m ': s) ('T_c (ArithResT And n m) ': s)
  XOR :: ArithOp Xor n m => Instr op cp ('T_c n ': 'T_c m ': s) ('T_c (ArithResT Xor n m) ': s)
  NOT :: UnaryArithOp Not n => Instr op cp ('T_c n ': s) ('T_c (UnaryArithResT Not n) ': s)
  COMPARE :: ArithOp Compare n m => Instr op cp ('T_c n ': 'T_c m ': s) ('T_c (ArithResT Compare n m) ': s)
  EQ :: UnaryArithOp Eq' n => Instr op cp ('T_c n ': s) ('T_c (UnaryArithResT Eq' n) ': s)
  NEQ :: UnaryArithOp Neq n => Instr op cp ('T_c n ': s) ('T_c (UnaryArithResT Neq n) ': s)
  LT :: UnaryArithOp Lt n => Instr op cp ('T_c n ': s) ('T_c (UnaryArithResT Lt n) ': s)
  GT :: UnaryArithOp Gt n => Instr op cp ('T_c n ': s) ('T_c (UnaryArithResT Gt n) ': s)
  LE :: UnaryArithOp Le n => Instr op cp ('T_c n ': s) ('T_c (UnaryArithResT Le n) ': s)
  GE :: UnaryArithOp Ge n => Instr op cp ('T_c n ': s) ('T_c (UnaryArithResT Ge n) ': s)
  -- INT               VarNote
  SELF :: Instr op cp s ('T_contract cp ': s)
  -- TODO this 'g' shall not be a free variable, but instead shall be bound by Instr type signature
  CONTRACT
    :: Instr op cp ('T_c 'T_address ': s) ('T_option ('T_contract p) ': s)
  TRANSFER_TOKENS
    :: Instr op cp (p ': 'T_c 'T_mutez ': 'T_contract p ': s)
                   ('T_operation ': s)
  SET_DELEGATE
    :: Instr op cp ('T_option ('T_c 'T_key_hash) ': s) ('T_operation ': s)

-- :: key_hash : option key_hash : bool : mutez : 'S
--    ->   operation : address : 'S
--
  CREATE_ACCOUNT
    :: Instr op cp
        ('T_c 'T_key_hash ': 'T_option ('T_c 'T_key_hash) ': 'T_c 'T_bool
         ': 'T_c 'T_mutez ': s) ('T_operation ': 'T_c 'T_address ': s)

  CREATE_CONTRACT
    :: Instr op cp
        ('T_c 'T_key_hash ': 'T_option ('T_c 'T_key_hash) ': 'T_c 'T_bool
          ': 'T_c 'T_bool ': 'T_c 'T_mutez
          ': 'T_lambda ('T_pair p g)
                       ('T_pair ('T_list 'T_operation) g) ': g ': s)
        ('T_operation ': 'T_c 'T_address ': s)
  CREATE_CONTRACT2
    :: Instr op p '[ 'T_pair p g ] '[ 'T_pair ('T_list 'T_operation) g ]
    -> Instr op cp ('T_c 'T_key_hash ': 'T_option ('T_c 'T_key_hash)
                    ': 'T_c 'T_bool ': 'T_c 'T_bool ': 'T_c 'T_mutez ': g ': s)
                   ('T_operation ': 'T_c 'T_address ': s)
  IMPLICIT_ACCOUNT
    :: Instr op cp ('T_c 'T_key_hash ': s) ('T_contract 'T_unit ': s)
  NOW :: Instr op cp s ('T_c 'T_timestamp ': s)
  AMOUNT :: Instr op cp s ('T_c 'T_mutez ': s)
  BALANCE :: Instr op cp s ('T_c 'T_mutez ': s)
  CHECK_SIGNATURE
    :: Instr op cp ('T_key ': 'T_signature ': 'T_c 'T_bytes ': s)
                   ('T_c 'T_bool ': s)
  SHA256 :: Instr op cp ('T_c 'T_bytes ': s) ('T_c 'T_bytes ': s)
  SHA512 :: Instr op cp ('T_c 'T_bytes ': s) ('T_c 'T_bytes ': s)
  BLAKE2B :: Instr op cp ('T_c 'T_bytes ': s) ('T_c 'T_bytes ': s)
  HASH_KEY :: Instr op cp ('T_key ': s) ('T_c 'T_key_hash ': s)
  STEPS_TO_QUOTA :: Instr op cp s ('T_c 'T_nat ': s)
  SOURCE :: Instr op cp s ('T_c 'T_address ': s)
  SENDER :: Instr op cp s ('T_c 'T_address ': s)
  ADDRESS :: Instr op cp ('T_contract a ': s) ('T_c 'T_address ': s)

deriving instance Show op => Show (Instr op cp inp out)

---------------------------------------------------------------------
-- Helper type classes for instructions with polymorphic stack type
---------------------------------------------------------------------

class MemOp (c :: T) where
  type MemOpKey c :: CT
  evalMem :: CVal (MemOpKey c) -> Val op cp c -> Bool
instance MemOp ('T_set e) where
  type MemOpKey ('T_set e) = e
  evalMem e (VSet s) = e `S.member` s
instance MemOp ('T_map k v) where
  type MemOpKey ('T_map k v) = k
  evalMem k (VMap m) = k `M.member` m
instance MemOp ('T_big_map k v) where
  type MemOpKey ('T_big_map k v) = k
  evalMem k (VBigMap m) = k `M.member` m

class MapOp (c :: T) where
  type MapOpInp c :: T
  type MapOpRes c :: T -> T
instance MapOp ('T_map k v) where
  type MapOpInp ('T_map k v) = 'T_pair ('T_c k) v
  type MapOpRes ('T_map k v) = 'T_map k
instance MapOp ('T_list e) where
  type MapOpInp ('T_list e) = e
  type MapOpRes ('T_list e) = 'T_list

class IterOp (c :: T) where
  type IterOpEl c :: T
instance IterOp ('T_map k v) where
  type IterOpEl ('T_map k v) = 'T_pair ('T_c k) v
instance IterOp ('T_list e) where
  type IterOpEl ('T_list e) = e
instance IterOp ('T_set e) where
  type IterOpEl ('T_set e) = 'T_c e

class SizeOp (c :: T)
instance SizeOp ('T_c 'T_string)
instance SizeOp ('T_c 'T_bytes)
instance SizeOp ('T_set a)
instance SizeOp ('T_list a)
instance SizeOp ('T_map k v)

class UpdOp (c :: T) where
  type UpdOpKey c :: CT
  type UpdOpParams c :: T
instance UpdOp ('T_map k v) where
  type UpdOpKey ('T_map k v) = k
  type UpdOpParams ('T_map k v) = 'T_option v
instance UpdOp ('T_big_map k v) where
  type UpdOpKey ('T_big_map k v) = k
  type UpdOpParams ('T_big_map k v) = 'T_option v
instance UpdOp ('T_set a) where
  type UpdOpKey ('T_set a) = a
  type UpdOpParams ('T_set a) = 'T_c 'T_bool

class GetOp (c :: T) where
  type GetOpKey c :: CT
  type GetOpVal c :: T
instance GetOp ('T_big_map k v) where
  type GetOpKey ('T_big_map k v) = k
  type GetOpVal ('T_big_map k v) = v
instance GetOp ('T_map k v) where
  type GetOpKey ('T_map k v) = k
  type GetOpVal ('T_map k v) = v

class ConcatOp (c :: T)
instance ConcatOp ('T_c 'T_string)
instance ConcatOp ('T_c 'T_bytes)

class SliceOp (c :: T)
instance SliceOp ('T_c 'T_string)
instance SliceOp ('T_c 'T_bytes)
