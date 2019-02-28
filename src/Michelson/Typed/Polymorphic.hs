-- | Module, containing type classes for operating with Michelson values
-- in the context of polymorphic stack type operations.

module Michelson.Typed.Polymorphic
  ( EDivOp (..)
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

import Michelson.Typed.T (CT(..), T(..))
import Michelson.Typed.CValue (CVal(..))
import Michelson.Typed.Value (Val(..))

class MemOp (c :: T) where
  type MemOpKey c :: CT
  evalMem :: CVal (MemOpKey c) -> Val cp c -> Bool
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
instance ConcatOp ('T_list t)

class SliceOp (c :: T)
instance SliceOp ('T_c 'T_string)
instance SliceOp ('T_c 'T_bytes)

class EDivOp (n :: CT) (m :: CT) where
  type EDivOpRes n m :: CT
  type EModOpRes n m :: CT
  evalEDivOp
    :: CVal n
    -> CVal m
    -> Val instr ('T_option ('T_pair ('T_c (EDivOpRes n m))
                                     ('T_c (EModOpRes n m))))

instance EDivOp 'T_int 'T_int where
  type EDivOpRes 'T_int 'T_int = 'T_int
  type EModOpRes 'T_int 'T_int = 'T_nat
  evalEDivOp (CvInt i) (CvInt j) =
    if j == 0
      then VOption $ Nothing
      else VOption $ Just $
        VPair (VC $ CvInt (div i j), VC $ CvNat $ fromInteger (mod i j))
instance EDivOp 'T_int 'T_nat where
  type EDivOpRes 'T_int 'T_nat = 'T_int
  type EModOpRes 'T_int 'T_nat = 'T_nat
  evalEDivOp (CvInt i) (CvNat j) =
    if j == 0
      then VOption $ Nothing
      else VOption $ Just $
        VPair (VC $ CvInt (div i (toInteger j)), VC $ CvNat $ (mod (fromInteger i) j))
instance EDivOp 'T_nat 'T_int where
  type EDivOpRes 'T_nat 'T_int = 'T_int
  type EModOpRes 'T_nat 'T_int = 'T_nat
  evalEDivOp (CvNat i) (CvInt j) =
    if j == 0
      then VOption $ Nothing
      else VOption $ Just $
        VPair (VC $ CvInt (div (toInteger i) j), VC $ CvNat $ (mod i (fromInteger j)))
instance EDivOp 'T_nat 'T_nat where
  type EDivOpRes 'T_nat 'T_nat = 'T_nat
  type EModOpRes 'T_nat 'T_nat = 'T_nat
  evalEDivOp (CvNat i) (CvNat j) =
    if j == 0
      then VOption $ Nothing
      else VOption $ Just $
        VPair (VC $ CvNat (div i j), VC $ CvNat $ (mod i j))
instance EDivOp 'T_mutez 'T_mutez where
  type EDivOpRes 'T_mutez 'T_mutez = 'T_mutez
  type EModOpRes 'T_mutez 'T_mutez = 'T_mutez
  evalEDivOp (CvMutez i) (CvMutez j) =
    if j == 0
      then VOption $ Nothing
      else VOption $ Just $
        VPair (VC $ CvMutez (div i j), VC $ CvMutez $ (mod i j))
instance EDivOp 'T_mutez 'T_nat where
  type EDivOpRes 'T_mutez 'T_nat = 'T_nat
  type EModOpRes 'T_mutez 'T_nat = 'T_mutez
  evalEDivOp (CvMutez i) (CvNat j) =
    if j == 0
      then VOption $ Nothing
      else VOption $ Just $
        VPair (VC $ CvNat (div (fromIntegral i) j), VC $ CvMutez $ (mod i (fromIntegral j)))
