-- | Module, containing type classes for operating with Michelson values
-- in the context of polymorphic stack type operations.

module Michelson.Typed.Polymorphic
  ( EDivOp (..)
  , MemOp (..)
  , MapOp (..)
  , IterOp (..)
  , SizeOp (..)
  , GetOp (..)
  , UpdOp (..)
  , SliceOp (..)
  , ConcatOp (..)
  ) where

import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Michelson.Typed.CValue (CVal(..))
import Michelson.Typed.T (CT(..), T(..))
import Michelson.Typed.Value (Val(..))

import Tezos.Core (divModMutez, divModMutezInt)

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

class MapOp (c :: T) (b :: T) where
  type MapOpInp c :: T
  type MapOpRes c b :: T
  mapOpToList :: Val instr c -> [Val instr (MapOpInp c)]
  mapOpFromList :: Val instr c -> [Val instr b] -> Val instr (MapOpRes c b)
instance MapOp ('T_map k v) v' where
  type MapOpInp ('T_map k v) = 'T_pair ('T_c k) v
  type MapOpRes ('T_map k v) v' = 'T_map k v'
  mapOpToList (VMap m) = map (\(k, v) -> VPair (VC k, v)) $ M.toAscList m
  mapOpFromList (VMap m) l =
    VMap $ M.fromList $ zip (map fst $ M.toAscList m) l
instance MapOp ('T_list e) e' where
  type MapOpInp ('T_list e) = e
  type MapOpRes ('T_list e) e' = 'T_list e'
  mapOpToList (VList l) = l
  mapOpFromList (VList _) l' = VList l'

class IterOp (c :: T) where
  type IterOpEl c :: T
instance IterOp ('T_map k v) where
  type IterOpEl ('T_map k v) = 'T_pair ('T_c k) v
instance IterOp ('T_list e) where
  type IterOpEl ('T_list e) = e
instance IterOp ('T_set e) where
  type IterOpEl ('T_set e) = 'T_c e

class SizeOp (c :: T) where
  evalSize :: Val cp c -> Int
instance SizeOp ('T_c 'T_string) where
  evalSize (VC (CvString s)) = length s
instance SizeOp ('T_c 'T_bytes) where
  evalSize (VC (CvBytes b)) = length b
instance SizeOp ('T_set a) where
  evalSize (VSet s) = S.size s
instance SizeOp ('T_list a) where
  evalSize (VList l) = length l
instance SizeOp ('T_map k v) where
  evalSize (VMap m) = M.size m

class UpdOp (c :: T) where
  type UpdOpKey c :: CT
  type UpdOpParams c :: T
  evalUpd 
    :: CVal (UpdOpKey c)
    -> Val cp (UpdOpParams c) -> Val cp c -> Val cp c
instance UpdOp ('T_map k v) where
  type UpdOpKey ('T_map k v) = k
  type UpdOpParams ('T_map k v) = 'T_option v
  evalUpd k (VOption o) (VMap m) =
    case o of
      Just newV -> VMap $ M.insert k newV m
      Nothing -> VMap $ M.delete k m
instance UpdOp ('T_big_map k v) where
  type UpdOpKey ('T_big_map k v) = k
  type UpdOpParams ('T_big_map k v) = 'T_option v
  evalUpd k (VOption o) (VBigMap m) =
    case o of
      Just newV -> VBigMap $ M.insert k newV m
      Nothing -> VBigMap $ M.delete k m
instance UpdOp ('T_set a) where
  type UpdOpKey ('T_set a) = a
  type UpdOpParams ('T_set a) = 'T_c 'T_bool
  evalUpd k (VC (CvBool b)) (VSet s) =
    case b of
      True -> VSet $ S.insert k s
      False -> VSet $ S.delete k s

class GetOp (c :: T) where
  type GetOpKey c :: CT
  type GetOpVal c :: T
  evalGet :: CVal (GetOpKey c) -> Val cp c -> Maybe (Val cp (GetOpVal c))
instance GetOp ('T_big_map k v) where
  type GetOpKey ('T_big_map k v) = k
  type GetOpVal ('T_big_map k v) = v
  evalGet k (VBigMap m) = k `M.lookup` m
instance GetOp ('T_map k v) where
  type GetOpKey ('T_map k v) = k
  type GetOpVal ('T_map k v) = v
  evalGet k (VMap m) = k `M.lookup` m

class ConcatOp (c :: T) where
  evalConcat :: Val cp c -> Val cp c -> Val cp c
  evalConcat' :: [Val cp c] -> Val cp c
instance ConcatOp ('T_c 'T_string) where
  evalConcat (VC (CvString s1)) (VC (CvString s2)) = (VC . CvString) (s1 <> s2)
  evalConcat' l =
    (VC . CvString . fromString) $ concat $ (map (\(VC (CvString s)) -> toString s)) l
instance ConcatOp ('T_c 'T_bytes) where
  evalConcat (VC (CvBytes b1)) (VC (CvBytes b2)) = (VC . CvBytes) (b1 <> b2)
  evalConcat' l =
    (VC . CvBytes) $ foldr (<>) mempty (map (\(VC (CvBytes b)) -> b) l)
instance ConcatOp ('T_list t) where
  evalConcat (VList l1) (VList l2) = VList $ l1 <> l2
  evalConcat' l =
    VList $ concat $ map (\(VList l') -> l') l

class SliceOp (c :: T) where
  evalSlice :: Natural -> Natural -> Val cp c -> Maybe (Val cp c)
instance SliceOp ('T_c 'T_string) where
  evalSlice o l (VC (CvString s)) =
    if o > fromIntegral (length s) || o + l > fromIntegral (length s)
    then Nothing
    else (Just . VC . CvString . toText) $ sliceText o l s
    where
      sliceText :: Natural -> Natural -> Text -> Text
      sliceText o' l' s' =
        T.drop ((fromIntegral . toInteger) o') $
          T.take ((fromIntegral . toInteger) l') s'
instance SliceOp ('T_c 'T_bytes) where
  evalSlice o l (VC (CvBytes b)) =
    if o > fromIntegral (length b) || o + l > fromIntegral (length b)
    then Nothing
    else (Just . VC . CvBytes) $ sliceBytes o l b
    where
      sliceBytes :: Natural -> Natural -> ByteString -> ByteString
      sliceBytes o' l' b' =
        B.drop ((fromIntegral . toInteger) o') $
          B.take ((fromIntegral . toInteger) l') b'

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
  type EDivOpRes 'T_mutez 'T_mutez = 'T_nat
  type EModOpRes 'T_mutez 'T_mutez = 'T_mutez
  evalEDivOp (CvMutez i) (CvMutez j) =
    VOption $
    i `divModMutez` j <&> \case
      (quotient, remainder) ->
        VPair (VC $ CvNat (fromIntegral quotient), VC $ CvMutez remainder)

instance EDivOp 'T_mutez 'T_nat where
  type EDivOpRes 'T_mutez 'T_nat = 'T_mutez
  type EModOpRes 'T_mutez 'T_nat = 'T_mutez
  evalEDivOp (CvMutez i) (CvNat j) =
    VOption $
    i `divModMutezInt` j <&> \case
      (quotient, remainder) ->
        VPair (VC $ CvMutez quotient, VC $ CvMutez remainder)
