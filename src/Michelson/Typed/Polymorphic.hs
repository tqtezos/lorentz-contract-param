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
instance MemOp ('TSet e) where
  type MemOpKey ('TSet e) = e
  evalMem e (VSet s) = e `S.member` s
instance MemOp ('TMap k v) where
  type MemOpKey ('TMap k v) = k
  evalMem k (VMap m) = k `M.member` m
instance MemOp ('TBigMap k v) where
  type MemOpKey ('TBigMap k v) = k
  evalMem k (VBigMap m) = k `M.member` m

class MapOp (c :: T) (b :: T) where
  type MapOpInp c :: T
  type MapOpRes c b :: T
  mapOpToList :: Val instr c -> [Val instr (MapOpInp c)]
  mapOpFromList :: Val instr c -> [Val instr b] -> Val instr (MapOpRes c b)
instance MapOp ('TMap k v) v' where
  type MapOpInp ('TMap k v) = 'TPair ('Tc k) v
  type MapOpRes ('TMap k v) v' = 'TMap k v'
  mapOpToList (VMap m) = map (\(k, v) -> VPair (VC k, v)) $ M.toAscList m
  mapOpFromList (VMap m) l =
    VMap $ M.fromList $ zip (map fst $ M.toAscList m) l
instance MapOp ('TList e) e' where
  type MapOpInp ('TList e) = e
  type MapOpRes ('TList e) e' = 'TList e'
  mapOpToList (VList l) = l
  mapOpFromList (VList _) l' = VList l'

class IterOp (c :: T) where
  type IterOpEl c :: T
  iterOpDetachOne ::
    Val instr c -> (Maybe (Val instr (IterOpEl c)), Val instr c)
instance IterOp ('TMap k v) where
  type IterOpEl ('TMap k v) = 'TPair ('Tc k) v
  iterOpDetachOne (VMap m) =
    ((VPair . (\(k, v) -> (VC k, v))) <$> M.lookupMin m, VMap $ M.deleteMin m)
instance IterOp ('TList e) where
  type IterOpEl ('TList e) = e
  iterOpDetachOne (VList l) =
    case l of
      x : xs -> (Just x, VList xs)
      [] -> (Nothing, VList [])
instance IterOp ('TSet e) where
  type IterOpEl ('TSet e) = 'Tc e
  iterOpDetachOne (VSet s) = (VC <$> S.lookupMin s, VSet $ S.deleteMin s)

class SizeOp (c :: T) where
  evalSize :: Val cp c -> Int
instance SizeOp ('Tc 'CString) where
  evalSize (VC (CvString s)) = length s
instance SizeOp ('Tc 'CBytes) where
  evalSize (VC (CvBytes b)) = length b
instance SizeOp ('TSet a) where
  evalSize (VSet s) = S.size s
instance SizeOp ('TList a) where
  evalSize (VList l) = length l
instance SizeOp ('TMap k v) where
  evalSize (VMap m) = M.size m

class UpdOp (c :: T) where
  type UpdOpKey c :: CT
  type UpdOpParams c :: T
  evalUpd
    :: CVal (UpdOpKey c)
    -> Val cp (UpdOpParams c) -> Val cp c -> Val cp c
instance UpdOp ('TMap k v) where
  type UpdOpKey ('TMap k v) = k
  type UpdOpParams ('TMap k v) = 'TOption v
  evalUpd k (VOption o) (VMap m) =
    case o of
      Just newV -> VMap $ M.insert k newV m
      Nothing -> VMap $ M.delete k m
instance UpdOp ('TBigMap k v) where
  type UpdOpKey ('TBigMap k v) = k
  type UpdOpParams ('TBigMap k v) = 'TOption v
  evalUpd k (VOption o) (VBigMap m) =
    case o of
      Just newV -> VBigMap $ M.insert k newV m
      Nothing -> VBigMap $ M.delete k m
instance UpdOp ('TSet a) where
  type UpdOpKey ('TSet a) = a
  type UpdOpParams ('TSet a) = 'Tc 'CBool
  evalUpd k (VC (CvBool b)) (VSet s) =
    case b of
      True -> VSet $ S.insert k s
      False -> VSet $ S.delete k s

class GetOp (c :: T) where
  type GetOpKey c :: CT
  type GetOpVal c :: T
  evalGet :: CVal (GetOpKey c) -> Val cp c -> Maybe (Val cp (GetOpVal c))
instance GetOp ('TBigMap k v) where
  type GetOpKey ('TBigMap k v) = k
  type GetOpVal ('TBigMap k v) = v
  evalGet k (VBigMap m) = k `M.lookup` m
instance GetOp ('TMap k v) where
  type GetOpKey ('TMap k v) = k
  type GetOpVal ('TMap k v) = v
  evalGet k (VMap m) = k `M.lookup` m

class ConcatOp (c :: T) where
  evalConcat :: Val cp c -> Val cp c -> Val cp c
  evalConcat' :: [Val cp c] -> Val cp c
instance ConcatOp ('Tc 'CString) where
  evalConcat (VC (CvString s1)) (VC (CvString s2)) = (VC . CvString) (s1 <> s2)
  evalConcat' l =
    (VC . CvString . fromString) $ concat $ (map (\(VC (CvString s)) -> toString s)) l
instance ConcatOp ('Tc 'CBytes) where
  evalConcat (VC (CvBytes b1)) (VC (CvBytes b2)) = (VC . CvBytes) (b1 <> b2)
  evalConcat' l =
    (VC . CvBytes) $ foldr (<>) mempty (map (\(VC (CvBytes b)) -> b) l)
instance ConcatOp ('TList t) where
  evalConcat (VList l1) (VList l2) = VList $ l1 <> l2
  evalConcat' l =
    VList $ concat $ map (\(VList l') -> l') l

class SliceOp (c :: T) where
  evalSlice :: Natural -> Natural -> Val cp c -> Maybe (Val cp c)
instance SliceOp ('Tc 'CString) where
  evalSlice o l (VC (CvString s)) =
    if o > fromIntegral (length s) || o + l > fromIntegral (length s)
    then Nothing
    else (Just . VC . CvString . toText) $ sliceText o l s
    where
      sliceText :: Natural -> Natural -> Text -> Text
      sliceText o' l' s' =
        T.drop ((fromIntegral . toInteger) o') $
          T.take ((fromIntegral . toInteger) l') s'
instance SliceOp ('Tc 'CBytes) where
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
    -> Val instr ('TOption ('TPair ('Tc (EDivOpRes n m))
                                     ('Tc (EModOpRes n m))))

instance EDivOp 'CInt 'CInt where
  type EDivOpRes 'CInt 'CInt = 'CInt
  type EModOpRes 'CInt 'CInt = 'CNat
  evalEDivOp (CvInt i) (CvInt j) =
    if j == 0
      then VOption $ Nothing
      else VOption $ Just $
        VPair (VC $ CvInt (div i j), VC $ CvNat $ fromInteger (mod i j))
instance EDivOp 'CInt 'CNat where
  type EDivOpRes 'CInt 'CNat = 'CInt
  type EModOpRes 'CInt 'CNat = 'CNat
  evalEDivOp (CvInt i) (CvNat j) =
    if j == 0
      then VOption $ Nothing
      else VOption $ Just $
        VPair (VC $ CvInt (div i (toInteger j)), VC $ CvNat $ (mod (fromInteger i) j))
instance EDivOp 'CNat 'CInt where
  type EDivOpRes 'CNat 'CInt = 'CInt
  type EModOpRes 'CNat 'CInt = 'CNat
  evalEDivOp (CvNat i) (CvInt j) =
    if j == 0
      then VOption $ Nothing
      else VOption $ Just $
        VPair (VC $ CvInt (div (toInteger i) j), VC $ CvNat $ (mod i (fromInteger j)))
instance EDivOp 'CNat 'CNat where
  type EDivOpRes 'CNat 'CNat = 'CNat
  type EModOpRes 'CNat 'CNat = 'CNat
  evalEDivOp (CvNat i) (CvNat j) =
    if j == 0
      then VOption $ Nothing
      else VOption $ Just $
        VPair (VC $ CvNat (div i j), VC $ CvNat $ (mod i j))
instance EDivOp 'CMutez 'CMutez where
  type EDivOpRes 'CMutez 'CMutez = 'CNat
  type EModOpRes 'CMutez 'CMutez = 'CMutez
  evalEDivOp (CvMutez i) (CvMutez j) =
    VOption $
    i `divModMutez` j <&> \case
      (quotient, remainder) ->
        VPair (VC $ CvNat (fromIntegral quotient), VC $ CvMutez remainder)

instance EDivOp 'CMutez 'CNat where
  type EDivOpRes 'CMutez 'CNat = 'CMutez
  type EModOpRes 'CMutez 'CNat = 'CMutez
  evalEDivOp (CvMutez i) (CvNat j) =
    VOption $
    i `divModMutezInt` j <&> \case
      (quotient, remainder) ->
        VPair (VC $ CvMutez quotient, VC $ CvMutez remainder)
