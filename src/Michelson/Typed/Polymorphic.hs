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

import Michelson.Typed.CValue (CValue(..))
import Michelson.Typed.T (CT(..), T(..))
import Michelson.Typed.Value (Value'(..))

import Tezos.Core (divModMutez, divModMutezInt)

class MemOp (c :: T) where
  type MemOpKey c :: CT
  evalMem :: CValue (MemOpKey c) -> Value' cp c -> Bool
instance MemOp ('TSet e) where
  type MemOpKey ('TSet e) = e
  evalMem e (VSet s) = e `S.member` s
instance MemOp ('TMap k v) where
  type MemOpKey ('TMap k v) = k
  evalMem k (VMap m) = k `M.member` m
instance MemOp ('TBigMap k v) where
  type MemOpKey ('TBigMap k v) = k
  evalMem k (VBigMap m) = k `M.member` m

class MapOp (c :: T) where
  type MapOpInp c :: T
  type MapOpRes c :: T -> T
  mapOpToList :: Value' instr c -> [Value' instr (MapOpInp c)]
  mapOpFromList :: Value' instr c -> [Value' instr b] -> Value' instr (MapOpRes c b)
instance MapOp ('TMap k v) where
  type MapOpInp ('TMap k v) = 'TPair ('Tc k) v
  type MapOpRes ('TMap k v) = 'TMap k
  mapOpToList (VMap m) = map (\(k, v) -> VPair (VC k, v)) $ M.toAscList m
  mapOpFromList (VMap m) l =
    VMap $ M.fromList $ zip (map fst $ M.toAscList m) l
instance MapOp ('TList e) where
  type MapOpInp ('TList e) = e
  type MapOpRes ('TList e) = 'TList
  mapOpToList (VList l) = l
  mapOpFromList (VList _) l' = VList l'
-- If you find it difficult to implement 'MapOp' for your datatype
-- because of order of type arguments in it, consider wrapping it
-- into a newtype.

class IterOp (c :: T) where
  type IterOpEl c :: T
  iterOpDetachOne ::
    Value' instr c -> (Maybe (Value' instr (IterOpEl c)), Value' instr c)
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
  evalSize :: Value' cp c -> Int
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
    :: CValue (UpdOpKey c)
    -> Value' cp (UpdOpParams c) -> Value' cp c -> Value' cp c
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
  evalGet :: CValue (GetOpKey c) -> Value' cp c -> Maybe (Value' cp (GetOpVal c))
instance GetOp ('TBigMap k v) where
  type GetOpKey ('TBigMap k v) = k
  type GetOpVal ('TBigMap k v) = v
  evalGet k (VBigMap m) = k `M.lookup` m
instance GetOp ('TMap k v) where
  type GetOpKey ('TMap k v) = k
  type GetOpVal ('TMap k v) = v
  evalGet k (VMap m) = k `M.lookup` m

class ConcatOp (c :: T) where
  evalConcat :: Value' cp c -> Value' cp c -> Value' cp c
  evalConcat' :: [Value' cp c] -> Value' cp c
instance ConcatOp ('Tc 'CString) where
  evalConcat (VC (CvString s1)) (VC (CvString s2)) = (VC . CvString) (s1 <> s2)
  evalConcat' l =
    (VC . CvString . fromString) $ concatMap (\(VC (CvString s)) -> toString s) l
instance ConcatOp ('Tc 'CBytes) where
  evalConcat (VC (CvBytes b1)) (VC (CvBytes b2)) = (VC . CvBytes) (b1 <> b2)
  evalConcat' l =
    (VC . CvBytes) $ foldr ((<>) . (\(VC (CvBytes b)) -> b)) mempty l

class SliceOp (c :: T) where
  evalSlice :: Natural -> Natural -> Value' cp c -> Maybe (Value' cp c)
instance SliceOp ('Tc 'CString) where
  evalSlice o l (VC (CvString s)) = VC . CvString <$> sliceImpl T.drop T.take o l s
instance SliceOp ('Tc 'CBytes) where
  evalSlice o l (VC (CvBytes b)) = VC . CvBytes <$> sliceImpl B.drop B.take o l b

sliceImpl ::
  Container str
  => (Int -> str -> str)
  -> (Int -> str -> str)
  -> Natural
  -> Natural
  -> str
  -> Maybe str
sliceImpl dropF takeF offset l s
  | offset + l > fromIntegral (length s) = Nothing
  | otherwise
  -- Drop offset and then take requested number of items.
   = Just . takeF (fromIntegral l) . dropF (fromIntegral offset) $ s

class EDivOp (n :: CT) (m :: CT) where
  type EDivOpRes n m :: CT
  type EModOpRes n m :: CT
  evalEDivOp
    :: CValue n
    -> CValue m
    -> Value' instr ('TOption ('TPair ('Tc (EDivOpRes n m))
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
