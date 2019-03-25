{-# LANGUAGE DataKinds, GADTs #-}

-- Module, containing functions to convert @Michelson.Types.Type@ to
-- @Michelson.Typed.T.T@ Michelson type representation (type stripped off all
-- annotations) and to @Michelson.Typed.Annotation.Notes@ value (which contains
-- field and type annotations for a given Michelson type).
--
-- I.e. @Michelson.Types.Type@ is split to value @t :: T@ and value of type
-- @Notes t@ for which @t@ is a type representation of value @t@.
module Michelson.Typed.Extract
  ( extractNotes
  , fromUType
  , mkUType
  , toUType
  ) where

import Michelson.Typed.Annotation (Notes(..), Notes'(..), mkNotes)
import Michelson.Typed.Sing (Sing(..), fromSingCT, fromSingT)
import Michelson.Typed.T (T(..))
import qualified Michelson.Untyped as Un

-- | Extracts 'T' type from 'Michelson.Untyped.Type'.
fromUType :: Un.Type -> T
fromUType (Un.Type wholeT _) = conv wholeT
  where
    conv (Un.Tc ct) = Tc ct
    conv Un.TKey = TKey
    conv Un.TUnit = TUnit
    conv Un.TSignature = TSignature
    conv (Un.TOption _ t) = TOption (fromUType t)
    conv (Un.TList t) = TList (fromUType t)
    conv (Un.TSet (Un.Comparable ct _)) = TSet ct
    conv Un.TOperation = TOperation
    conv (Un.TContract t) = TContract (fromUType t)
    conv (Un.TPair _ _ lT rT) = TPair (fromUType lT) (fromUType rT)
    conv (Un.TOr _ _ lT rT) = TOr (fromUType lT) (fromUType rT)
    conv (Un.TLambda lT rT) = TLambda (fromUType lT) (fromUType rT)
    conv (Un.TMap (Un.Comparable key _) val) = TMap key (fromUType val)
    conv (Un.TBigMap (Un.Comparable key _) val) = TBigMap key (fromUType val)


mkUType :: Sing x -> Notes x -> Un.Type
mkUType sing notes = case (sing, notes) of
  (STc ct, N (NTc tn))              -> mt (Un.Tc (fromSingCT ct)) tn
  (STc ct, NStar)                    -> mt (Un.Tc (fromSingCT ct)) na
  (STKey, N (NTKey tn))             -> mt Un.TKey tn
  (STKey, NStar)                     -> mt Un.TKey na
  (STUnit, N (NTUnit tn))           -> mt Un.TUnit tn
  (STUnit, NStar)                    -> mt Un.TUnit na
  (STSignature, N (NTSignature tn)) -> mt Un.TSignature tn
  (STSignature,NStar)                -> mt Un.TSignature na
  (STOption t,N (NTOption tn fn n)) -> mt (Un.TOption fn (mkUType t n)) tn
  ((STOption t), NStar)              -> mt (Un.TOption na (mkUType t NStar)) na
  (STList t, N (NTList tn n))       -> mt (Un.TList (mkUType t n)) tn
  (STList t, NStar)                  -> mt (Un.TList (mkUType t NStar)) na
  (STSet ct, N (NTSet tn n))        -> mt (Un.TSet $ mkComp ct n) tn
  (STSet ct, NStar)                  -> mt (Un.TSet $ mkComp ct na) na
  (STOperation, N (NTOperation tn)) -> mt Un.TOperation tn
  (STOperation, NStar)               -> mt Un.TOperation na
  (STContract t, N (NTContract tn n)) ->
    mt (Un.TContract (mkUType t n)) tn
  (STContract t, NStar)              -> mt (Un.TContract (mkUType t NStar)) na
  (STPair tl tr, N (NTPair tn fl fr nl nr)) ->
    mt (Un.TPair fl fr (mkUType tl nl) (mkUType tr nr)) tn
  (STPair tl tr, NStar) ->
    mt (Un.TPair na na (mkUType tl NStar) (mkUType tr NStar)) na
  (STOr tl tr, N (NTOr tn fl fr nl nr)) ->
    mt (Un.TOr fl fr (mkUType tl nl) (mkUType tr nr)) tn
  (STOr tl tr, NStar) ->
    mt (Un.TOr na na (mkUType tl NStar) (mkUType tr NStar)) na
  (STLambda p q, N (NTLambda tn np nq)) ->
    mt (Un.TLambda (mkUType p np) (mkUType q nq)) tn
  (STLambda p q, NStar) ->
    mt (Un.TLambda (mkUType p NStar) (mkUType q NStar)) na
  (STMap k v, N (NTMap tn nk nv)) ->
    mt (Un.TMap (mkComp k nk) (mkUType v nv)) tn
  (STMap k v, NStar) ->
    mt (Un.TMap (mkComp k na) (mkUType v NStar)) na
  (STBigMap k v, N (NTBigMap tn nk nv)) ->
    mt (Un.TBigMap (mkComp k nk) (mkUType v nv)) tn
  (STBigMap k v, NStar) ->
    mt (Un.TBigMap (mkComp k na) (mkUType v NStar)) na
 where
  mkComp t a = Un.Comparable (fromSingCT t) a
  mt = Un.Type
  na = Un.noAnn

-- | Extracts @Notes t@ type from 'Michelson.Type.Type' and corresponding
-- singleton.
extractNotes :: Un.Type -> Sing t -> Either Text (Notes t)
extractNotes (Un.Type wholeT tn) s = conv wholeT s
  where
    conv :: Un.T -> Sing t -> Either Text (Notes t)
    conv (Un.Tc ct) (STc cst)
      | fromSingCT cst == ct = pure $ mkNotes $ NTc tn
    conv Un.TKey STKey = pure $ mkNotes $ NTKey tn
    conv Un.TUnit STUnit = pure $ mkNotes $ NTUnit tn
    conv Un.TSignature STSignature = pure $ mkNotes $ NTSignature tn
    conv (Un.TOption fn t) (STOption st) =
      mkNotes . NTOption tn fn <$> extractNotes t st
    conv (Un.TList t) (STList st) = do
      mkNotes . NTList tn <$> extractNotes t st
    conv (Un.TSet (Un.Comparable et sn)) (STSet est)
      | fromSingCT est == et = pure $ mkNotes $ NTSet tn sn
    conv Un.TOperation STOperation = pure $ mkNotes $ NTOperation tn
    conv (Un.TContract t) (STContract st) =
      mkNotes . NTContract tn <$> extractNotes t st
    conv (Un.TPair pf qf pt qt) (STPair spt sqt) =
      liftA2 (mkNotes ... NTPair tn pf qf)
             (extractNotes pt spt)
             (extractNotes qt sqt)
    conv (Un.TOr pf qf pt qt) (STOr spt sqt) = do
      liftA2 (mkNotes ... NTOr tn pf qf)
             (extractNotes pt spt)
             (extractNotes qt sqt)
    conv (Un.TLambda pt qt) (STLambda spt sqt) = do
      liftA2 (mkNotes ... NTLambda tn)
             (extractNotes pt spt)
             (extractNotes qt sqt)
    conv (Un.TMap (Un.Comparable kt kn) vt) (STMap kst svt)
       | fromSingCT kst == kt = mkNotes . NTMap tn kn  <$> extractNotes vt svt
    conv (Un.TBigMap (Un.Comparable kt kn) vt) (STBigMap kst svt)
      | fromSingCT kst == kt =
        mkNotes . NTBigMap tn kn  <$> extractNotes vt svt
    conv a (fromSingT -> b) =
      Left $ "failed to construct annotation, provided types do not match: "
                <> show a <> " /= " <> show b

-- | Converts from 'T' to 'Michelson.Type.Type'.
toUType :: T -> Un.Type
toUType t = Un.Type (convert t) Un.noAnn
  where
    convert :: T -> Un.T
    convert (Tc a) = Un.Tc a
    convert (TKey) = Un.TKey
    convert (TUnit) = Un.TUnit
    convert (TSignature) = Un.TSignature
    convert (TOption a) = Un.TOption Un.noAnn (toUType a)
    convert (TList a) = Un.TList (toUType a)
    convert (TSet a) = Un.TSet $ Un.Comparable a Un.noAnn
    convert (TOperation) = Un.TOperation
    convert (TContract a) = Un.TContract (toUType a)
    convert (TPair a b) =
      Un.TPair Un.noAnn Un.noAnn (toUType a) (toUType b)
    convert (TOr a b) =
      Un.TOr Un.noAnn Un.noAnn (toUType a) (toUType b)
    convert (TLambda a b) =
      Un.TLambda (toUType a) (toUType b)
    convert (TMap a b) =
      Un.TMap (Un.Comparable a Un.noAnn) (toUType b)
    convert (TBigMap a b) =
      Un.TBigMap (Un.Comparable a Un.noAnn) (toUType b)
