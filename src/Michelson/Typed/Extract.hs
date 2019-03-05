{-# LANGUAGE DataKinds, GADTs #-}

-- Module, containing functions to convert @Michelson.Types.Type@ to
-- @Michelson.Typed.T.T@ Michelson type representation (type stripped off all
-- annotations) and to @Michelson.Typed.Annotation.Notes@ value (which contains
-- field and type annotations for a given Michelson type).
--
-- I.e. @Michelson.Types.Type@ is split to value @t :: T@ and value of type
-- @Notes t@ for which @t@ is a type representation of value @t@.
module Michelson.Typed.Extract
  (
    extractNotes
  , fromMType
  , toMType
  ) where

import qualified Michelson.Untyped as M

import Michelson.Typed.Annotation (Notes(..), Notes'(..), mkNotes)
import Michelson.Typed.Sing (Sing(..), fromSingCT, fromSingT)
import Michelson.Typed.T (T(..))

-- | Extracts 'T' type from 'Michelson.Type.Type'.
fromMType :: M.Type -> T
fromMType (M.Type wholeT _) = conv wholeT
  where
    conv (M.T_comparable ct) = T_c ct
    conv M.T_key = T_key
    conv M.T_unit = T_unit
    conv M.T_signature = T_signature
    conv (M.T_option _ t) = T_option (fromMType t)
    conv (M.T_list t) = T_list (fromMType t)
    conv (M.T_set (M.Comparable ct _)) = T_set ct
    conv M.T_operation = T_operation
    conv (M.T_contract t) = T_contract (fromMType t)
    conv (M.T_pair _ _ lT rT) = T_pair (fromMType lT) (fromMType rT)
    conv (M.T_or _ _ lT rT) = T_or (fromMType lT) (fromMType rT)
    conv (M.T_lambda lT rT) = T_lambda (fromMType lT) (fromMType rT)
    conv (M.T_map (M.Comparable key _) val) = T_map key (fromMType val)
    conv (M.T_big_map (M.Comparable key _) val) = T_big_map key (fromMType val)

-- | Extracts @Notes t@ type from 'Michelson.Type.Type' and corresponding
-- singleton.
extractNotes :: M.Type -> Sing t -> Either Text (Notes t)
extractNotes (M.Type wholeT tn) s = conv wholeT s
  where
    conv :: M.T -> Sing t -> Either Text (Notes t)
    conv (M.T_comparable ct) (ST_c cst)
      | fromSingCT cst == ct = pure $ mkNotes $ NT_c tn
    conv M.T_key ST_key = pure $ mkNotes $ NT_key tn
    conv M.T_unit ST_unit = pure $ mkNotes $ NT_unit tn
    conv M.T_signature ST_signature = pure $ mkNotes $ NT_signature tn
    conv (M.T_option fn t) (ST_option st) =
      mkNotes . NT_option tn fn <$> extractNotes t st
    conv (M.T_list t) (ST_list st) = do
      mkNotes . NT_list tn <$> extractNotes t st
    conv (M.T_set (M.Comparable et sn)) (ST_set est)
      | fromSingCT est == et = pure $ mkNotes $ NT_set tn sn
    conv M.T_operation ST_operation = pure $ mkNotes $ NT_operation tn
    conv (M.T_contract t) (ST_contract st) =
      mkNotes . NT_contract tn <$> extractNotes t st
    conv (M.T_pair pf qf pt qt) (ST_pair spt sqt) =
      liftA2 (mkNotes ... NT_pair tn pf qf)
             (extractNotes pt spt)
             (extractNotes qt sqt)
    conv (M.T_or pf qf pt qt) (ST_or spt sqt) = do
      liftA2 (mkNotes ... NT_or tn pf qf)
             (extractNotes pt spt)
             (extractNotes qt sqt)
    conv (M.T_lambda pt qt) (ST_lambda spt sqt) = do
      liftA2 (mkNotes ... NT_lambda tn)
             (extractNotes pt spt)
             (extractNotes qt sqt)
    conv (M.T_map (M.Comparable kt kn) vt) (ST_map kst svt)
       | fromSingCT kst == kt = mkNotes . NT_map tn kn  <$> extractNotes vt svt
    conv (M.T_big_map (M.Comparable kt kn) vt) (ST_big_map kst svt)
      | fromSingCT kst == kt =
        mkNotes . NT_big_map tn kn  <$> extractNotes vt svt
    conv a (fromSingT -> b) =
      Left $ "failed to construct annotation, provided types do not match: "
                <> show a <> " /= " <> show b

-- | Converts from 'T' to 'Michelson.Type.Type'.
toMType :: T -> M.Type
toMType t = M.Type (convert t) M.noAnn
  where
    convert :: T -> M.T
    convert (T_c a) = M.T_comparable a
    convert (T_key) = M.T_key
    convert (T_unit) = M.T_unit
    convert (T_signature) = M.T_signature
    convert (T_option a) = M.T_option  M.noAnn (toMType a)
    convert (T_list a) = M.T_list (toMType a)
    convert (T_set a) = M.T_set $ M.Comparable a M.noAnn
    convert (T_operation) = M.T_operation
    convert (T_contract a) = M.T_contract (toMType a)
    convert (T_pair a b) =
      M.T_pair M.noAnn M.noAnn (toMType a) (toMType b)
    convert (T_or a b) =
      M.T_or M.noAnn M.noAnn (toMType a) (toMType b)
    convert (T_lambda a b) =
      M.T_lambda (toMType a) (toMType b)
    convert (T_map a b) =
      M.T_map (M.Comparable a M.noAnn) (toMType b)
    convert (T_big_map a b) =
      M.T_big_map (M.Comparable a M.noAnn) (toMType b)
