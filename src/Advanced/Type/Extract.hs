{-# LANGUAGE DataKinds, GADTs #-}

-- Module, containing functions to convert @Michelson.Type.Type@ to
-- @Advanced.Type.T.T@ Michelson type representation (type stripped off all
-- annotations) and to @Advanced.Type.Annotation.Notes@ value (which contains
-- field and type annotations for a given Michelson type).
--
-- I.e. @Michelson.Type.Type@ is split to value @t :: T@ and value of type
-- @Notes t@ for which @t@ is a type representation of value @t@.
module Advanced.Type.Extract
  (
    extractNotes
  , isStar
  , fromMType
  ) where

import qualified Michelson.Types as M

import Advanced.Type.Annotation (Notes(..), Notes'(..), mkNotes, isStar, mkNotes0)
import Advanced.Type.Sing (Sing(..), fromSingT)
import Advanced.Type.T (T(..))

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
    conv (M.T_comparable _) (ST_c _) = pure $ mkNotes0 NT_c tn
    conv M.T_key ST_key = pure $ mkNotes0 NT_key tn
    conv M.T_unit ST_unit = pure $ mkNotes0 NT_unit tn
    conv M.T_signature ST_signature = pure $ mkNotes0 NT_signature tn
    conv (M.T_option fn t) (ST_option st) =
      mkNotes . NT_option tn fn <$> extractNotes t st
    conv (M.T_list t) (ST_list st) = do
      mkNotes . NT_list tn <$> extractNotes t st
    conv (M.T_set (M.Comparable _ sn)) (ST_set _) =
      pure $ mkNotes $ NT_set tn sn
    conv M.T_operation ST_operation = pure $ mkNotes0 NT_operation tn
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
    conv (M.T_map (M.Comparable _ kn) vt) (ST_map _ svt) = do
      mkNotes . NT_map tn kn  <$> extractNotes vt svt
    conv (M.T_big_map (M.Comparable _ kn) vt) (ST_big_map _ svt) = do
      mkNotes . NT_big_map tn kn  <$> extractNotes vt svt
    conv a (fromSingT -> b) =
      Left $ "Error pure . mkNotes0truction annotations, provided types not match: "
                <> show a <> " /= " <> show b
