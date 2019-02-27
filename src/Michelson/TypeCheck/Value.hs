module Michelson.TypeCheck.Value
    ( typeCheckValImpl
    , typeCheckCVal
    ) where

import Data.Default (def)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Singletons (SingI)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime (zonedTimeToUTC)
import Data.Time.RFC3339 (parseTimeRFC3339)
import Data.Typeable ((:~:)(..))
import Prelude hiding (EQ, GT, LT)

import Michelson.TypeCheck.Helpers
import Michelson.TypeCheck.Types
import Michelson.Typed
  (CT(..), Instr(..), Notes(..), Notes'(..), Sing(..), T(..), converge, mkNotes, withSomeSingCT,
  withSomeSingT)
import Michelson.Typed.Value (CVal(..), Val(..))

import qualified Michelson.Untyped as M
import Tezos.Crypto (Address(..), parseAddress, parseKeyHash, parsePublicKey, parseSignature)

typeCheckCVal :: M.Value M.Op -> CT -> Maybe SomeValC
typeCheckCVal (M.ValueInt i) T_int = pure $ CvInt i :--: ST_int
typeCheckCVal (M.ValueInt i) T_nat
  | i >= 0 = pure $ CvNat (fromInteger i) :--: ST_nat
typeCheckCVal (M.ValueInt (fromInteger -> i)) T_mutez
  | i <= maxBound && i >= minBound = pure $ CvMutez i :--: ST_mutez
typeCheckCVal (M.ValueString s) T_string =
  pure $ CvString s :--: ST_string

typeCheckCVal (M.ValueString (parseAddress -> Right s)) T_address =
  pure $ CvAddress s :--: ST_address
typeCheckCVal (M.ValueBytes (M.InternalByteString s)) T_address =
  pure $ CvAddress (Address s) :--: ST_address

typeCheckCVal (M.ValueString (parseKeyHash -> Right s)) T_key_hash =
  pure $ CvKeyHash s :--: ST_key_hash

typeCheckCVal (M.ValueString (parseTimeRFC3339 -> Just zt)) T_timestamp =
  pure $ CvTimestamp (zonedTimeToUTC zt) :--: ST_timestamp
typeCheckCVal (M.ValueInt i) T_timestamp = do
  let t = posixSecondsToUTCTime (fromInteger i)
  pure $ CvTimestamp t :--: ST_timestamp

typeCheckCVal (M.ValueBytes (M.InternalByteString s)) T_bytes =
  pure $ CvBytes s :--: ST_bytes
typeCheckCVal M.ValueTrue T_bool = pure $ CvBool True :--: ST_bool
typeCheckCVal M.ValueFalse T_bool = pure $ CvBool False :--: ST_bool
typeCheckCVal _ _ = Nothing

typeCheckCVals
  :: forall t . Typeable t
  => [M.Value M.Op] -> CT -> Either (M.Value M.Op, Text) [CVal t]
typeCheckCVals mvs t = traverse check mvs
  where
    check mv = do
      v :--: (_ :: Sing t') <-
        maybe (Left (mv, "failed to typecheck cval")) pure $ typeCheckCVal mv t
      Refl <- eqT' @t @t' `onLeft` (,) mv
      pure v

-- | Function @typeCheckValImpl@ converts a single Michelson value
-- given in representation from @Michelson.Type@ module to representation
-- in strictly typed GADT.
--
-- As a third argument, @typeCheckValImpl@ accepts expected type of value.
--
-- Type checking algorithm pattern-matches on parse value representation,
-- expected type @t@ and constructs @Val cp t@ value.
--
-- If there was no match on a given pair of value and expected type,
-- that is interpreted as input of wrong type and type check finishes with
-- error.
typeCheckValImpl
  :: forall cp.
    (Typeable cp, SingI cp)
  => ([M.Instr] -> SomeIT -> Either TCError (SomeInstr cp))
  -> M.Value M.Op -> T -> Either TCError (SomeVal cp)
typeCheckValImpl _ mv t@(T_c ct) =
  maybe (Left $ TCFailedOnValue mv t "")
        (\(v :--: cst) -> pure $ VC v :::: (ST_c cst, NStar))
        (typeCheckCVal mv ct)
typeCheckValImpl _ (M.ValueString (parsePublicKey -> Right s)) T_key =
  pure $ VKey s :::: (ST_key, NStar)

typeCheckValImpl _ (M.ValueString (parseSignature -> Right s)) T_signature =
  pure $ VSignature s :::: (ST_signature, NStar)

typeCheckValImpl _ (M.ValueString (parseAddress -> Right s)) (T_contract pt) =
  withSomeSingT pt $ \p ->
    pure $ VContract s :::: (ST_contract p, NStar)
typeCheckValImpl _ (M.ValueBytes (M.InternalByteString s)) (T_contract pt) = do
  withSomeSingT pt $ \p ->
    pure $ VContract (Address s) :::: (ST_contract p, NStar)
typeCheckValImpl _ M.ValueUnit T_unit = pure $ VUnit :::: (ST_unit, NStar)
typeCheckValImpl tcDo (M.ValuePair ml mr) (T_pair lt rt) = do
  l :::: (lst, ln) <- typeCheckValImpl tcDo ml lt
  r :::: (rst, rn) <- typeCheckValImpl tcDo mr rt
  let ns = mkNotes $ NT_pair def def def ln rn
  pure $ VPair (l, r) :::: (ST_pair lst rst, ns)
typeCheckValImpl tcDo (M.ValueLeft ml) (T_or lt rt) = do
  l :::: (lst, ln) <- typeCheckValImpl tcDo ml lt
  withSomeSingT rt $ \rst ->
    pure $ VOr (Left l) :::: ( ST_or lst rst
                             , mkNotes $ NT_or def def def ln NStar )
typeCheckValImpl tcDo (M.ValueRight mr) (T_or lt rt) = do
  r :::: (rst, rn) <- typeCheckValImpl tcDo mr rt
  withSomeSingT lt $ \lst ->
    pure $ VOr (Right r) :::: ( ST_or lst rst
                              , mkNotes $ NT_or def def def NStar rn )
typeCheckValImpl tcDo (M.ValueSome mv) (T_option vt) = do
  v :::: (vst, vns) <- typeCheckValImpl tcDo mv vt
  let ns = mkNotes $ NT_option def def vns
  pure $ VOption (Just v) :::: (ST_option vst, ns)
typeCheckValImpl _ M.ValueNone (T_option vt) =
  withSomeSingT vt $ \vst ->
    pure $ VOption Nothing :::: (ST_option vst, NStar)

typeCheckValImpl tcDo (M.ValueSeq mels) (T_list vt) =
  withSomeSingT vt $ \vst -> do
    (els, ns) <- typeCheckValsImpl tcDo mels vt
    pure $ VList els :::: (ST_list vst, mkNotes $ NT_list def ns)

typeCheckValImpl _ (M.ValueSeq mels) (T_set vt) =
  withSomeSingCT vt $ \vst -> do
    els <- typeCheckCVals mels vt
            `onLeft` \(cv, err) -> TCFailedOnValue cv (T_c vt) $
                                      "wrong type of set element: " <> err
    pure $ VSet (S.fromList els) :::: (ST_set vst, NStar)

typeCheckValImpl tcDo (M.ValueMap mels) (T_map kt vt) =
  withSomeSingT vt $ \vst ->
  withSomeSingCT kt $ \kst -> do
    ks <- typeCheckCVals (map (\(M.Elt k _) -> k) mels) kt
            `onLeft` \(cv, err) -> TCFailedOnValue cv (T_c kt) $
                                      "wrong type of map key: " <> err
    (vals, vns) <- typeCheckValsImpl tcDo (map (\(M.Elt _ v) -> v) mels) vt
    let ns = mkNotes $ NT_map def def vns
    pure $ VMap (M.fromList $ zip ks vals) :::: (ST_map kst vst, ns)

typeCheckValImpl tcDo v@(M.ValueLambda (fmap M.unOp -> mp)) t@(T_lambda mi mo) =
  withSomeSingT mi $ \(it :: Sing it) ->
  withSomeSingT mo $ \(ot :: Sing ot) ->
  tcDo mp (SomeIT $ (it, NStar, def) ::& INil) >>= \case
    SiFail ->
      pure $ VLam FAILWITH :::: (ST_lambda it ot, NStar)
    lam ::: ((li :: IT li), (lo :: IT lo)) -> do
      Refl <- eqT' @li @'[ it ] `onLeft` unexpectedErr
      case (eqT' @'[ ot ] @lo, SomeIT lo, SomeIT li) of
        ( Right Refl
         , SomeIT ((_, ons, _) ::& INil :: IT lo')
         , SomeIT ((_, ins, _) ::& INil :: IT li')
         ) -> do
            Refl <- eqT' @lo @lo' `onLeft` unexpectedErr
            Refl <- eqT' @li @li' `onLeft` unexpectedErr
            let ns = mkNotes $ NT_lambda def ins ons
            pure $ VLam lam :::: (ST_lambda it ot, ns)
        (Right _, _, _) ->
          Left $ TCFailedOnValue v t
                  "wrong output type of lambda's value (wrong stack size)"
        (Left m, _, _) ->
          Left $ TCFailedOnValue v t $
                  "wrong output type of lambda's value: " <> m
  where
    unexpectedErr m = TCFailedOnValue v t ("unexpected " <> m)

typeCheckValImpl _ v t = Left $ TCFailedOnValue v t ""

typeCheckValsImpl
  :: forall t cp.
    ( Typeable t
    , Typeable cp, SingI cp
    )
  => ([M.Instr] -> SomeIT -> Either TCError (SomeInstr cp))
  -> [M.Value M.Op] -> T -> Either TCError ([Val (Instr cp) t], Notes t)
typeCheckValsImpl tcDo mvs t = foldM check ([], NStar) mvs
  where
    check (res, ns) mv = do
      v :::: ((_ :: Sing t'), vns) <- typeCheckValImpl tcDo mv t
      Refl <- eqT' @t @t'
                `onLeft` (TCFailedOnValue mv t . ("wrong element type " <>))
      ns' <- converge ns vns `onLeft` TCFailedOnValue mv t
      pure (v : res, ns')
