module Michelson.TypeCheck.Value
    ( typeCheckValImpl
    , typeCheckCVal
    ) where

import Control.Monad.Except (liftEither, throwError)
import Data.Default (def)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Typeable ((:~:)(..))
import Prelude hiding (EQ, GT, LT)

import Michelson.TypeCheck.Helpers
import Michelson.TypeCheck.Types
import Michelson.Typed
  (CT(..), Instr(..), Notes(..), Notes'(..), Sing(..), T(..), converge, mkNotes, withSomeSingCT,
  withSomeSingT, InstrExtT)
import Michelson.Typed.Value (CVal(..), Val(..))
import qualified Michelson.Untyped as Un
import Tezos.Address (parseAddress)
import Tezos.Core (mkMutez, parseTimestamp, timestampFromSeconds)
import Tezos.Crypto (parseKeyHash, parsePublicKey, parseSignature)

typeCheckCVal :: Un.Value op -> CT -> Maybe SomeValC
typeCheckCVal (Un.ValueInt i) T_int = pure $ CvInt i :--: ST_int
typeCheckCVal (Un.ValueInt i) T_nat
  | i >= 0 = pure $ CvNat (fromInteger i) :--: ST_nat
typeCheckCVal (Un.ValueInt (mkMutez . fromInteger -> Just mtz)) T_mutez =
  pure $ CvMutez mtz :--: ST_mutez
typeCheckCVal (Un.ValueString s) T_string =
  pure $ CvString s :--: ST_string
typeCheckCVal (Un.ValueString (parseAddress -> Right s)) T_address =
  pure $ CvAddress s :--: ST_address
typeCheckCVal (Un.ValueString (parseKeyHash -> Right s)) T_key_hash =
  pure $ CvKeyHash s :--: ST_key_hash
typeCheckCVal (Un.ValueString (parseTimestamp -> Just t)) T_timestamp =
  pure $ CvTimestamp t :--: ST_timestamp
typeCheckCVal (Un.ValueInt i) T_timestamp =
  pure $ CvTimestamp (timestampFromSeconds i) :--: ST_timestamp
typeCheckCVal (Un.ValueBytes (Un.InternalByteString s)) T_bytes =
  pure $ CvBytes s :--: ST_bytes
typeCheckCVal Un.ValueTrue T_bool = pure $ CvBool True :--: ST_bool
typeCheckCVal Un.ValueFalse T_bool = pure $ CvBool False :--: ST_bool
typeCheckCVal _ _ = Nothing

typeCheckCVals
  :: forall t op . Typeable t
  => [Un.Value op]
  -> CT
  -> Either (Un.Value op, Text) [CVal t]
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
-- expected type @t@ and constructs @Val t@ value.
--
-- If there was no match on a given pair of value and expected type,
-- that is interpreted as input of wrong type and type check finishes with
-- error.
typeCheckValImpl
  :: Show InstrExtT
  => TcInstrHandler
  -> Un.Value Un.Op
  -> T
  -> TypeCheckT SomeVal
typeCheckValImpl _ mv t@(T_c ct) =
  maybe (throwError $ TCFailedOnValue mv t "")
        (\(v :--: cst) -> pure $ VC v :::: (ST_c cst, NStar))
        (typeCheckCVal mv ct)
typeCheckValImpl _ (Un.ValueString (parsePublicKey -> Right s)) T_key =
  pure $ VKey s :::: (ST_key, NStar)

typeCheckValImpl _ (Un.ValueString (parseSignature -> Right s)) T_signature =
  pure $ VSignature s :::: (ST_signature, NStar)

typeCheckValImpl _ (Un.ValueString (parseAddress -> Right s)) (T_contract pt) =
  withSomeSingT pt $ \p ->
    pure $ VContract s :::: (ST_contract p, NStar)
typeCheckValImpl _ Un.ValueUnit T_unit = pure $ VUnit :::: (ST_unit, NStar)
typeCheckValImpl tcDo (Un.ValuePair ml mr) (T_pair lt rt) = do
  l :::: (lst, ln) <- typeCheckValImpl tcDo ml lt
  r :::: (rst, rn) <- typeCheckValImpl tcDo mr rt
  let ns = mkNotes $ NT_pair def def def ln rn
  pure $ VPair (l, r) :::: (ST_pair lst rst, ns)
typeCheckValImpl tcDo (Un.ValueLeft ml) (T_or lt rt) = do
  l :::: (lst, ln) <- typeCheckValImpl tcDo ml lt
  withSomeSingT rt $ \rst ->
    pure $ VOr (Left l) :::: ( ST_or lst rst
                             , mkNotes $ NT_or def def def ln NStar )
typeCheckValImpl tcDo (Un.ValueRight mr) (T_or lt rt) = do
  r :::: (rst, rn) <- typeCheckValImpl tcDo mr rt
  withSomeSingT lt $ \lst ->
    pure $ VOr (Right r) :::: ( ST_or lst rst
                              , mkNotes $ NT_or def def def NStar rn )
typeCheckValImpl tcDo (Un.ValueSome mv) (T_option vt) = do
  v :::: (vst, vns) <- typeCheckValImpl tcDo mv vt
  let ns = mkNotes $ NT_option def def vns
  pure $ VOption (Just v) :::: (ST_option vst, ns)
typeCheckValImpl _ Un.ValueNone (T_option vt) =
  withSomeSingT vt $ \vst ->
    pure $ VOption Nothing :::: (ST_option vst, NStar)

typeCheckValImpl tcDo (Un.ValueSeq mels) (T_list vt) =
  withSomeSingT vt $ \vst -> do
    (els, ns) <- typeCheckValsImpl tcDo mels vt
    pure $ VList els :::: (ST_list vst, mkNotes $ NT_list def ns)

typeCheckValImpl _ (Un.ValueSeq mels) (T_set vt) =
  withSomeSingCT vt $ \vst -> do
    els <- liftEither $ typeCheckCVals mels vt
            `onLeft` \(cv, err) -> TCFailedOnValue cv (T_c vt) $
                                      "wrong type of set element: " <> err
    pure $ VSet (S.fromList els) :::: (ST_set vst, NStar)

typeCheckValImpl tcDo (Un.ValueMap mels) (T_map kt vt) =
  withSomeSingT vt $ \vst ->
  withSomeSingCT kt $ \kst -> do
    ks <- liftEither $  typeCheckCVals (map (\(Un.Elt k _) -> k) mels) kt
            `onLeft` \(cv, err) -> TCFailedOnValue cv (T_c kt) $
                                      "wrong type of map key: " <> err
    (vals, vns) <- typeCheckValsImpl tcDo (map (\(Un.Elt _ v) -> v) mels) vt
    let ns = mkNotes $ NT_map def def vns
    pure $ VMap (M.fromList $ zip ks vals) :::: (ST_map kst vst, ns)

typeCheckValImpl tcDo v@(Un.ValueLambda (fmap Un.unOp -> mp)) t@(T_lambda mi mo) =
  withSomeSingT mi $ \(it :: Sing it) ->
  withSomeSingT mo $ \(ot :: Sing ot) ->
    typeCheckImpl tcDo mp (SomeHST $ (it, NStar, def) ::& SNil) >>= \case
      SiFail -> pure $ VLam FAILWITH :::: (ST_lambda it ot, NStar)
      lam ::: ((li :: HST li), (lo :: HST lo)) -> do
        Refl <- liftEither $ eqT' @li @'[ it ] `onLeft` unexpectedErr
        case (eqT' @'[ ot ] @lo, SomeHST lo, SomeHST li) of
          (Right Refl,
           SomeHST ((_, ons, _) ::& SNil :: HST lo'),
           SomeHST ((_, ins, _) ::& SNil :: HST li')) -> do
            Refl <- liftEither $ eqT' @lo @lo' `onLeft` unexpectedErr
            Refl <- liftEither $ eqT' @li @li' `onLeft` unexpectedErr
            let ns = mkNotes $ NT_lambda def ins ons
            pure $ VLam lam :::: (ST_lambda it ot, ns)
          (Right _, _, _) ->
            throwError $ TCFailedOnValue v t
                    "wrong output type of lambda's value (wrong stack size)"
          (Left m, _, _) ->
            throwError $ TCFailedOnValue v t $
                    "wrong output type of lambda's value: " <> m
  where
    unexpectedErr m = TCFailedOnValue v t ("unexpected " <> m)

typeCheckValImpl _ v t = throwError $ TCFailedOnValue v t ""

typeCheckValsImpl
  :: forall t . (Typeable t, Show InstrExtT)
  => TcInstrHandler
  -> [Un.Value Un.Op]
  -> T
  -> TypeCheckT ([Val Instr t], Notes t)
typeCheckValsImpl tcDo mvs t = foldM check ([], NStar) mvs
  where
    check (res, ns) mv = do
      v :::: ((_ :: Sing t'), vns) <- typeCheckValImpl tcDo mv t
      Refl <- liftEither $ eqT' @t @t'
                `onLeft` (TCFailedOnValue mv t . ("wrong element type " <>))
      ns' <- liftEither $ converge ns vns `onLeft` TCFailedOnValue mv t
      pure (v : res, ns')
