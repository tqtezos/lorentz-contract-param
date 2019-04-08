module Michelson.TypeCheck.Value
    ( typeCheckValImpl
    , typeCheckCValue
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
  (CT(..), ConversibleExt, Instr(..), InstrExtT, Notes(..), Notes'(..), Sing(..), T(..),
  Value'(..), converge, mkNotes, withSomeSingCT, withSomeSingT)
import qualified Michelson.Typed as T
import Michelson.Typed.Value (CValue(..))
import qualified Michelson.Untyped as U
import Tezos.Address (parseAddress)
import Tezos.Core (mkMutez, parseTimestamp, timestampFromSeconds)
import Tezos.Crypto (parseKeyHash, parsePublicKey, parseSignature)

typeCheckCValue :: U.Value' op -> CT -> Maybe SomeCValue
typeCheckCValue (U.ValueInt i) CInt = pure $ CvInt i :--: SCInt
typeCheckCValue (U.ValueInt i) CNat
  | i >= 0 = pure $ CvNat (fromInteger i) :--: SCNat
typeCheckCValue (U.ValueInt (mkMutez . fromInteger -> Just mtz)) CMutez =
  pure $ CvMutez mtz :--: SCMutez
typeCheckCValue (U.ValueString s) CString =
  pure $ CvString s :--: SCString
typeCheckCValue (U.ValueString (parseAddress -> Right s)) CAddress =
  pure $ CvAddress s :--: SCAddress
typeCheckCValue (U.ValueString (parseKeyHash -> Right s)) CKeyHash =
  pure $ CvKeyHash s :--: SCKeyHash
typeCheckCValue (U.ValueString (parseTimestamp -> Just t)) CTimestamp =
  pure $ CvTimestamp t :--: SCTimestamp
typeCheckCValue (U.ValueInt i) CTimestamp =
  pure $ CvTimestamp (timestampFromSeconds i) :--: SCTimestamp
typeCheckCValue (U.ValueBytes (U.InternalByteString s)) CBytes =
  pure $ CvBytes s :--: SCBytes
typeCheckCValue U.ValueTrue CBool = pure $ CvBool True :--: SCBool
typeCheckCValue U.ValueFalse CBool = pure $ CvBool False :--: SCBool
typeCheckCValue _ _ = Nothing

typeCheckCVals
  :: forall t op . Typeable t
  => [U.Value' op]
  -> CT
  -> Either (U.Value' op, Text) [CValue t]
typeCheckCVals mvs t = traverse check mvs
  where
    check mv = do
      v :--: (_ :: Sing t') <-
        maybe (Left (mv, "failed to typecheck cval")) pure $ typeCheckCValue mv t
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
  :: (Show InstrExtT, ConversibleExt, Eq U.ExpandedInstrExtU)
  => TcInstrHandler
  -> U.Value
  -> T
  -> TypeCheckT SomeValue
typeCheckValImpl _ mv t@(Tc ct) =
  maybe (throwError $ TCFailedOnValue mv t "")
        (\(v :--: cst) -> pure $ T.VC v :::: (STc cst, NStar))
        (typeCheckCValue mv ct)
typeCheckValImpl _ (U.ValueString (parsePublicKey -> Right s)) TKey =
  pure $ T.VKey s :::: (STKey, NStar)

typeCheckValImpl _ (U.ValueString (parseSignature -> Right s)) TSignature =
  pure $ T.VSignature s :::: (STSignature, NStar)

typeCheckValImpl _ (U.ValueString (parseAddress -> Right s)) (TContract pt) =
  withSomeSingT pt $ \p ->
    pure $ T.VContract s :::: (STContract p, NStar)
typeCheckValImpl _ U.ValueUnit TUnit = pure $ T.VUnit :::: (STUnit, NStar)
typeCheckValImpl tcDo (U.ValuePair ml mr) (TPair lt rt) = do
  l :::: (lst, ln) <- typeCheckValImpl tcDo ml lt
  r :::: (rst, rn) <- typeCheckValImpl tcDo mr rt
  let ns = mkNotes $ NTPair def def def ln rn
  pure $ T.VPair (l, r) :::: (STPair lst rst, ns)
typeCheckValImpl tcDo (U.ValueLeft ml) (TOr lt rt) = do
  l :::: (lst, ln) <- typeCheckValImpl tcDo ml lt
  withSomeSingT rt $ \rst ->
    pure $ T.VOr (Left l) :::: ( STOr lst rst
                             , mkNotes $ NTOr def def def ln NStar )
typeCheckValImpl tcDo (U.ValueRight mr) (TOr lt rt) = do
  r :::: (rst, rn) <- typeCheckValImpl tcDo mr rt
  withSomeSingT lt $ \lst ->
    pure $ T.VOr (Right r) :::: ( STOr lst rst
                              , mkNotes $ NTOr def def def NStar rn )
typeCheckValImpl tcDo (U.ValueSome mv) (TOption vt) = do
  v :::: (vst, vns) <- typeCheckValImpl tcDo mv vt
  let ns = mkNotes $ NTOption def def vns
  pure $ T.VOption (Just v) :::: (STOption vst, ns)
typeCheckValImpl _ U.ValueNone (TOption vt) =
  withSomeSingT vt $ \vst ->
    pure $ T.VOption Nothing :::: (STOption vst, NStar)

typeCheckValImpl _ U.ValueNil (TList vt) =
  withSomeSingT vt $ \vst ->
    pure $ T.VList [] :::: (STList vst, mkNotes $ NTList def NStar)

typeCheckValImpl tcDo (U.ValueSeq (toList -> mels)) (T.TList vt) =
  withSomeSingT vt $ \vst -> do
    (els, ns) <- typeCheckValsImpl tcDo mels vt
    pure $ T.VList els :::: (STList vst, mkNotes $ NTList def ns)

typeCheckValImpl _ U.ValueNil (TSet vt) =
  withSomeSingCT vt $ \vst ->
    pure $ T.VSet S.empty :::: (STSet vst, NStar)

typeCheckValImpl _ sq@(U.ValueSeq (toList -> mels)) (T.TSet vt) =
  withSomeSingCT vt $ \vst -> do
    els <- liftEither $ typeCheckCVals mels vt
            `onLeft` \(cv, err) -> TCFailedOnValue cv (Tc vt) $
                                      "wrong type of set element: " <> err
    elsS <- liftEither $ S.fromDistinctAscList <$> ensureDistinctAsc els
            `onLeft` TCFailedOnValue sq (Tc vt)
    pure $ T.VSet elsS :::: (STSet vst, NStar)

typeCheckValImpl _ U.ValueNil (TMap kt vt) =
  withSomeSingT vt $ \vst ->
  withSomeSingCT kt $ \kst -> do
    let ns = mkNotes $ NTMap def def NStar
    pure $ T.VMap M.empty :::: (STMap kst vst, ns)

typeCheckValImpl tcDo sq@(U.ValueMap (toList -> mels)) (TMap kt vt) =
  withSomeSingT vt $ \vst ->
  withSomeSingCT kt $ \kst -> do
    ks <- liftEither $  typeCheckCVals (map (\(U.Elt k _) -> k) mels) kt
            `onLeft` \(cv, err) -> TCFailedOnValue cv (Tc kt) $
                                      "wrong type of map key: " <> err
    (vals, vns) <- typeCheckValsImpl tcDo (map (\(U.Elt _ v) -> v) mels) vt
    let ns = mkNotes $ NTMap def def vns
    ksS <- liftEither $ ensureDistinctAsc ks
            `onLeft` TCFailedOnValue sq (Tc kt)
    pure $ T.VMap (M.fromDistinctAscList $ zip ksS vals) :::: (STMap kst vst, ns)

typeCheckValImpl tcDo v t@(TLambda mi mo) = do
  mp <- case v of
    U.ValueNil -> pure []
    U.ValueLambda mp -> pure $ toList mp
    _ -> throwError $ TCFailedOnValue v t ""

  withSomeSingT mi $ \(it :: Sing it) ->
    withSomeSingT mo $ \(ot :: Sing ot) ->
      typeCheckImpl tcDo mp ((it, NStar, def) ::& SNil) >>= \case
        SiFail -> pure $ VLam FAILWITH :::: (STLambda it ot, NStar)
        lam ::: (li, (lo :: HST lo)) -> do
          case eqT' @'[ ot ] @lo of
            Right Refl -> do
              let (_, ons, _) ::& SNil = lo
              let (_, ins, _) ::& SNil = li
              let ns = mkNotes $ NTLambda def ins ons
              pure $ VLam lam :::: (STLambda it ot, ns)
            Left m ->
              throwError $ TCFailedOnValue v t $
                      "wrong output type of lambda's value: " <> m

typeCheckValImpl _ v t = throwError $ TCFailedOnValue v t ""

typeCheckValsImpl
  :: forall t . (Typeable t, Show InstrExtT, ConversibleExt, Eq U.ExpandedInstrExtU)
  => TcInstrHandler
  -> [U.Value]
  -> T
  -> TypeCheckT ([T.Value t], Notes t)
typeCheckValsImpl tcDo mvs t =
  fmap (first reverse) $ foldM check ([], NStar) mvs
  where
    check (res, ns) mv = do
      v :::: ((_ :: Sing t'), vns) <- typeCheckValImpl tcDo mv t
      Refl <- liftEither $ eqT' @t @t' `onLeft` (TCFailedOnValue mv t . ("wrong element type " <>))
      ns' <- liftEither $ converge ns vns `onLeft` TCFailedOnValue mv t
      pure (v : res, ns')
