module Michelson.TypeCheck.Value
    ( typeCheckValImpl
    , typeCheckCValue
    ) where

import Control.Monad.Except (liftEither, throwError)
import Data.Default (def)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Singletons (SingI)
import Data.Typeable ((:~:)(..))
import Fmt (pretty)
import Prelude hiding (EQ, GT, LT)

import Michelson.Text
import Michelson.TypeCheck.Error (TCError(..), TCTypeError(..))
import Michelson.TypeCheck.Helpers
import Michelson.TypeCheck.TypeCheck (TcInstrHandler, TypeCheckEnv(..), TypeCheckInstr)
import Michelson.TypeCheck.Types
import Michelson.Typed
  (CT(..), CValue(..), Notes(..), Notes'(..), Sing(..), Value'(..), converge, fromSingCT,
  fromSingT, mkNotes, notesCase)
import qualified Michelson.Typed as T
import qualified Michelson.Untyped as U
import Tezos.Address (Address(..), parseAddress)
import Tezos.Core (mkMutez, parseTimestamp, timestampFromSeconds)
import Tezos.Crypto (parseKeyHash, parsePublicKey, parseSignature)

typeCheckCValue
  :: U.Value' op -> CT -> Either (U.Value' op, TCTypeError) SomeCValue
typeCheckCValue (U.ValueInt i) CInt = pure $ CvInt i :--: SCInt
typeCheckCValue (U.ValueInt i) CNat
  | i >= 0 = pure $ CvNat (fromInteger i) :--: SCNat
typeCheckCValue (U.ValueInt (mkMutez . fromInteger -> Just mtz)) CMutez =
  pure $ CvMutez mtz :--: SCMutez
typeCheckCValue (U.ValueString s) CString =
  pure $ CvString s :--: SCString
typeCheckCValue (U.ValueString (parseAddress . unMText -> Right s)) CAddress =
  pure $ CvAddress s :--: SCAddress
typeCheckCValue (U.ValueString (parseKeyHash . unMText -> Right s)) CKeyHash =
  pure $ CvKeyHash s :--: SCKeyHash
typeCheckCValue (U.ValueString (parseTimestamp . unMText -> Just t)) CTimestamp =
  pure $ CvTimestamp t :--: SCTimestamp
typeCheckCValue (U.ValueInt i) CTimestamp =
  pure $ CvTimestamp (timestampFromSeconds i) :--: SCTimestamp
typeCheckCValue (U.ValueBytes (U.InternalByteString s)) CBytes =
  pure $ CvBytes s :--: SCBytes
typeCheckCValue U.ValueTrue CBool = pure $ CvBool True :--: SCBool
typeCheckCValue U.ValueFalse CBool = pure $ CvBool False :--: SCBool
typeCheckCValue v t =
  Left $ (v, UnknownType (T.Tc t))

typeCheckCVals
  :: forall t op . (Typeable t, SingI t)
  => [U.Value' op]
  -> CT
  -> Either (U.Value' op, TCTypeError) [CValue t]
typeCheckCVals mvs t = traverse check mvs
  where
    check mv = do
      v :--: (_ :: Sing t') <- typeCheckCValue mv t
      Refl <- eqType @('T.Tc t) @('T.Tc t') `onLeft` (,) mv
      pure v

tcFailedOnValue :: U.Value -> T.T -> Text -> Maybe TCTypeError -> TypeCheckInstr a
tcFailedOnValue v t msg err = do
  loc <- ask
  throwError $ TCFailedOnValue v t msg loc err

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
  :: TcInstrHandler
  -> U.Value
  -> (Sing t, Notes t)
  -> TypeCheckInstr SomeValue
typeCheckValImpl _ mv (t@(STc ct), ann) = do
  let nt = notesCase U.noAnn (\(NTc x) -> x) ann
  case typeCheckCValue mv (fromSingCT ct) of
    Left (uval, err) -> tcFailedOnValue uval (fromSingT $ t) "" (Just err)
    Right (v :--: cst) -> pure $ VC v :::: (STc cst, mkNotes $ NTc nt)
typeCheckValImpl _ (U.ValueString (parsePublicKey . unMText -> Right s)) t@(STKey, _) =
  pure $ T.VKey s :::: t

typeCheckValImpl _ (U.ValueString (parseSignature . unMText -> Right s)) t@(STSignature, _) =
  pure $ VSignature s :::: t

typeCheckValImpl _ (U.ValueString (parseAddress . unMText -> Right s@(KeyAddress _)))
                   t@(STContract STUnit, _) =
    pure $ T.VContract s :::: t

typeCheckValImpl _ cv@(U.ValueString (parseAddress . unMText -> Right s))
                   t@(STContract pc, cn) = do
  instrPos <- ask
  let tcFail = \msg -> TCFailedOnValue cv (fromSingT $ fst t) msg instrPos Nothing
  let pn = notesCase NStar (\(NTContract _ x) -> x) cn
  contracts <- gets tcContracts
  case M.lookup s contracts of
    Just contractParam -> do
      liftEither $ first (TCFailedOnValue cv (fromSingT $ fst t) "invalid contract parameter" instrPos . Just) $
        compareTypes (pc, pn) contractParam
      pure $ VContract s :::: t
    _ -> throwError $ tcFail $ "Contract literal " <> pretty s <> " doesn't exist"

typeCheckValImpl _ U.ValueUnit t@(STUnit, _) = pure $ VUnit :::: t
typeCheckValImpl tcDo (U.ValuePair ml mr) (STPair lt rt, pn) = do
  let (n1, n2, n3, nl, nr) =
        notesCase (U.noAnn, U.noAnn, U.noAnn, NStar, NStar) (\(NTPair x1 x2 x3 xl xr) -> (x1, x2, x3, xl, xr)) pn
  l :::: (lst, ln) <- typeCheckValImpl tcDo ml (lt, nl)
  r :::: (rst, rn) <- typeCheckValImpl tcDo mr (rt, nr)
  let ns = mkNotes $ NTPair n1 n2 n3 ln rn
  pure $ VPair (l, r) :::: (STPair lst rst, ns)
typeCheckValImpl tcDo (U.ValueLeft ml) (STOr lt rt, ann) = do
  let (n1, n2, n3, nl, nr) =
        notesCase (U.noAnn, U.noAnn, U.noAnn, NStar, NStar) (\(NTOr x1 x2 x3 xl xr) -> (x1, x2, x3, xl, xr)) ann
  l :::: (lst, ln) <- typeCheckValImpl tcDo ml (lt, nl)
  pure $ VOr (Left l) :::: ( STOr lst rt
                            , mkNotes $ NTOr n1 n2 n3 ln nr )
typeCheckValImpl tcDo (U.ValueRight mr) (STOr lt rt, ann) = do
  let (n1, n2, n3, nl, nr) =
        notesCase (U.noAnn, U.noAnn, U.noAnn, NStar, NStar) (\(NTOr x1 x2 x3 xl xr) -> (x1, x2, x3, xl, xr)) ann
  r :::: (rst, rn) <- typeCheckValImpl tcDo mr (rt, nr)
  pure $ VOr (Right r) :::: ( STOr lt rst
                            , mkNotes $ NTOr n1 n2 n3 nl rn )
typeCheckValImpl tcDo (U.ValueSome mv) (STOption vt, ann) = do
  let (n1, n2, nt) = notesCase (U.noAnn, U.noAnn, NStar) (\(NTOption x1 x2 xt) -> (x1, x2, xt)) ann
  v :::: (vst, vns) <- typeCheckValImpl tcDo mv (vt, nt)
  let ns = mkNotes $ NTOption n1 n2 vns
  pure $ VOption (Just v) :::: (STOption vst, ns)
typeCheckValImpl _ U.ValueNone t@(STOption _, _) =
  pure $ VOption Nothing :::: t

typeCheckValImpl _ U.ValueNil t@(STList _, _) =
  pure $ T.VList [] :::: t

typeCheckValImpl tcDo (U.ValueSeq (toList -> mels)) t@(STList vt, ann) = do
  let nt = notesCase NStar (\(NTList _ x) -> x) ann
  (els, _) <- typeCheckValsImpl tcDo mels (vt, nt)
  pure $ VList els :::: t

typeCheckValImpl _ U.ValueNil t@(STSet _, _) = pure $ T.VSet S.empty :::: t

typeCheckValImpl _ sq@(U.ValueSeq (toList -> mels)) t@(STSet vt, _) = do
  instrPos <- ask
  els <- liftEither $ typeCheckCVals mels (fromSingCT vt)
          `onLeft` \(cv, err) -> TCFailedOnValue cv (fromSingT $ STc vt)
                                      "wrong type of set element:" instrPos (Just err)
  elsS <- liftEither $ S.fromDistinctAscList <$> ensureDistinctAsc id els
            `onLeft` \msg -> TCFailedOnValue sq (fromSingT $ STc vt) msg instrPos Nothing
  pure $ VSet elsS :::: t

typeCheckValImpl _ U.ValueNil t@(STMap _ _, _) = pure $ T.VMap M.empty :::: t

typeCheckValImpl tcDo sq@(U.ValueMap (toList -> mels)) t@(STMap kt vt, ann) = do
  let vn = notesCase NStar (\(NTMap _ _ nt) -> nt) ann
  keyOrderedElts <- typeCheckMapVal tcDo mels sq vn kt vt
  pure $ VMap (M.fromDistinctAscList keyOrderedElts) :::: t

typeCheckValImpl _ U.ValueNil t@(STBigMap _ _ , _) = pure $ T.VBigMap M.empty :::: t

typeCheckValImpl tcDo sq@(U.ValueMap (toList -> mels)) t@(STBigMap kt vt, ann) = do
  let vn = notesCase NStar (\(NTBigMap _ _ nt) -> nt) ann
  keyOrderedElts <- typeCheckMapVal tcDo mels sq vn kt vt
  pure $ VBigMap (M.fromDistinctAscList keyOrderedElts) :::: t

typeCheckValImpl tcDo v (t@(STLambda (it :: Sing it) (ot :: Sing ot)), ann) = do
  mp <- case v of
    U.ValueNil       -> pure []
    U.ValueLambda mp -> pure $ toList mp
    _ -> tcFailedOnValue v (fromSingT t) "unexpected value" Nothing
  let vn = notesCase U.noAnn (\(NTLambda n1 _ _) -> n1) ann
  li :/ instr <- typeCheckImpl tcDo mp ((it, NStar, def) ::& SNil)
  let (_, ins, _) ::& SNil = li
  let lamS = STLambda it ot
  let lamN ons = mkNotes $ NTLambda def ins ons
  case instr of
    lam ::: (lo :: HST lo) -> do
      case eqHST1 @ot lo of
        Right Refl -> do
          let (_, ons, _) ::& SNil = lo
          let ns = mkNotes $ NTLambda vn ins ons
          pure $ VLam lam :::: (STLambda it ot, ns)
        Left m ->
          tcFailedOnValue v (fromSingT t)
                  "wrong output type of lambda's value:" (Just m)
    AnyOutInstr lam ->
      pure $ VLam lam :::: (lamS, lamN NStar)

typeCheckValImpl _ v (t, _) = tcFailedOnValue v (fromSingT t) "unknown value" Nothing

-- | Function @typeCheckMapVal@ typechecks given list of @Elt@s and
-- ensures, that its keys are in ascending order.
--
-- It return list of pairs (key, value) with keys in ascending order
-- so it is safe to call @fromDistinctAscList@ on returned list
typeCheckMapVal
  :: (SingI kt, Typeable kt, SingI vt, Typeable vt)
  => TcInstrHandler
  -> [U.Elt U.ExpandedOp]
  -> U.Value
  -> Notes vt
  -> Sing kt
  -> Sing vt
  -> TypeCheckInstr [(CValue kt, T.Value vt)]
typeCheckMapVal tcDo mels sq vn kt vt = do
  instrPos <- ask
  ks <- liftEither $ typeCheckCVals (map (\(U.Elt k _) -> k) mels) (fromSingCT kt)
          `onLeft` \(cv, err) -> TCFailedOnValue cv (fromSingT $ STc kt)
                                      "wrong type of map key:" instrPos (Just err)
  (vals, _) <- typeCheckValsImpl tcDo (map (\(U.Elt _ v) -> v) mels) (vt, vn)
  ksS <- liftEither $ ensureDistinctAsc id ks
        `onLeft` \msg -> TCFailedOnValue sq (fromSingT $ STc kt) msg instrPos Nothing
  pure $ zip ksS vals

typeCheckValsImpl
  :: forall t . (Typeable t, SingI t)
  => TcInstrHandler
  -> [U.Value]
  -> (Sing t, Notes t)
  -> TypeCheckInstr ([T.Value t], Notes t)
typeCheckValsImpl tcDo mvs (t, nt) =
  fmap (first reverse) $ foldM check ([], nt) mvs
  where
    check (res, ns) mv = do
      instrPos <- ask
      v :::: ((_ :: Sing t'), vns) <- typeCheckValImpl tcDo mv (t, nt)
      Refl <- liftEither $ eqType @t @t' `onLeft`
        (TCFailedOnValue mv (fromSingT t) "wrong element type" instrPos . Just)
      ns' <- liftEither $ converge ns vns `onLeft`
        ((TCFailedOnValue mv (fromSingT t) "wrong element type") instrPos . Just . AnnError)
      pure (v : res, ns')
