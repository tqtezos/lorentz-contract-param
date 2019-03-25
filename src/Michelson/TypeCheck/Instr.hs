-- | Module, providing functions for conversion from
-- instruction and value representation from @Michelson.Type@ module
-- to strictly-typed GADT-based representation from @Michelson.Value@ module.
--
-- This conversion is labeled as type check because that's what we are obliged
-- to do on our way.
--
-- Type check algorithm relies on the property of Michelson language that each
-- instruction on a given input stack type produces a definite output stack
-- type.
-- Michelson contract defines concrete types for storage and parameter, from
-- which input stack type is deduced. Then this type is being combined with
-- each subsequent instruction, producing next stack type after each
-- application.
--
-- Function @typeCheck@ takes list of instructions and returns value of type
-- @Instr inp out@ along with @HST inp@ and @HST out@ all wrapped into
-- @SomeInstr@ data type. This wrapping is done to satsify Haskell type
-- system (which has no support for dependent types).
-- Functions @typeCheckInstr@, @typeCheckVal@ behave similarly.
--
-- When a recursive call is made within @typeCheck@, @typeCheckInstr@ or
-- @typeCheckVal@, result of a call is unwrapped from @SomeInstr@ and type
-- information from @HST inp@ and @HST out@ is being used to assert that
-- recursive call returned instruction of expected type
-- (error is thrown otherwise).
module Michelson.TypeCheck.Instr
    ( typeCheckContract
    , typeCheckVal
    , typeCheckList
    ) where

import Prelude hiding (EQ, GT, LT)

import Control.Monad.Except (liftEither, throwError, withExceptT)
import Data.Default (def)
import Data.Singletons (SingI(sing))
import Data.Typeable ((:~:)(..))

import Michelson.TypeCheck.Helpers
import Michelson.TypeCheck.Types
import Michelson.TypeCheck.Value
import Michelson.Typed
  (Abs, And, CT(..), Contract, ContractInp, ContractOut, Eq', Ge, Gt, Instr(..), IterOp(..), Le,
  Lsl, Lsr, Lt, MapOp(..), Neg, Neq, Not, Notes(..), Notes'(..), Or, Sing(..), T(..), Val(..), Xor,
  converge, convergeAnns, extractNotes, fromUType, mkNotes, notesCase, orAnn, withSomeSingCT,
  withSomeSingT, ( # ))

import qualified Michelson.Untyped as Un
import Michelson.Untyped.Annotation (VarAnn)

typeCheckContract
  :: ExtC
  => TcExtHandler
  -> Un.Contract Un.Instr
  -> Either TCError SomeContract
typeCheckContract nh c = runTypeCheckT nh (Un.para c) $ typeCheckContractImpl c

typeCheckContractImpl
  :: ExtC
  => Un.Contract Un.Instr
  -> TypeCheckT SomeContract
typeCheckContractImpl (Un.Contract mParam mStorage pCode) = do
  code <- maybe (throwError $ TCOtherError "no instructions in contract code")
                pure (nonEmpty pCode)
  withSomeSingT (fromUType mParam) $ \(paramS :: Sing param) ->
    withSomeSingT (fromUType mStorage) $ \(storageS :: Sing st) -> do
      storageNote <-
        liftEither $ extractNotes mStorage storageS `onLeft` \m -> TCOtherError $
                        "failed to extract annotations for storage: " <> m
      paramNote <-
        liftEither $ extractNotes mParam paramS `onLeft` \m -> TCOtherError $
                        "failed to extract annotations for parameter: " <> m
      let inpNote = mkNotes (NTPair def def def paramNote storageNote)
      let inp = (STPair paramS storageS, inpNote, def) ::& SNil
      typeCheckNE code (SomeHST inp) >>= \case
        SiFail -> do
          let outNote = mkNotes (NTPair def def def NStar storageNote)
              out = (STPair (STList STOperation) storageS, outNote, def)
                      ::& SNil
          pure $ SomeContract (FAILWITH :: Instr (ContractInp param st) (ContractOut st)) inp out
        instr ::: ((inp' :: HST inp), (out :: HST out)) -> do
          let mkIErr m = TCOtherError $
                          "contract input type violates convention: " <> m
          let mkOErr m = TCOtherError $
                          "contract output type violates convention: " <> m
          liftEither $ do
            Refl <- eqT' @out @(ContractOut st) `onLeft` mkOErr
            Refl <- eqT' @inp @(ContractInp param st) `onLeft` mkIErr
            let outN = outNotes out
            _ <- converge outN (N $ NTPair def def def NStar storageNote)
                    `onLeft` mkOErr
            pure $ SomeContract instr inp' out
  where
    outNotes :: HST '[o] -> Notes o
    outNotes ((_, n, _) ::& SNil) = n

-- | Like 'typeCheck', but for non-empty lists.
typeCheckNE
  :: ExtC
  => NonEmpty Un.Instr
  -> SomeHST
  -> TypeCheckT SomeInstr
typeCheckNE (x :| xs) = typeCheckImpl typeCheckInstr (x : xs)

-- | Function @typeCheckList@ converts list of Michelson instructions
-- given in representation from @Michelson.Type@ module to representation
-- in strictly typed GADT.
--
-- Types are checked along the way which is neccessary to construct a
-- strictly typed value.
--
-- As a second argument, @typeCheckList@ accepts input stack type representation.
typeCheckList
  :: ExtC
  => [Un.Instr]
  -> SomeHST
  -> TypeCheckT SomeInstr
typeCheckList = typeCheckImpl typeCheckInstr

-- | Function @typeCheckVal@ converts a single Michelson value
-- given in representation from @Michelson.Type@ module to representation
-- in strictly typed GADT.
--
-- As a second argument, @typeCheckVal@ accepts expected type of value.
--
-- Type checking algorithm pattern-matches on parse value representation,
-- expected type @t@ and constructs @Val t@ value.
--
-- If there was no match on a given pair of value and expected type,
-- that is interpreted as input of wrong type and type check finishes with
-- error.
typeCheckVal
  :: ExtC
  => Un.Value Un.Op
  -> T
  -> TypeCheckT SomeVal
typeCheckVal = typeCheckValImpl typeCheckInstr

-- | Function @typeCheckInstr@ converts a single Michelson instruction
-- given in representation from @Michelson.Type@ module to representation
-- in strictly typed GADT.
--
-- As a second argument, @typeCheckInstr@ accepts input stack type representation.
--
-- Type checking algorithm pattern-matches on given instruction, input stack
-- type and constructs strictly typed GADT value, checking necessary type
-- equalities when neccessary.
--
-- If there was no match on a given pair of instruction and input stack,
-- that is interpreted as input of wrong type and type check finishes with
-- error.
typeCheckInstr
  :: ExtC
  => TcInstrHandler

typeCheckInstr (Un.EXT ext) si@(SomeHST it) = do
  nh <- gets tcExtHandler
  nfs <- gets tcExtFrames
  (nfs', res) <- nh ext nfs si
  modify $ \te -> te {tcExtFrames = nfs'}
  case res of
    Just tExt -> pure $ Ext tExt ::: (it, it)
    Nothing   -> pure $ Nop ::: (it, it)

typeCheckInstr Un.DROP (SomeHST i@(_ ::& rs)) = pure (DROP ::: (i, rs))

typeCheckInstr (Un.DUP _vn) (SomeHST i@(a ::& rs)) =
  pure (DUP ::: (i, (a ::& a::& rs)))

typeCheckInstr Un.SWAP (SomeHST i@(a ::& b ::& rs)) =
  pure (SWAP ::: (i, b ::& a ::& rs))

typeCheckInstr instr@(Un.PUSH vn mt mval) (SomeHST i) = do
  val :::: (t, n) <- typeCheckVal mval (fromUType mt)
  notes <- liftEither $ (extractNotes mt t >>= converge n)
              `onLeft` TCFailedOnInstr instr (SomeHST i)
  pure $ PUSH val ::: (i, (t, notes, vn) ::& i)

typeCheckInstr (Un.SOME tn vn fn) (SomeHST i@((at, an, _) ::& rs)) = do
  let n = mkNotes (NTOption tn fn an)
  pure (SOME ::: (i, (STOption at, n, vn) ::& rs))

typeCheckInstr instr@(Un.NONE tn vn fn elMt) (SomeHST i) = do
  withSomeSingT (fromUType elMt) $ \elT -> do
    let t = STOption elT
    notes <- liftEither $ extractNotes (Un.Type (Un.TOption fn elMt) tn) t
              `onLeft` TCFailedOnInstr instr (SomeHST i)
    pure $ NONE ::: (i, (t, notes, vn) ::& i)

typeCheckInstr (Un.UNIT tn vn) (SomeHST i) = do
  let ns = mkNotes $ NTUnit tn
  pure $ UNIT ::: (i, (STUnit, ns, vn) ::& i)

typeCheckInstr (Un.IF_NONE mp mq) (SomeHST i@((STOption a, ons, ovn) ::& rs) ) = do
  let (an, avn) = deriveNsOption ons ovn
  genericIf IF_NONE Un.IF_NONE mp mq rs ((a, an, avn) ::& rs) i

typeCheckInstr (Un.PAIR tn vn pfn qfn) (SomeHST i@((a, an, avn) ::&
                                             (b, bn, bvn) ::& rs)) = do
  let (vn', pfn', qfn') = deriveSpecialFNs pfn qfn avn bvn
      ns = mkNotes $ NTPair tn pfn' qfn' an bn
  pure (PAIR ::: (i, (STPair a b, ns, vn `orAnn` vn') ::& rs))

typeCheckInstr (Un.CAR vn _) (SomeHST i@((STPair a _, NStar, _) ::& rs)) =
  pure (CAR ::: (i, (a, NStar, vn) ::& rs))
typeCheckInstr instr@(Un.CAR vn fn)
            (SomeHST i@(( STPair a b
                       , N (NTPair pairTN pfn qfn pns qns)
                       , pairVN ) ::& rs)) = do
  pfn' <- liftEither $ convergeAnns fn pfn
              `onLeft` TCFailedOnInstr instr (SomeHST i)
  let vn' = deriveSpecialVN vn pfn' pairVN
      i' = ( STPair a b
            , N (NTPair pairTN pfn' qfn pns qns)
            , pairVN ) ::& rs
  pure $ CAR ::: (i', (a, pns, vn') ::& rs)

typeCheckInstr (Un.CDR vn _) (SomeHST i@((STPair _ b, NStar, _) ::& rs)) =
  pure (CDR ::: (i, (b, NStar, vn) ::& rs))
typeCheckInstr instr@(Un.CDR vn fn)
          (SomeHST i@(( STPair a b
                      , N (NTPair pairTN pfn qfn pns qns)
                      , pairVN ) ::& rs)) = do
  qfn' <- liftEither $ convergeAnns fn qfn
              `onLeft` TCFailedOnInstr instr (SomeHST i)
  let vn' = deriveSpecialVN vn qfn' pairVN
      i' = ( STPair a b
            , N (NTPair pairTN pfn qfn' pns qns)
            , pairVN ) ::& rs
  pure $ CDR ::: (i', (b, qns, vn') ::& rs)

typeCheckInstr instr@(Un.LEFT tn vn pfn qfn bMt) (SomeHST i@((a, an, _) ::& rs)) =
  withSomeSingT (fromUType bMt) $ \b -> do
    bn <- liftEither $ extractNotes bMt b
              `onLeft` TCFailedOnInstr instr (SomeHST i)
    let ns = mkNotes $ NTOr tn pfn qfn an bn
    pure (LEFT ::: (i, (STOr a b, ns, vn) ::& rs))

typeCheckInstr instr@(Un.RIGHT tn vn pfn qfn aMt) (SomeHST i@((b, bn, _) ::& rs)) =
  withSomeSingT (fromUType aMt) $ \a -> do
    an <- liftEither $ extractNotes aMt a
              `onLeft` TCFailedOnInstr instr (SomeHST i)
    let ns = mkNotes $ NTOr tn pfn qfn an bn
    pure (RIGHT ::: (i, (STOr a b, ns, vn) ::& rs))

typeCheckInstr (Un.IF_LEFT mp mq) (SomeHST i@((STOr a b, ons, ovn) ::& rs) ) = do
  let (an, bn, avn, bvn) = deriveNsOr ons ovn
      ait = (a, an, avn) ::& rs
      bit = (b, bn, bvn) ::& rs
  genericIf IF_LEFT Un.IF_LEFT mp mq ait bit i

typeCheckInstr (Un.IF_RIGHT mq mp) (SomeHST i@((STOr a b, ons, ovn) ::& rs) ) = do
  let (an, bn, avn, bvn) = deriveNsOr ons ovn
      ait = (a, an, avn) ::& rs
      bit = (b, bn, bvn) ::& rs
  genericIf IF_RIGHT Un.IF_RIGHT mq mp bit ait i

typeCheckInstr instr@(Un.NIL tn vn elMt) (SomeHST i) =
  withSomeSingT (fromUType elMt) $ \elT -> liftEither $ do
    let t = STList elT
    notes <- extractNotes (Un.Type (Un.TList elMt) tn) t
              `onLeft` TCFailedOnInstr instr (SomeHST i)
    pure $ NIL ::: (i, (t, notes, vn) ::& i)

typeCheckInstr instr@(Un.CONS vn) (SomeHST i@(((at :: Sing a), an, _)
                              ::& (STList (_ :: Sing a'), ln, _) ::& rs)) =
  case eqT' @a @a' of
    Right Refl -> liftEither $  do
      n <- converge ln (mkNotes $ NTList def an)
              `onLeft` TCFailedOnInstr instr (SomeHST i)
      pure $ CONS ::: (i, (STList at, n, vn) ::& rs)
    Left m -> typeCheckInstrErrM instr (SomeHST i) $
                "list element type is different from one "
                <> "that is being CONSed: " <> m


typeCheckInstr (Un.IF_CONS mp mq) (SomeHST i@((STList a, ns, vn) ::& rs) ) = do
  let an = notesCase NStar (\(NTList _ an_) -> an_) ns
      ait =
        (a, an, vn <> "hd") ::& (STList a, ns, vn <> "tl") ::& rs
  genericIf IF_CONS Un.IF_CONS mp mq ait rs i

typeCheckInstr (Un.SIZE vn) (SomeHST i@((STList _, _, _) ::& _) ) = liftEither $ sizeImpl i vn
typeCheckInstr (Un.SIZE vn) (SomeHST i@((STSet _, _, _) ::& _) ) = liftEither $ sizeImpl i vn
typeCheckInstr (Un.SIZE vn) (SomeHST i@((STMap _ _, _, _) ::& _) ) = liftEither $ sizeImpl i vn
typeCheckInstr (Un.SIZE vn) (SomeHST i@((STc SCString, _, _) ::& _) ) = liftEither $  sizeImpl i vn
typeCheckInstr (Un.SIZE vn) (SomeHST i@((STc SCBytes, _, _) ::& _) ) = liftEither $ sizeImpl i vn

typeCheckInstr (Un.EMPTY_SET tn vn (Un.Comparable mk ktn)) (SomeHST i) =
  withSomeSingCT mk $ \k ->
    pure $ EMPTY_SET ::: (i, (STSet k, mkNotes $ NTSet tn ktn, vn) ::& i)

typeCheckInstr instr@(Un.EMPTY_MAP tn vn (Un.Comparable mk ktn) mv) (SomeHST i) =
  withSomeSingT (fromUType mv) $ \v ->
  withSomeSingCT mk $ \k -> liftEither $ do
    vns <- extractNotes mv v
              `onLeft` TCFailedOnInstr instr (SomeHST i)
    let ns = mkNotes $ NTMap tn ktn vns
    pure $ EMPTY_MAP ::: (i, (STMap k v, ns, vn) ::& i)

typeCheckInstr instr@(Un.MAP vn mp) (SomeHST i@((STList v, ns, _vn) ::& rs) ) = do
  let vns = notesCase NStar (\(NTList _ v') -> v') ns
  typeCheckList (Un.unOp <$> mp)
                      (SomeHST $ (v, vns, def) ::& rs) >>= \case
    SiFail -> pure SiFail
    someInstr@(_ ::: (_, (out :: HST out))) ->
      case out of
        (_ :: Sing b, _, _) ::& _ ->
          mapImpl @b instr someInstr i
                  (\rt rn -> (::&) (STList rt, mkNotes $ NTList def rn, vn))
        _ -> liftEither $ typeCheckInstrErr instr (SomeHST i) $
              "iteration expression has wrong output stack type (empty stack)"

typeCheckInstr instr@(Un.MAP vn mp) (SomeHST i@((STMap k v, ns, _vn) ::& rs) ) = do
  let (kns, vns) = notesCase (def, NStar) (\(NTMap _ k' v') -> (k', v')) ns
      pns = mkNotes $ NTPair def def def (mkNotes $ NTc kns) vns
  typeCheckList (Un.unOp <$> mp)
                      (SomeHST $ ((STPair (STc k) v), pns, def) ::& rs) >>= \case
    SiFail -> pure SiFail
    someInstr@(_ ::: (_, (out :: HST out))) ->
      case out of
        (_ :: Sing b, _, _) ::& _ ->
          mapImpl @b instr someInstr i
                  (\rt rn -> (::&) (STMap k rt, mkNotes $ NTMap def kns rn, vn))
        _ -> liftEither $ typeCheckInstrErr instr (SomeHST i) $
              "iteration expression has wrong output stack type (empty stack)"

-- case `Un.HSTER []` is wrongly typed by definition
-- (as it is required to at least drop an element), so we don't consider it

typeCheckInstr instr@(Un.ITER (i1 : ir)) (SomeHST i@((STSet e, n, _) ::& _)) = do
  let en = notesCase NStar (\(NTSet _ en_) -> mkNotes $ NTc en_) n
  iterImpl (STc e) en instr (i1 :| ir) i
typeCheckInstr instr@(Un.ITER (i1 : ir)) (SomeHST i@((STList e, n, _) ::& _)) = do
  let en = notesCase NStar (\(NTList _ en_) -> en_) n
  iterImpl e en instr (i1 :| ir) i
typeCheckInstr instr@(Un.ITER (i1 : ir)) (SomeHST i@((STMap k v, n, _) ::& _)) = do
  let en = notesCase NStar (\(NTMap _ kns vns) ->
              mkNotes $ NTPair def def def (mkNotes $ NTc kns) vns) n
  iterImpl (STPair (STc k) v) en instr (i1 :| ir) i

typeCheckInstr instr@(Un.MEM vn)
           (SomeHST i@((STc _, _, _) ::& (STSet _, _, _) ::& _)) =
  liftEither $ memImpl instr i vn
typeCheckInstr instr@(Un.MEM vn)
           (SomeHST i@((STc _, _, _) ::& (STMap _ _, _, _) ::& _)) =
  liftEither $ memImpl instr i vn
typeCheckInstr instr@(Un.MEM vn)
           (SomeHST i@((STc _, _, _) ::& (STBigMap _ _, _, _) ::& _)) =
  liftEither $ memImpl instr i vn

typeCheckInstr instr@(Un.GET vn) (SomeHST i@(_ ::& (STMap _ vt, cn, _) ::& _)) =
  liftEither $ getImpl instr i vt (notesCase NStar (\(NTMap _ _ v) -> v) cn) vn
typeCheckInstr instr@(Un.GET vn) (SomeHST i@(_ ::& (STBigMap _ vt, cn, _) ::& _)) =
  liftEither $ getImpl instr i vt (notesCase NStar (\(NTBigMap _ _ v) -> v) cn) vn

typeCheckInstr instr@Un.UPDATE (SomeHST i@(_ ::& _ ::& (STMap _ _, _, _) ::& _)) =
  liftEither $ updImpl instr i
typeCheckInstr instr@Un.UPDATE (SomeHST i@(_ ::& _ ::& (STBigMap _ _, _, _)
                                             ::& _)) =
  liftEither $ updImpl instr i
typeCheckInstr instr@Un.UPDATE (SomeHST i@(_ ::& _ ::& (STSet _, _, _) ::& _)) =
  liftEither $ updImpl instr i

typeCheckInstr (Un.IF mp mq) (SomeHST i@((STc SCBool, _, _) ::& rs) ) =
  genericIf IF Un.IF mp mq rs rs i

typeCheckInstr instr@(Un.LOOP is)
           (SomeHST i@((STc SCBool, _, _) ::& (rs :: HST rs)) ) = do
  typeCheckList (fmap Un.unOp is) (SomeHST rs) >>= \case
    SiFail -> pure SiFail
    subI ::: ((_ :: HST rs'), (o :: HST o)) -> liftEither $ do
      Refl <- assertEqT @rs @rs' instr i
      case (eqT' @o @('Tc 'CBool ': rs), SomeHST o) of
        (Right Refl, SomeHST (_ ::& rs' :: HST o')) -> do
            Refl <- assertEqT @o @o' instr i
            pure $ LOOP subI ::: (i, rs')
        (Left m, _) ->
          typeCheckInstrErr instr (SomeHST i) $
                    "iteration expression has wrong output stack type: " <> m
        _ -> typeCheckInstrErr instr (SomeHST i) $
                        "iteration expression has wrong output stack type"

typeCheckInstr instr@(Un.LOOP_LEFT is)
           (SomeHST i@((STOr (at :: Sing a) (bt :: Sing b), ons, ovn)
                      ::& (rs :: HST rs)) ) = do
  let (an, bn, avn, bvn) = deriveNsOr ons ovn
      ait = (at, an, avn) ::& rs
  typeCheckList (fmap Un.unOp is) (SomeHST ait) >>= \case
    SiFail -> pure SiFail
    subI ::: ((_ :: HST rs'), (o :: HST o)) -> liftEither $ do
      Refl <- assertEqT @(a ': rs) @rs' instr i
      case (eqT' @o @('TOr a b ': rs), SomeHST o) of
        (Right Refl, SomeHST ((STOr _ bt', ons', ovn') ::& rs' :: HST o')) -> do
            Refl <- assertEqT @o @o' instr i
            let (_, bn', _, bvn') = deriveNsOr ons' ovn'
            br <- convergeHSTEl (bt, bn, bvn) (bt', bn', bvn')
                    `onLeft` TCFailedOnInstr instr (SomeHST i)
            pure $ LOOP_LEFT subI ::: (i, br ::& rs')
        (Left m, _) -> typeCheckInstrErr instr (SomeHST i) $
                        "iteration expression has wrong output stack type: " <> m
        _ -> typeCheckInstrErr instr (SomeHST i) $
                        "iteration expression has wrong output stack type"

typeCheckInstr instr@(Un.LAMBDA vn imt omt is) (SomeHST i) = do
  withSomeSingT (fromUType imt) $ \(it :: Sing it) -> do
    withSomeSingT (fromUType omt) $ \(ot :: Sing ot) -> do
      ins <- liftEither $ extractNotes imt it
              `onLeft` TCFailedOnInstr instr (SomeHST i)
      ons <- liftEither $ extractNotes omt ot
              `onLeft` TCFailedOnInstr instr (SomeHST i)
      -- further processing is extracted into another function because
      -- otherwise I encountered some weird GHC error with that code
      -- located right here
      lamImpl instr is vn it ins ot ons i

typeCheckInstr instr@(Un.EXEC vn) (SomeHST i@(((_ :: Sing t1), _, _)
                              ::& (STLambda (_ :: Sing t1') t2, ln, _)
                              ::& rs)) = do
  let t2n = notesCase NStar (\(NTLambda _ _ n) -> n) ln
  case eqT' @t1 @t1' of
    Right Refl -> pure $ EXEC ::: (i, (t2, t2n, vn) ::& rs)
    Left m -> typeCheckInstrErrM instr (SomeHST i) $
                "lambda is given argument with wrong type: " <> m

typeCheckInstr instr@(Un.DIP is) (SomeHST i@(a ::& (s :: HST s))) =
  typeCheckList (fmap Un.unOp is) (SomeHST s) >>= \case
    SiFail -> pure SiFail
    subI ::: ((_ :: HST s'), t) -> liftEither $ do
      Refl <- assertEqT @s @s' instr i
      pure $ DIP subI ::: (i, a ::& t)

typeCheckInstr Un.FAILWITH _ = pure SiFail

typeCheckInstr instr@(Un.CAST vn mt)
           (SomeHST i@(((e :: Sing e), (en :: Notes e), evn) ::& rs)) =
  withSomeSingT (fromUType mt) $ \(_ :: Sing e') ->
    case eqT' @e @e' of
      Right Refl ->
        case extractNotes mt e of
          Right en' ->
            case converge en en' of
              Right ns ->
                pure $ CAST ::: (i, (e, ns, vn `orAnn` evn) ::& rs)
              Left m -> err m
          Left m -> err m
      Left m -> err m
    where
      err = \m -> typeCheckInstrErrM instr (SomeHST i) $
                  "cast to incompatible type: " <> m

typeCheckInstr (Un.RENAME vn) (SomeHST i@((at, an, _) ::& rs)) =
  pure $ RENAME ::: (i, (at, an, vn) ::& rs)

typeCheckInstr instr@(Un.UNPACK vn mt) (SomeHST i@((STc SCBytes, _, _) ::& rs)) =
  withSomeSingT (fromUType mt) $ \t -> liftEither $ do
    tns <- extractNotes mt t
            `onLeft` TCFailedOnInstr instr (SomeHST i)
    let ns = mkNotes $ NTOption def def tns
    pure $ UNPACK ::: (i, (STOption t, ns, vn) ::& rs)

typeCheckInstr (Un.PACK vn) (SomeHST i@(_ ::& rs)) =
  pure $ PACK ::: (i, (STc SCBytes, NStar, vn) ::& rs)

typeCheckInstr (Un.CONCAT vn) (SomeHST i@((STc SCBytes, _, _) ::&
                                    (STc SCBytes, _, _) ::& _)) =
  liftEither $ concatImpl i vn
typeCheckInstr (Un.CONCAT vn) (SomeHST i@((STc SCString, _, _) ::&
                                    (STc SCString, _, _) ::& _)) =
  liftEither $ concatImpl i vn
typeCheckInstr (Un.CONCAT vn) (SomeHST i@((STList (STc SCBytes), _, _) ::& _)) =
  liftEither $  concatImpl' i vn
typeCheckInstr (Un.CONCAT vn) (SomeHST i@((STList (STc SCString), _, _) ::& _)) =
  liftEither $ concatImpl' i vn


typeCheckInstr (Un.SLICE vn) (SomeHST i@((STc SCNat, _, _) ::&
                                   (STc SCNat, _, _) ::&
                                   (STc SCString, _, _) ::& _)) =
  liftEither $ sliceImpl i vn
typeCheckInstr (Un.SLICE vn) (SomeHST i@((STc SCNat, _, _) ::&
                                   (STc SCNat, _, _) ::&
                                   (STc SCBytes, _, _) ::& _)) =
  liftEither $ sliceImpl i vn

typeCheckInstr (Un.ISNAT vn') (SomeHST i@((STc SCInt, _, oldVn) ::& rs)) = do
  let vn = vn' `orAnn` oldVn
  pure $ ISNAT ::: (i, (STOption (STc SCNat), NStar, vn) ::& rs)

typeCheckInstr (Un.ADD vn) (SomeHST i@((STc a, _, _) ::&
                                 (STc b, _, _) ::& _)) = liftEither $ addImpl a b i vn

typeCheckInstr (Un.SUB vn) (SomeHST i@((STc a, _, _) ::&
                                 (STc b, _, _) ::& _)) = liftEither $ subImpl a b i vn

typeCheckInstr (Un.MUL vn) (SomeHST i@((STc a, _, _) ::&
                                 (STc b, _, _) ::& _)) = liftEither $ mulImpl a b i vn

typeCheckInstr (Un.EDIV vn) (SomeHST i@((STc a, _, _) ::&
                                  (STc b, _, _) ::& _)) = liftEither $ edivImpl a b i vn

typeCheckInstr (Un.ABS vn) (SomeHST i@((STc SCInt, _, _) ::& _)) =
  liftEither $ unaryArithImpl @Abs ABS i vn

typeCheckInstr Un.NEG (SomeHST i@((STc SCInt, _, _) ::& _)) =
  liftEither $ unaryArithImpl @Neg NEG i def

typeCheckInstr (Un.LSL vn) (SomeHST i@((STc SCNat, _, _) ::&
                         (STc SCNat, _, _) ::& _)) = liftEither $ arithImpl @Lsl LSL i vn

typeCheckInstr (Un.LSR vn) (SomeHST i@((STc SCNat, _, _) ::&
                         (STc SCNat, _, _) ::& _)) = liftEither $ arithImpl @Lsr LSR i vn

typeCheckInstr (Un.OR vn) (SomeHST i@((STc SCBool, _, _) ::&
                         (STc SCBool, _, _) ::& _)) = liftEither $ arithImpl @Or OR i vn
typeCheckInstr (Un.OR vn) (SomeHST i@((STc SCNat, _, _) ::&
                         (STc SCNat, _, _) ::& _)) = liftEither $ arithImpl @Or OR i vn

typeCheckInstr (Un.AND vn) (SomeHST i@((STc SCInt, _, _) ::&
                         (STc SCNat, _, _) ::& _)) = liftEither $ arithImpl @And AND i vn
typeCheckInstr (Un.AND vn) (SomeHST i@((STc SCNat, _, _) ::&
                         (STc SCNat, _, _) ::& _)) = liftEither $ arithImpl @And AND i vn
typeCheckInstr (Un.AND vn) (SomeHST i@((STc SCBool, _, _) ::&
                         (STc SCBool, _, _) ::& _)) = liftEither $ arithImpl @And AND i vn

typeCheckInstr (Un.XOR vn) (SomeHST i@((STc SCBool, _, _) ::&
                         (STc SCBool, _, _) ::& _)) = liftEither $ arithImpl @Xor XOR i vn
typeCheckInstr (Un.XOR vn) (SomeHST i@((STc SCNat, _, _) ::&
                         (STc SCNat, _, _) ::& _)) = liftEither $ arithImpl @Xor XOR i vn

typeCheckInstr (Un.NOT vn) (SomeHST i@((STc SCNat, _, _) ::& _)) =
  liftEither $ unaryArithImpl @Not NOT i vn
typeCheckInstr (Un.NOT vn) (SomeHST i@((STc SCBool, _, _) ::& _)) =
  liftEither $ unaryArithImpl @Not NOT i vn
typeCheckInstr (Un.NOT vn) (SomeHST i@((STc SCInt, _, _) ::& _)) =
  liftEither $ unaryArithImpl @Not NOT i vn

typeCheckInstr (Un.COMPARE vn) (SomeHST i@((STc a, _, _) ::&
                                 (STc b, _, _) ::& _)) = liftEither $ compareImpl a b i vn

typeCheckInstr (Un.EQ vn) (SomeHST i@((STc SCInt, _, _) ::& _)) =
  liftEither $ unaryArithImpl @Eq' EQ i vn

typeCheckInstr (Un.NEQ vn) (SomeHST i@((STc SCInt, _, _) ::& _)) =
  liftEither $ unaryArithImpl @Neq NEQ i vn

typeCheckInstr (Un.LT vn) (SomeHST i@((STc SCInt, _, _) ::& _)) =
  liftEither $ unaryArithImpl @Lt LT i vn

typeCheckInstr (Un.GT vn) (SomeHST i@((STc SCInt, _, _) ::& _)) =
  liftEither $ unaryArithImpl @Gt GT i vn

typeCheckInstr (Un.LE vn) (SomeHST i@((STc SCInt, _, _) ::& _)) =
  liftEither $ unaryArithImpl @Le LE i vn

typeCheckInstr (Un.GE vn) (SomeHST i@((STc SCInt, _, _) ::& _)) =
  liftEither $ unaryArithImpl @Ge GE i vn

typeCheckInstr (Un.INT vn) (SomeHST i@((STc SCNat, _, _) ::& rs)) =
  pure $ INT ::: (i, (STc SCInt, NStar, vn) ::& rs)

typeCheckInstr instr@(Un.SELF vn) shst@(SomeHST i) = do
  cpType <- gets tcContractParam
  let t = fromUType cpType
  withSomeSingT t $ \(singcp :: Sing cp) -> do
    nt <- liftEither $ extractNotes cpType singcp `onLeft` TCFailedOnInstr instr shst
    pure $ SELF @cp ::: (i, (sing @('TContract cp), N (NTContract Un.noAnn nt), vn) ::& i)

typeCheckInstr instr@(Un.CONTRACT vn mt)
           (SomeHST i@((STc SCAddress, _, _) ::& rs)) =
  withSomeSingT (fromUType mt) $ \t -> liftEither $ do
    tns <- extractNotes mt t
            `onLeft` TCFailedOnInstr instr (SomeHST i)
    let ns = mkNotes $ NTOption def def $ mkNotes $ NTContract def tns
    pure $ CONTRACT ::: (i, (STOption $ STContract t, ns, vn) ::& rs)

typeCheckInstr instr@(Un.TRANSFER_TOKENS vn) (SomeHST i@(((_ :: Sing p'), _, _)
  ::& (STc SCMutez, _, _) ::& (STContract (_ :: Sing p), _, _) ::& rs)) = do
  case eqT' @p @p' of
    Right Refl ->
      pure $ TRANSFER_TOKENS ::: (i, (STOperation, NStar, vn) ::& rs)
    Left m ->
      typeCheckInstrErrM instr (SomeHST i) $ "mismatch of contract param type: " <> m

typeCheckInstr (Un.SET_DELEGATE vn)
           (SomeHST i@((STOption (STc SCKeyHash), _, _) ::& rs)) = do
  pure $ SET_DELEGATE ::: (i, (STOperation, NStar, vn) ::& rs)

typeCheckInstr (Un.CREATE_ACCOUNT ovn avn)
           (SomeHST i@((STc SCKeyHash, _, _)
             ::& (STOption (STc SCKeyHash), _, _) ::& (STc SCBool, _, _)
             ::& (STc SCMutez, _, _) ::& rs)) =
  pure $ CREATE_ACCOUNT ::: (i, (STOperation, NStar, ovn) ::&
                                 (STc SCAddress, NStar, avn) ::& rs)

typeCheckInstr instr@(Un.CREATE_CONTRACT ovn avn)
           (SomeHST i@((STc SCKeyHash, _, _)
             ::& (STOption (STc SCKeyHash), _, _)
             ::& (STc SCBool, _, _) ::& (STc SCBool, _, _)
             ::& (STc SCMutez, _, _)
             ::& (STLambda (STPair _ (_ :: Sing g1))
                   (STPair (STList STOperation) (_ :: Sing g2)), ln, _)
                        ::& ((_ :: Sing g3), gn3, _) ::& rs)) = do
  let (gn1, gn2) = notesCase (NStar, NStar)
                    (\(NTLambda _ l r) ->
                      (,) (notesCase NStar (\(NTPair _ _ _ _ n) -> n) l)
                          (notesCase NStar (\(NTPair _ _ _ _ n) -> n) r)) ln
  liftEither $ either (\m -> typeCheckInstrErr instr (SomeHST i) $
                  "mismatch of contract storage type: " <> m) pure $ do
    Refl <- eqT' @g1 @g2
    Refl <- eqT' @g2 @g3
    gn12 <- converge gn1 gn2
    _ <- converge gn12 gn3
    pure $ CREATE_CONTRACT ::: (i, (STOperation, NStar, ovn) ::&
                                     (STc SCAddress, NStar, avn) ::& rs)

typeCheckInstr instr@(Un.CREATE_CONTRACT2 ovn avn contract)
           (SomeHST i@((STc SCKeyHash, _, _)
             ::& (STOption (STc SCKeyHash), _, _)
             ::& (STc SCBool, _, _)
             ::& (STc SCBool, _, _)
             ::& (STc SCMutez, _, _)
             ::& ((_ :: Sing g), gn, _) ::& rs)) = do
  (SomeContract (contr :: Contract i' g') _ out) <-
      flip withExceptT (typeCheckContractImpl $ fmap Un.unOp contract)
                       (\err -> TCFailedOnInstr instr (SomeHST i)
                                    ("failed to type check contract: " <> show err))
  liftEither $ do
    Refl <- checkEqT @g @g' instr i "contract storage type mismatch"
    void $ converge gn (outNotes out) `onLeft` TCFailedOnInstr instr (SomeHST i)
    pure $ CREATE_CONTRACT2 contr ::: (i, (STOperation, NStar, ovn) ::&
                                          (STc SCAddress, NStar, avn) ::& rs)
  where
    outNotes :: HST '[ 'TPair ('TList 'TOperation) g' ] -> Notes g'
    outNotes ((_, n, _) ::& SNil) =
      notesCase NStar (\(NTPair _ _ _ _ n') -> n') n

typeCheckInstr (Un.IMPLICIT_ACCOUNT vn)
           (SomeHST i@((STc SCKeyHash, _, _) ::& rs)) =
  pure $ IMPLICIT_ACCOUNT ::: (i, (STContract STUnit, NStar, vn) ::& rs)

typeCheckInstr (Un.NOW vn) (SomeHST i) =
  pure $ NOW ::: (i, (STc SCTimestamp, NStar, vn) ::& i)

typeCheckInstr (Un.AMOUNT vn) (SomeHST i) =
  pure $ AMOUNT ::: (i, (STc SCMutez, NStar, vn) ::& i)

typeCheckInstr (Un.BALANCE vn) (SomeHST i) =
  pure $ BALANCE ::: (i, (STc SCMutez, NStar, vn) ::& i)

typeCheckInstr (Un.CHECK_SIGNATURE vn)
           (SomeHST i@((STKey, _, _)
             ::& (STSignature, _, _) ::& (STc SCBytes, _, _) ::& rs)) =
  pure $ CHECK_SIGNATURE ::: (i, (STc SCBool, NStar, vn) ::& rs)

typeCheckInstr (Un.SHA256 vn)
           (SomeHST i@((STc SCBytes, _, _) ::& rs)) =
  pure $ SHA256 ::: (i, (STc SCBytes, NStar, vn) ::& rs)

typeCheckInstr (Un.SHA512 vn)
           (SomeHST i@((STc SCBytes, _, _) ::& rs)) =
  pure $ SHA512 ::: (i, (STc SCBytes, NStar, vn) ::& rs)

typeCheckInstr (Un.BLAKE2B vn)
           (SomeHST i@((STc SCBytes, _, _) ::& rs)) =
  pure $ BLAKE2B ::: (i, (STc SCBytes, NStar, vn) ::& rs)

typeCheckInstr (Un.HASH_KEY vn)
           (SomeHST i@((STKey, _, _) ::& rs)) =
  pure $ HASH_KEY ::: (i, (STc SCKeyHash, NStar, vn) ::& rs)

typeCheckInstr (Un.STEPS_TO_QUOTA vn) (SomeHST i) =
  pure $ STEPS_TO_QUOTA ::: (i, (STc SCNat, NStar, vn) ::& i)

typeCheckInstr (Un.SOURCE vn) (SomeHST i) =
  pure $ SOURCE ::: (i, (STc SCAddress, NStar, vn) ::& i)

typeCheckInstr (Un.SENDER vn) (SomeHST i) =
  pure $ SENDER ::: (i, (STc SCAddress, NStar, vn) ::& i)

typeCheckInstr (Un.ADDRESS vn) (SomeHST i@((STContract _, _, _) ::& rs)) =
  pure $ ADDRESS ::: (i, (STc SCAddress, NStar, vn) ::& rs)

typeCheckInstr instr sit = typeCheckInstrErrM instr sit ""

-- | Helper function for two-branch if where each branch is given a single
-- value.
genericIf
  :: forall bti bfi cond rs .
    (Typeable bti, Typeable bfi, ExtC)
  => (forall s'.
        Instr bti s' ->
        Instr bfi s' ->
        Instr (cond ': rs) s'
     )
  -> ([Un.Op] -> [Un.Op] -> Un.Instr)
  -> [Un.Op]
  -> [Un.Op]
  -> HST bti
  -> HST bfi
  -> HST (cond ': rs)
  -> TypeCheckT SomeInstr
genericIf cons mCons mbt mbf bti bfi i@(_ ::& _) =
  liftA2 (,) (typeCheckList (Un.unOp <$> mbt) (SomeHST bti))
             (typeCheckList (Un.unOp <$> mbf) (SomeHST bfi)) >>= liftEither . \case
  (p ::: ((_ :: HST pi), (po :: HST po)), q ::: ((_ :: HST qi), (qo :: HST qo))) -> do
    Refl <- assertEqT @bti @pi instr i
    Refl <- assertEqT @bfi @qi instr i
    Refl <- checkEqT @qo @po instr i
                  "branches have different output stack types"
    o <- convergeHST po qo `onLeft` TCFailedOnInstr instr (SomeHST i)
    pure $ cons p q ::: (i, o)
  (SiFail, q ::: ((_ :: HST qi), (qo :: HST qo))) -> do
    Refl <- assertEqT @bfi @qi instr i
    -- TODO TM-27 There shall be no `PUSH VUnit`, rewrite this code
    pure $ cons (PUSH VUnit # FAILWITH) q ::: (i, qo)
  (p ::: ((_ :: HST pi), (po :: HST po)), SiFail) -> do
    Refl <- assertEqT @bti @pi instr i
    -- TODO TM-27 There shall be no `PUSH VUnit`, rewrite this code
    pure $ cons p (PUSH VUnit # FAILWITH) ::: (i, po)
  _ -> pure SiFail

  where
    instr = mCons mbt mbf

mapImpl
  :: forall b c rs .
  ( MapOp c b
  , Typeable b
  , Typeable (MapOpInp c)
  , Typeable (MapOpRes c b)
  )
  => Un.Instr
  -> SomeInstr
  -> HST (c ': rs)
  -> (forall v' . (Typeable v', SingI v') =>
        Sing v' -> Notes v' -> HST rs -> HST (MapOpRes c v' ': rs))
  -> TypeCheckT SomeInstr
mapImpl instr someInstr i@(_ ::& _) mkRes =
  case someInstr of
    SiFail -> pure SiFail
    sub ::: ((_ :: HST subi), (subo :: HST subo)) -> liftEither $ do
      Refl <- assertEqT @subi @(MapOpInp c ': rs) instr i
      case SomeHST subo of
        SomeHST ((b :: Sing b', bn, _bvn) ::& (rs' :: HST rs') :: HST subo') -> do
          Refl <- assertEqT @b @b' instr i
          Refl <- assertEqT @subo @subo' instr i
          Refl <- checkEqT @rs @rs' instr i $
                      "iteration expression has wrong output stack type"
          pure $ MAP sub ::: (i, mkRes b bn rs')
        _ -> typeCheckInstrErr instr (SomeHST i) $
              "iteration expression has wrong output stack type (empty stack)"

iterImpl
  :: forall c rs .
    ( IterOp c
    , SingI (IterOpEl c)
    , Typeable (IterOpEl c)
    , ExtC
    )
  => Sing (IterOpEl c)
  -> Notes (IterOpEl c)
  -> Un.Instr
  -> NonEmpty Un.Op
  -> HST (c ': rs)
  -> TypeCheckT SomeInstr
iterImpl et en instr mp i@((_, _, lvn) ::& rs) = do
  let evn = deriveVN "elt" lvn
  typeCheckNE (fmap Un.unOp mp) (SomeHST ((et, en, evn) ::& rs)) >>= \case
    SiFail -> pure SiFail
    subI ::: ((_ :: HST i), (o :: HST o)) -> liftEither $ do
      Refl <- assertEqT @i @(IterOpEl c ': rs) instr i
      Refl <- checkEqT @o @rs instr i
                "iteration expression has wrong output stack type"
      pure $ ITER subI ::: (i, o)

lamImpl
  :: forall it ot ts .
    ( Typeable it, Typeable ts, Typeable ot
    , ExtC
    , SingI it, SingI ot
    )
  => Un.Instr
  -> [Un.Op]  -> VarAnn
  -> Sing it -> Notes it
  -> Sing ot -> Notes ot
  -> HST ts
  -> TypeCheckT SomeInstr
lamImpl instr is vn it ins ot ons i = do
  typeCheckList (fmap Un.unOp is) (SomeHST $ (it, ins, def) ::& SNil) >>=
    \case
      SiFail -> pure SiFail
      lam ::: ((_ :: HST li), (lo :: HST lo)) -> liftEither $ do
        Refl <- assertEqT @'[ it ] @li instr i
        case (eqT' @'[ ot ] @lo, SomeHST lo) of
          (Right Refl, SomeHST ((_, ons', _) ::& SNil :: HST lo')) -> do
              Refl <- assertEqT @lo @lo' instr i
              onsr <- converge ons ons'
                        `onLeft` TCFailedOnInstr instr (SomeHST i)
              let ns = mkNotes $ NTLambda def ins onsr
              pure (LAMBDA (VLam lam) ::: (i, (STLambda it ot, ns, vn) ::& i))
          (Right Refl, _) ->
            typeCheckInstrErr instr (SomeHST i)
              "wrong output type of lambda's expression (wrong stack size)"
          (Left m, _) -> typeCheckInstrErr instr (SomeHST i) $
                          "wrong output type of lambda's expression: " <> m
