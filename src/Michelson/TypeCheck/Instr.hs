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
-- Functions @typeCheckInstr@, @typeCheckValue@ behave similarly.
--
-- When a recursive call is made within @typeCheck@, @typeCheckInstr@ or
-- @typeCheckValue@, result of a call is unwrapped from @SomeInstr@ and type
-- information from @HST inp@ and @HST out@ is being used to assert that
-- recursive call returned instruction of expected type
-- (error is thrown otherwise).
module Michelson.TypeCheck.Instr
    ( typeCheckContract
    , typeCheckValue
    , typeCheckList
    ) where

import Prelude hiding (EQ, GT, LT)

import Control.Monad.Except (liftEither, throwError, withExceptT)
import Data.Constraint (Dict(..))
import Data.Default (def)
import Data.Generics (everything, mkQ)
import Data.Singletons (SingI(sing))
import Data.Typeable ((:~:)(..))

import Michelson.TypeCheck.Helpers
import Michelson.TypeCheck.Types
import Michelson.TypeCheck.Value

import Michelson.Typed
  (Abs, And, CT(..), Contract, ContractInp, ContractOut, Eq', Ge, Gt, Instr(..), IterOp(..), Le,
  Lsl, Lsr, Lt, MapOp(..), Neg, Neq, Not, Notes(..), Notes'(..), Or, Sing(..), T(..), Value'(..),
  Xor, converge, convergeAnns, extractNotes, fromUType, mkNotes, notesCase, opAbsense, orAnn,
  withSomeSingCT, withSomeSingT, ( # ))

import qualified Michelson.Typed as T
import qualified Michelson.Untyped as U
import Michelson.Untyped.Annotation (VarAnn)

typeCheckContract
  :: ExtC
  => TcExtHandler
  -> TcOriginatedContracts
  -> U.Contract
  -> Either TCError SomeContract
typeCheckContract nh cs c = runTypeCheckT nh (U.para c) cs $ typeCheckContractImpl c

typeCheckContractImpl
  :: ExtC
  => U.Contract
  -> TypeCheckT SomeContract
typeCheckContractImpl (U.Contract mParam mStorage pCode) = do
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
      Dict <-
        liftEither . maybeToRight (TCOtherError $ hasOpError "parameter") $
        opAbsense paramS
      Dict <-
        liftEither . maybeToRight (TCOtherError $ hasOpError "storage") $
        opAbsense storageS
      let inpNote = mkNotes (NTPair def def def paramNote storageNote)
      let inp = (STPair paramS storageS, inpNote, def) ::& SNil
      typeCheckNE code inp >>= \case
        SiFail -> do
          let outNote = mkNotes (NTPair def def def NStar storageNote)
              out = (STPair (STList STOperation) storageS, outNote, def)
                      ::& SNil
          pure $ SomeContract (FAILWITH :: Instr (ContractInp param st) (ContractOut st)) inp out
        instr ::: (inp', (out :: HST out)) -> do
          let mkOErr m = TCOtherError $
                          "contract output type violates convention: " <> m
          liftEither $ do
            Refl <- eqT' @out @(ContractOut st) `onLeft` mkOErr
            let outN = outNotes out
            _ <- converge outN (N $ NTPair def def def NStar storageNote)
                    `onLeft` mkOErr
            pure $ SomeContract instr inp' out
  where
    outNotes :: HST '[o] -> Notes o
    outNotes ((_, n, _) ::& SNil) = n
    hasOpError name = "contract " <> name <> " type cannot contain operations"

-- | Like 'typeCheck', but for non-empty lists.
typeCheckNE
  :: (ExtC, Typeable inp)
  => NonEmpty U.ExpandedOp
  -> HST inp
  -> TypeCheckT (SomeInstr inp)
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
  :: (ExtC, Typeable inp)
  => [U.ExpandedOp]
  -> HST inp
  -> TypeCheckT (SomeInstr inp)
typeCheckList = typeCheckImpl typeCheckInstr

-- | Function @typeCheckValue@ converts a single Michelson value
-- given in representation from @Michelson.Type@ module to representation
-- in strictly typed GADT.
--
-- As a second argument, @typeCheckValue@ accepts expected type of value.
--
-- Type checking algorithm pattern-matches on parse value representation,
-- expected type @t@ and constructs @Val t@ value.
--
-- If there was no match on a given pair of value and expected type,
-- that is interpreted as input of wrong type and type check finishes with
-- error.
typeCheckValue
  :: ExtC
  => U.Value
  -> (Sing t, Notes t)
  -> TypeCheckT SomeValue
typeCheckValue = typeCheckValImpl typeCheckInstr

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
typeCheckInstr (U.EXT ext) si = do
  nh <- gets tcExtHandler
  nfs <- gets tcExtFrames
  (nfs', res) <- nh ext nfs (SomeHST si)
  modify $ \te -> te {tcExtFrames = nfs'}
  case res of
    Just tExt -> pure $ Ext tExt ::: (si, si)
    Nothing   -> pure $ Nop ::: (si, si)

typeCheckInstr U.DROP (i@(_ ::& rs)) = pure (DROP ::: (i, rs))

typeCheckInstr (U.DUP _vn) i@(a ::& rs) =
  pure (DUP ::: (i, (a ::& a::& rs)))

typeCheckInstr U.SWAP (i@(a ::& b ::& rs)) =
  pure (SWAP ::: (i, b ::& a ::& rs))

typeCheckInstr instr@(U.PUSH vn mt mval) i =
  withSomeSingT (fromUType mt) $ \t' -> do
    nt' <- liftEither $ extractNotes mt t' `onLeft` TCFailedOnInstr instr (SomeHST i)
    val :::: (t :: Sing t, nt) <- typeCheckValue mval (t', nt')
    let failure = TCFailedOnInstr instr (SomeHST i)
    proof <- maybe (throwError $ failure "Operations in constant are not allowed")
            pure (opAbsense t)
    case proof of
      Dict -> pure $ PUSH val ::: (i, (t, nt, vn) ::& i)

typeCheckInstr (U.SOME tn vn fn) i@((at, an, _) ::& rs) = do
  let n = mkNotes (NTOption tn fn an)
  pure (SOME ::: (i, (STOption at, n, vn) ::& rs))

typeCheckInstr instr@(U.NONE tn vn fn elMt) i = do
  withSomeSingT (fromUType elMt) $ \elT -> do
    let t = STOption elT
    notes <- liftEither $ extractNotes (U.Type (U.TOption fn elMt) tn) t
              `onLeft` TCFailedOnInstr instr (SomeHST i)
    pure $ NONE ::: (i, (t, notes, vn) ::& i)

typeCheckInstr (U.UNIT tn vn) i = do
  let ns = mkNotes $ NTUnit tn
  pure $ UNIT ::: (i, (STUnit, ns, vn) ::& i)

typeCheckInstr (U.IF_NONE mp mq) i@((STOption a, ons, ovn) ::& rs) = do
  let (an, avn) = deriveNsOption ons ovn
  genericIf IF_NONE U.IF_NONE mp mq rs ((a, an, avn) ::& rs) i

typeCheckInstr (U.PAIR tn vn pfn qfn) i@((a, an, avn) ::&
                                             (b, bn, bvn) ::& rs) = do
  let (vn', pfn', qfn') = deriveSpecialFNs pfn qfn avn bvn
      ns = mkNotes $ NTPair tn pfn' qfn' an bn
  pure (PAIR ::: (i, (STPair a b, ns, vn `orAnn` vn') ::& rs))

typeCheckInstr (U.CAR vn _) i@((STPair a _, NStar, _) ::& rs) =
  pure (CAR ::: (i, (a, NStar, vn) ::& rs))
typeCheckInstr instr@(U.CAR vn fn)
            (i@(( STPair a b
                       , N (NTPair pairTN pfn qfn pns qns)
                       , pairVN ) ::& rs)) = do
  pfn' <- liftEither $ convergeAnns fn pfn
              `onLeft` TCFailedOnInstr instr (SomeHST i)
  let vn' = deriveSpecialVN vn pfn' pairVN
      i' = ( STPair a b
            , N (NTPair pairTN pfn' qfn pns qns)
            , pairVN ) ::& rs
  pure $ CAR ::: (i', (a, pns, vn') ::& rs)

typeCheckInstr (U.CDR vn _) i@((STPair _ b, NStar, _) ::& rs) =
  pure (CDR ::: (i, (b, NStar, vn) ::& rs))
typeCheckInstr instr@(U.CDR vn fn)
          (i@(( STPair a b
                      , N (NTPair pairTN pfn qfn pns qns)
                      , pairVN ) ::& rs)) = do
  qfn' <- liftEither $ convergeAnns fn qfn
              `onLeft` TCFailedOnInstr instr (SomeHST i)
  let vn' = deriveSpecialVN vn qfn' pairVN
      i' = ( STPair a b
            , N (NTPair pairTN pfn qfn' pns qns)
            , pairVN ) ::& rs
  pure $ CDR ::: (i', (b, qns, vn') ::& rs)

typeCheckInstr instr@(U.LEFT tn vn pfn qfn bMt) i@((a, an, _) ::& rs) =
  withSomeSingT (fromUType bMt) $ \b -> do
    bn <- liftEither $ extractNotes bMt b
              `onLeft` TCFailedOnInstr instr (SomeHST i)
    let ns = mkNotes $ NTOr tn pfn qfn an bn
    pure (LEFT ::: (i, (STOr a b, ns, vn) ::& rs))

typeCheckInstr instr@(U.RIGHT tn vn pfn qfn aMt) i@((b, bn, _) ::& rs) =
  withSomeSingT (fromUType aMt) $ \a -> do
    an <- liftEither $ extractNotes aMt a
              `onLeft` TCFailedOnInstr instr (SomeHST i)
    let ns = mkNotes $ NTOr tn pfn qfn an bn
    pure (RIGHT ::: (i, (STOr a b, ns, vn) ::& rs))

typeCheckInstr (U.IF_LEFT mp mq) i@((STOr a b, ons, ovn) ::& rs) = do
  let (an, bn, avn, bvn) = deriveNsOr ons ovn
      ait = (a, an, avn) ::& rs
      bit = (b, bn, bvn) ::& rs
  genericIf IF_LEFT U.IF_LEFT mp mq ait bit i

typeCheckInstr instr@(U.NIL tn vn elMt) i =
  withSomeSingT (fromUType elMt) $ \elT -> liftEither $ do
    let t = STList elT
    notes <- extractNotes (U.Type (U.TList elMt) tn) t
              `onLeft` TCFailedOnInstr instr (SomeHST i)
    pure $ NIL ::: (i, (t, notes, vn) ::& i)

typeCheckInstr instr@(U.CONS vn) i@((((at :: Sing a), an, _)
                              ::& (STList (_ :: Sing a'), ln, _) ::& rs)) =
  case eqT' @a @a' of
    Right Refl -> liftEither $  do
      n <- converge ln (mkNotes $ NTList def an)
              `onLeft` TCFailedOnInstr instr (SomeHST i)
      pure $ CONS ::: (i, (STList at, n, vn) ::& rs)
    Left m -> typeCheckInstrErrM instr (SomeHST i) $
                "list element type is different from one "
                <> "that is being CONSed: " <> m


typeCheckInstr (U.IF_CONS mp mq) i@((STList a, ns, vn) ::& rs)  = do
  let an = notesCase NStar (\(NTList _ an_) -> an_) ns
      ait =
        (a, an, vn <> "hd") ::& (STList a, ns, vn <> "tl") ::& rs
  genericIf IF_CONS U.IF_CONS mp mq ait rs i

typeCheckInstr (U.SIZE vn) i@((STList _, _, _) ::& _)  = liftEither $ sizeImpl i vn
typeCheckInstr (U.SIZE vn) i@((STSet _, _, _) ::& _)  = liftEither $ sizeImpl i vn
typeCheckInstr (U.SIZE vn) i@((STMap _ _, _, _) ::& _)  = liftEither $ sizeImpl i vn
typeCheckInstr (U.SIZE vn) i@((STc SCString, _, _) ::& _)  = liftEither $  sizeImpl i vn
typeCheckInstr (U.SIZE vn) i@((STc SCBytes, _, _) ::& _)  = liftEither $ sizeImpl i vn

typeCheckInstr (U.EMPTY_SET tn vn (U.Comparable mk ktn)) i =
  withSomeSingCT mk $ \k ->
    pure $ EMPTY_SET ::: (i, (STSet k, mkNotes $ NTSet tn ktn, vn) ::& i)

typeCheckInstr instr@(U.EMPTY_MAP tn vn (U.Comparable mk ktn) mv) i =
  withSomeSingT (fromUType mv) $ \v ->
  withSomeSingCT mk $ \k -> liftEither $ do
    vns <- extractNotes mv v
              `onLeft` TCFailedOnInstr instr (SomeHST i)
    let ns = mkNotes $ NTMap tn ktn vns
    pure $ EMPTY_MAP ::: (i, (STMap k v, ns, vn) ::& i)

typeCheckInstr instr@(U.MAP vn mp) i@((STList _, ns, _vn) ::& _) = do
  let vns = notesCase NStar (\(NTList _ v') -> v') ns
  mapImpl vns instr mp i
    (\rt rn -> (::&) (STList rt, mkNotes $ NTList def rn, vn))

typeCheckInstr instr@(U.MAP vn mp) i@((STMap k _, ns, _vn) ::& _)  = do
  let (kns, vns) = notesCase (def, NStar) (\(NTMap _ k' v') -> (k', v')) ns
      pns = mkNotes $ NTPair def def def (mkNotes $ NTc kns) vns
  mapImpl pns instr mp i
    (\rt rn -> (::&) (STMap k rt, mkNotes $ NTMap def kns rn, vn))

-- case `U.HSTER []` is wrongly typed by definition
-- (as it is required to at least drop an element), so we don't consider it

typeCheckInstr instr@(U.ITER (i1 : ir)) i@((STSet _, n, _) ::& _) = do
  let en = notesCase NStar (\(NTSet _ en_) -> mkNotes $ NTc en_) n
  iterImpl en instr (i1 :| ir) i
typeCheckInstr instr@(U.ITER (i1 : ir)) (i@((STList _, n, _) ::& _)) = do
  let en = notesCase NStar (\(NTList _ en_) -> en_) n
  iterImpl en instr (i1 :| ir) i
typeCheckInstr instr@(U.ITER (i1 : ir)) (i@((STMap _ _, n, _) ::& _)) = do
  let en = notesCase NStar (\(NTMap _ kns vns) ->
              mkNotes $ NTPair def def def (mkNotes $ NTc kns) vns) n
  iterImpl en instr (i1 :| ir) i

typeCheckInstr instr@(U.MEM vn)
           i@((STc _, _, _) ::& (STSet _, _, _) ::& _) =
  liftEither $ memImpl instr i vn
typeCheckInstr instr@(U.MEM vn)
           i@((STc _, _, _) ::& (STMap _ _, _, _) ::& _) =
  liftEither $ memImpl instr i vn
typeCheckInstr instr@(U.MEM vn)
           i@((STc _, _, _) ::& (STBigMap _ _, _, _) ::& _) =
  liftEither $ memImpl instr i vn

typeCheckInstr instr@(U.GET vn) i@(_ ::& (STMap _ vt, cn, _) ::& _) =
  liftEither $ getImpl instr i vt (notesCase NStar (\(NTMap _ _ v) -> v) cn) vn
typeCheckInstr instr@(U.GET vn) i@(_ ::& (STBigMap _ vt, cn, _) ::& _) =
  liftEither $ getImpl instr i vt (notesCase NStar (\(NTBigMap _ _ v) -> v) cn) vn

typeCheckInstr instr@U.UPDATE i@(_ ::& _ ::& (STMap _ _, _, _) ::& _) =
  liftEither $ updImpl instr i
typeCheckInstr instr@U.UPDATE i@(_ ::& _ ::& (STBigMap _ _, _, _)
                                             ::& _) =
  liftEither $ updImpl instr i
typeCheckInstr instr@U.UPDATE i@(_ ::& _ ::& (STSet _, _, _) ::& _) =
  liftEither $ updImpl instr i

typeCheckInstr (U.IF mp mq) i@((STc SCBool, _, _) ::& rs)  =
  genericIf IF U.IF mp mq rs rs i

typeCheckInstr instr@(U.LOOP is)
           i@((STc SCBool, _, _) ::& (rs :: HST rs))  = do
  typeCheckList is rs >>= \case
    SiFail -> pure SiFail
    subI ::: ((_ :: HST rs'), (o :: HST o)) -> liftEither $ do
      case eqT' @o @('Tc 'CBool ': rs) of
        Right Refl -> do
          let _ ::& rs' = o
          pure $ LOOP subI ::: (i, rs')
        Left m ->
          typeCheckInstrErr instr (SomeHST i) $
                    "iteration expression has wrong output stack type: " <> m

typeCheckInstr instr@(U.LOOP_LEFT is)
           i@((STOr (at :: Sing a) (bt :: Sing b), ons, ovn)
                      ::& (rs :: HST rs))  = do
  let (an, bn, avn, bvn) = deriveNsOr ons ovn
      ait = (at, an, avn) ::& rs
  typeCheckList is ait >>= \case
    SiFail -> pure SiFail
    subI ::: (_, (o :: HST o)) -> liftEither $ do
      case (eqT' @o @('TOr a b ': rs), o) of
        (Right Refl, ((STOr _ bt', ons', ovn') ::& rs')) -> do
            let (_, bn', _, bvn') = deriveNsOr ons' ovn'
            br <- convergeHSTEl (bt, bn, bvn) (bt', bn', bvn')
                    `onLeft` TCFailedOnInstr instr (SomeHST i)
            pure $ LOOP_LEFT subI ::: (i, br ::& rs')
        (Left m, _) -> typeCheckInstrErr instr (SomeHST i) $
                        "iteration expression has wrong output stack type: " <> m

typeCheckInstr instr@(U.LAMBDA vn imt omt is) i = do
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

typeCheckInstr instr@(U.EXEC vn) i@(((_ :: Sing t1), _, _)
                              ::& (STLambda (_ :: Sing t1') t2, ln, _)
                              ::& rs) = do
  let t2n = notesCase NStar (\(NTLambda _ _ n) -> n) ln
  case eqT' @t1 @t1' of
    Right Refl -> pure $ EXEC ::: (i, (t2, t2n, vn) ::& rs)
    Left m -> typeCheckInstrErrM instr (SomeHST i) $
                "lambda is given argument with wrong type: " <> m

typeCheckInstr (U.DIP is) i@(a ::& (s :: HST s)) =
  typeCheckList is (s) >>= \case
    SiFail -> pure SiFail
    subI ::: ((_ :: HST s'), t) ->
      pure $ DIP subI ::: (i, a ::& t)

typeCheckInstr U.FAILWITH _ = pure SiFail

typeCheckInstr instr@(U.CAST vn mt)
           i@(((e :: Sing e), (en :: Notes e), evn) ::& rs) =
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

typeCheckInstr (U.RENAME vn) i@((at, an, _) ::& rs) =
  pure $ RENAME ::: (i, (at, an, vn) ::& rs)

typeCheckInstr instr@(U.UNPACK vn mt) i@((STc SCBytes, _, _) ::& rs) =
  withSomeSingT (fromUType mt) $ \t -> liftEither $ do
    tns <- extractNotes mt t
            `onLeft` TCFailedOnInstr instr (SomeHST i)
    let ns = mkNotes $ NTOption def def tns
    case opAbsense t of
      Just Dict ->
        pure $ UNPACK ::: (i, (STOption t, ns, vn) ::& rs)
      Nothing ->
        throwError $ TCFailedOnInstr instr (SomeHST i)
                      "Operations cannot appear in serialized data"

typeCheckInstr instr@(U.PACK vn) i@((a, _, _) ::& rs) = do
  case opAbsense a of
    Just Dict ->
      pure $ PACK ::: (i, (STc SCBytes, NStar, vn) ::& rs)
    Nothing ->
      throwError $ TCFailedOnInstr instr (SomeHST i)
                    "Operations in serialized data are not allowed"

typeCheckInstr (U.CONCAT vn) i@((STc SCBytes, _, _) ::&
                                    (STc SCBytes, _, _) ::& _) =
  liftEither $ concatImpl i vn
typeCheckInstr (U.CONCAT vn) i@((STc SCString, _, _) ::&
                                    (STc SCString, _, _) ::& _) =
  liftEither $ concatImpl i vn
typeCheckInstr (U.CONCAT vn) i@((STList (STc SCBytes), _, _) ::& _) =
  liftEither $  concatImpl' i vn
typeCheckInstr (U.CONCAT vn) i@((STList (STc SCString), _, _) ::& _) =
  liftEither $ concatImpl' i vn


typeCheckInstr (U.SLICE vn) i@((STc SCNat, _, _) ::&
                                   (STc SCNat, _, _) ::&
                                   (STc SCString, _, _) ::& _) =
  liftEither $ sliceImpl i vn
typeCheckInstr (U.SLICE vn) i@((STc SCNat, _, _) ::&
                                   (STc SCNat, _, _) ::&
                                   (STc SCBytes, _, _) ::& _) =
  liftEither $ sliceImpl i vn

typeCheckInstr (U.ISNAT vn') i@((STc SCInt, _, oldVn) ::& rs) = do
  let vn = vn' `orAnn` oldVn
  pure $ ISNAT ::: (i, (STOption (STc SCNat), NStar, vn) ::& rs)

typeCheckInstr (U.ADD vn) i@((STc a, _, _) ::&
                                 (STc b, _, _) ::& _) = liftEither $ addImpl a b i vn

typeCheckInstr (U.SUB vn) i@((STc a, _, _) ::&
                                 (STc b, _, _) ::& _) = liftEither $ subImpl a b i vn

typeCheckInstr (U.MUL vn) i@((STc a, _, _) ::&
                                 (STc b, _, _) ::& _) = liftEither $ mulImpl a b i vn

typeCheckInstr (U.EDIV vn) i@((STc a, _, _) ::&
                                  (STc b, _, _) ::& _) = liftEither $ edivImpl a b i vn

typeCheckInstr (U.ABS vn) i@((STc SCInt, _, _) ::& _) =
  liftEither $ unaryArithImpl @Abs ABS i vn

typeCheckInstr U.NEG (i@((STc SCInt, _, _) ::& _)) =
  liftEither $ unaryArithImpl @Neg NEG i def

typeCheckInstr (U.LSL vn) i@((STc SCNat, _, _) ::&
                         (STc SCNat, _, _) ::& _) = liftEither $ arithImpl @Lsl LSL i vn

typeCheckInstr (U.LSR vn) i@((STc SCNat, _, _) ::&
                         (STc SCNat, _, _) ::& _) = liftEither $ arithImpl @Lsr LSR i vn

typeCheckInstr (U.OR vn) i@((STc SCBool, _, _) ::&
                         (STc SCBool, _, _) ::& _) = liftEither $ arithImpl @Or OR i vn
typeCheckInstr (U.OR vn) i@((STc SCNat, _, _) ::&
                         (STc SCNat, _, _) ::& _) = liftEither $ arithImpl @Or OR i vn

typeCheckInstr (U.AND vn) i@((STc SCInt, _, _) ::&
                         (STc SCNat, _, _) ::& _) = liftEither $ arithImpl @And AND i vn
typeCheckInstr (U.AND vn) i@((STc SCNat, _, _) ::&
                         (STc SCNat, _, _) ::& _) = liftEither $ arithImpl @And AND i vn
typeCheckInstr (U.AND vn) i@((STc SCBool, _, _) ::&
                         (STc SCBool, _, _) ::& _) = liftEither $ arithImpl @And AND i vn

typeCheckInstr (U.XOR vn) i@((STc SCBool, _, _) ::&
                         (STc SCBool, _, _) ::& _) = liftEither $ arithImpl @Xor XOR i vn
typeCheckInstr (U.XOR vn) i@((STc SCNat, _, _) ::&
                         (STc SCNat, _, _) ::& _) = liftEither $ arithImpl @Xor XOR i vn

typeCheckInstr (U.NOT vn) i@((STc SCNat, _, _) ::& _) =
  liftEither $ unaryArithImpl @Not NOT i vn
typeCheckInstr (U.NOT vn) i@((STc SCBool, _, _) ::& _) =
  liftEither $ unaryArithImpl @Not NOT i vn
typeCheckInstr (U.NOT vn) i@((STc SCInt, _, _) ::& _) =
  liftEither $ unaryArithImpl @Not NOT i vn

typeCheckInstr (U.COMPARE vn) i@((STc a, _, _) ::&
                                 (STc b, _, _) ::& _) = liftEither $ compareImpl a b i vn

typeCheckInstr (U.EQ vn) i@((STc SCInt, _, _) ::& _) =
  liftEither $ unaryArithImpl @Eq' EQ i vn

typeCheckInstr (U.NEQ vn) i@((STc SCInt, _, _) ::& _) =
  liftEither $ unaryArithImpl @Neq NEQ i vn

typeCheckInstr (U.LT vn) i@((STc SCInt, _, _) ::& _) =
  liftEither $ unaryArithImpl @Lt LT i vn

typeCheckInstr (U.GT vn) i@((STc SCInt, _, _) ::& _) =
  liftEither $ unaryArithImpl @Gt GT i vn

typeCheckInstr (U.LE vn) i@((STc SCInt, _, _) ::& _) =
  liftEither $ unaryArithImpl @Le LE i vn

typeCheckInstr (U.GE vn) i@((STc SCInt, _, _) ::& _) =
  liftEither $ unaryArithImpl @Ge GE i vn

typeCheckInstr (U.INT vn) i@((STc SCNat, _, _) ::& rs) =
  pure $ INT ::: (i, (STc SCInt, NStar, vn) ::& rs)

typeCheckInstr instr@(U.SELF vn) shst@i = do
  cpType <- gets tcContractParam
  let t = fromUType cpType
  withSomeSingT t $ \(singcp :: Sing cp) -> do
    nt <- liftEither $ extractNotes cpType singcp `onLeft` TCFailedOnInstr instr (SomeHST shst)
    pure $ SELF @cp ::: (i, (sing @('TContract cp), mkNotes (NTContract U.noAnn nt), vn) ::& i)

typeCheckInstr instr@(U.CONTRACT vn mt)
           i@((STc SCAddress, _, _) ::& rs) =
  withSomeSingT (fromUType mt) $ \t -> liftEither $ do
    tns <- extractNotes mt t
            `onLeft` TCFailedOnInstr instr (SomeHST i)
    let ns = mkNotes $ NTOption def def $ mkNotes $ NTContract def tns
    pure $ CONTRACT tns ::: (i, (STOption $ STContract t, ns, vn) ::& rs)

typeCheckInstr instr@(U.TRANSFER_TOKENS vn) i@(((_ :: Sing p'), _, _)
  ::& (STc SCMutez, _, _) ::& (STContract (p :: Sing p), _, _) ::& rs) = do
  case (eqT' @p @p', opAbsense p) of
    (Right Refl, Just Dict) ->
      pure $ TRANSFER_TOKENS ::: (i, (STOperation, NStar, vn) ::& rs)
    (Left m, _) ->
      typeCheckInstrErrM instr (SomeHST i) $
        "mismatch of contract param type: " <> m
    (_, Nothing) ->
      typeCheckInstrErrM instr (SomeHST i) $
        "contract param type cannot contain operation"

typeCheckInstr (U.SET_DELEGATE vn)
           i@((STOption (STc SCKeyHash), _, _) ::& rs) = do
  pure $ SET_DELEGATE ::: (i, (STOperation, NStar, vn) ::& rs)

typeCheckInstr (U.CREATE_ACCOUNT ovn avn)
           i@((STc SCKeyHash, _, _)
             ::& (STOption (STc SCKeyHash), _, _) ::& (STc SCBool, _, _)
             ::& (STc SCMutez, _, _) ::& rs) =
  pure $ CREATE_ACCOUNT ::: (i, (STOperation, NStar, ovn) ::&
                                 (STc SCAddress, NStar, avn) ::& rs)

typeCheckInstr instr@(U.CREATE_CONTRACT ovn avn contract)
           i@((STc SCKeyHash, _, _)
             ::& (STOption (STc SCKeyHash), _, _)
             ::& (STc SCBool, _, _)
             ::& (STc SCBool, _, _)
             ::& (STc SCMutez, _, _)
             ::& ((_ :: Sing g), gn, _) ::& rs) = do
  (SomeContract (contr :: Contract p' g') _ out) <-
      flip withExceptT (typeCheckContractImpl contract)
                       (\err -> TCFailedOnInstr instr (SomeHST i)
                                    ("failed to type check contract: " <> show err))
  liftEither $ do
    Refl <- checkEqT @g @g' instr i "contract storage type mismatch"
    void $ converge gn (outNotes out) `onLeft` TCFailedOnInstr instr (SomeHST i)
    pure $ CREATE_CONTRACT contr ::: (i, (STOperation, NStar, ovn) ::&
                                          (STc SCAddress, NStar, avn) ::& rs)
  where
    outNotes :: HST '[ 'TPair ('TList 'TOperation) g' ] -> Notes g'
    outNotes ((_, n, _) ::& SNil) =
      notesCase NStar (\(NTPair _ _ _ _ n') -> n') n

typeCheckInstr (U.IMPLICIT_ACCOUNT vn)
           i@((STc SCKeyHash, _, _) ::& rs) =
  pure $ IMPLICIT_ACCOUNT ::: (i, (STContract STUnit, NStar, vn) ::& rs)

typeCheckInstr (U.NOW vn) i =
  pure $ NOW ::: (i, (STc SCTimestamp, NStar, vn) ::& i)

typeCheckInstr (U.AMOUNT vn) i =
  pure $ AMOUNT ::: (i, (STc SCMutez, NStar, vn) ::& i)

typeCheckInstr (U.BALANCE vn) i =
  pure $ BALANCE ::: (i, (STc SCMutez, NStar, vn) ::& i)

typeCheckInstr (U.CHECK_SIGNATURE vn)
           i@((STKey, _, _)
             ::& (STSignature, _, _) ::& (STc SCBytes, _, _) ::& rs) =
  pure $ CHECK_SIGNATURE ::: (i, (STc SCBool, NStar, vn) ::& rs)

typeCheckInstr (U.SHA256 vn)
           i@((STc SCBytes, _, _) ::& rs) =
  pure $ SHA256 ::: (i, (STc SCBytes, NStar, vn) ::& rs)

typeCheckInstr (U.SHA512 vn)
           i@((STc SCBytes, _, _) ::& rs) =
  pure $ SHA512 ::: (i, (STc SCBytes, NStar, vn) ::& rs)

typeCheckInstr (U.BLAKE2B vn)
           i@((STc SCBytes, _, _) ::& rs) =
  pure $ BLAKE2B ::: (i, (STc SCBytes, NStar, vn) ::& rs)

typeCheckInstr (U.HASH_KEY vn)
           i@((STKey, _, _) ::& rs) =
  pure $ HASH_KEY ::: (i, (STc SCKeyHash, NStar, vn) ::& rs)

typeCheckInstr (U.STEPS_TO_QUOTA vn) i =
  pure $ STEPS_TO_QUOTA ::: (i, (STc SCNat, NStar, vn) ::& i)

typeCheckInstr (U.SOURCE vn) i =
  pure $ SOURCE ::: (i, (STc SCAddress, NStar, vn) ::& i)

typeCheckInstr (U.SENDER vn) i =
  pure $ SENDER ::: (i, (STc SCAddress, NStar, vn) ::& i)

typeCheckInstr (U.ADDRESS vn) i@((STContract _, _, _) ::& rs) =
  pure $ ADDRESS ::: (i, (STc SCAddress, NStar, vn) ::& rs)

typeCheckInstr instr sit = typeCheckInstrErrM instr (SomeHST sit) ""

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
  -> ([U.ExpandedOp] -> [U.ExpandedOp] -> U.ExpandedInstr)
  -> [U.ExpandedOp]
  -> [U.ExpandedOp]
  -> HST bti
  -> HST bfi
  -> HST (cond ': rs)
  -> TypeCheckT (SomeInstr (cond ': rs))
genericIf cons mCons mbt mbf bti bfi i@(_ ::& _) =
  liftA2 (,) (typeCheckList mbt (bti))
             (typeCheckList mbf (bfi)) >>= liftEither . \case
  (p ::: ((_ :: HST pi), (po :: HST po)), q ::: ((_ :: HST qi), (qo :: HST qo))) -> do
    Refl <- checkEqT @qo @po instr i
                  "branches have different output stack types"
    o <- convergeHST po qo `onLeft` TCFailedOnInstr instr (SomeHST i)
    pure $ cons p q ::: (i, o)
  (SiFail, q ::: ((_ :: HST qi), (qo :: HST qo))) -> do
    -- TODO TM-27 There shall be no `PUSH VUnit`, rewrite this code
    pure $ cons (PUSH T.VUnit # FAILWITH) q ::: (i, qo)
  (p ::: ((_ :: HST pi), (po :: HST po)), SiFail) -> do
    -- TODO TM-27 There shall be no `PUSH VUnit`, rewrite this code
    pure $ cons p (PUSH T.VUnit # FAILWITH) ::: (i, po)
  _ -> pure SiFail

  where
    instr = mCons mbt mbf

mapImpl
  :: forall c rs .
    ( MapOp c
    , SingI (MapOpInp c)
    , Typeable (MapOpInp c)
    , Typeable (MapOpRes c)
    , ExtC
    )
  => Notes (MapOpInp c)
  -> U.ExpandedInstr
  -> [U.ExpandedOp]
  -> HST (c ': rs)
  -> (forall v' . (Typeable v', SingI v') =>
        Sing v' -> Notes v' -> HST rs -> HST (MapOpRes c v' ': rs))
  -> TypeCheckT (SomeInstr (c ': rs))
mapImpl vn instr mp i@(_ ::& rs) mkRes =
  typeCheckList mp ((sing, vn, def) ::& rs) >>= \case
    SiFail -> pure SiFail
    sub ::: (_, (subo :: HST subo)) -> liftEither $ do
      case subo of
        ((b :: Sing b, bn, _bvn) ::& (rs' :: HST rs') :: HST subo') -> do
          Refl <- checkEqT @rs @rs' instr i $
                      "map expression has changed not only top of the stack"
          pure $ MAP sub ::: (i, mkRes b bn rs')
        _ -> typeCheckInstrErr instr (SomeHST i) $
              "map expression has wrong output stack type (empty stack)"

iterImpl
  :: forall c rs .
    ( IterOp c
    , SingI (IterOpEl c)
    , Typeable (IterOpEl c)
    , ExtC
    )
  => Notes (IterOpEl c)
  -> U.ExpandedInstr
  -> NonEmpty U.ExpandedOp
  -> HST (c ': rs)
  -> TypeCheckT (SomeInstr (c ': rs))
iterImpl en instr mp i@((_, _, lvn) ::& rs) = do
  let evn = deriveVN "elt" lvn
  typeCheckNE mp (((sing, en, evn) ::& rs)) >>= \case
    SiFail -> pure SiFail
    subI ::: (_, (o :: HST o)) -> liftEither $ do
      Refl <- checkEqT @o @rs instr i
                "iteration expression has wrong output stack type"
      pure $ ITER subI ::: (i, o)

lamImpl
  :: forall it ot ts .
    ( Typeable it, Typeable ts, Typeable ot
    , ExtC
    , SingI it, SingI ot
    )
  => U.ExpandedInstr
  -> [U.ExpandedOp]
  -> VarAnn
  -> Sing it -> Notes it
  -> Sing ot -> Notes ot
  -> HST ts
  -> TypeCheckT (SomeInstr ts)
lamImpl instr is vn it ins ot ons i = do
  when (any hasSelf is) $ typeCheckInstrErrM instr (SomeHST i)
    "The SELF instruction cannot appear in a lambda"
  typeCheckList is ((it, ins, def) ::& SNil) >>=
    \case
      SiFail -> pure SiFail
      lam ::: (_, (lo :: HST lo)) -> liftEither $ do
        case eqT' @'[ ot ] @lo of
          Right Refl -> do
              let (_, ons', _) ::& SNil = lo
              onsr <- converge ons ons'
                        `onLeft` TCFailedOnInstr instr (SomeHST i)
              let ns = mkNotes $ NTLambda def ins onsr
              pure (LAMBDA (VLam lam) ::: (i, (STLambda it ot, ns, vn) ::& i))
          Left m -> typeCheckInstrErr instr (SomeHST i) $
                      "wrong output type of lambda's expression: " <> m
  where
    hasSelf :: U.ExpandedOp -> Bool
    hasSelf = everything (||)
      (mkQ False
       (\x -> case x of
           (U.SELF _ :: U.InstrAbstract U.ExpandedOp) -> True
           _ -> False
       )
      )
