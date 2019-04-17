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
import Data.Typeable ((:~:)(..), typeRep)

import Michelson.TypeCheck.Error
import Michelson.TypeCheck.Ext
import Michelson.TypeCheck.Helpers
import Michelson.TypeCheck.TypeCheck
  (TcInstrHandler, TcOriginatedContracts, TypeCheckEnv(..), TypeCheckT, runTypeCheckT)
import Michelson.TypeCheck.Types
import Michelson.TypeCheck.Value

import Michelson.Typed
  (Abs, And, CT(..), Contract, ContractOut, Eq', Ge, Gt, Instr(..), IterOp(..), Le, Lsl, Lsr, Lt,
  MapOp(..), Neg, Neq, Not, Notes(..), Notes'(..), Or, Sing(..), T(..), Value'(..), Xor, converge,
  convergeAnns, extractNotes, fromUType, mkNotes, notesCase, opAbsense, orAnn, withSomeSingCT,
  withSomeSingT)

import qualified Michelson.Untyped as U
import Michelson.Untyped.Annotation (VarAnn)

typeCheckContract
  :: TcOriginatedContracts
  -> U.Contract
  -> Either TCError SomeContract
typeCheckContract cs c = runTypeCheckT (U.para c) cs $ typeCheckContractImpl c

typeCheckContractImpl
  :: U.Contract
  -> TypeCheckT SomeContract
typeCheckContractImpl (U.Contract mParam mStorage pCode) = do
  code <- maybe (throwError $ TCContractError "no instructions in contract code" Nothing)
                pure (nonEmpty pCode)
  withSomeSingT (fromUType mParam) $ \(paramS :: Sing param) ->
    withSomeSingT (fromUType mStorage) $ \(storageS :: Sing st) -> do
      storageNote <-
        liftEither $ extractNotes mStorage storageS `onLeft`
        (TCContractError "failed to extract annotations for storage:" . Just . ExtractionTypeMismatch)
      paramNote <-
        liftEither $ extractNotes mParam paramS `onLeft`
        (TCContractError "failed to extract annotations for parameter:" . Just . ExtractionTypeMismatch)
      Dict <-
        liftEither . maybeToRight (hasOpError "parameter") $
        opAbsense paramS
      Dict <-
        liftEither . maybeToRight (hasOpError "storage") $
        opAbsense storageS
      let inpNote = mkNotes (NTPair def def def paramNote storageNote)
      let inp = (STPair paramS storageS, inpNote, def) ::& SNil
      inp' :/ instrOut <- typeCheckNE code inp
      case instrOut of
        instr ::: (out :: HST out) -> liftEither $ do
          case eqType @out @(ContractOut st) of
            Right Refl -> do
              let (_, outN, _) ::& SNil = out
              _ <- converge outN (N $ NTPair def def def NStar storageNote)
                      `onLeft`
                  ((TCContractError "contract output type violates convention:") . Just . AnnError)
              pure $ SomeContract instr inp' out
            Left err -> Left $ TCContractError "contract output type violates convention:" $ Just err
        AnyOutInstr instr -> do
          let outNote = mkNotes (NTPair def def def NStar storageNote)
          let out = (STPair (STList STOperation) storageS, outNote, def)
                      ::& SNil
          pure $ SomeContract instr inp' out
  where
    hasOpError name =
      TCContractError ("contract " <> name <> " type error") $
      Just $ UnsupportedTypes [typeRep (Proxy @'TOperation)]

-- | Like 'typeCheck', but for non-empty lists.
typeCheckNE
  :: (Typeable inp)
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
  :: (Typeable inp)
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
  :: U.Value
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
typeCheckInstr :: TcInstrHandler
typeCheckInstr (U.EXT ext) si = do
  nfs <- gets tcExtFrames
  (nfs', res) <- typeCheckExt typeCheckList ext nfs si
  modify $ \te -> te {tcExtFrames = nfs'}
  let nopOrExt = maybe Nop Ext res
  return $ si :/ nopOrExt ::: si

typeCheckInstr U.DROP i@(_ ::& rs) = pure (i :/ DROP ::: rs)

typeCheckInstr (U.DUP _vn) i@(a ::& rs) =
  pure (i :/ DUP ::: (a ::& a::& rs))

typeCheckInstr U.SWAP (i@(a ::& b ::& rs)) =
  pure (i :/ SWAP ::: (b ::& a ::& rs))

typeCheckInstr instr@(U.PUSH vn mt mval) i =
  withSomeSingT (fromUType mt) $ \t' -> do
    nt' <- liftEither $ extractNotes mt t' `onLeft`
      typeCheckInstrTypeErr instr i "wrong push type:"
    val :::: (t :: Sing t, nt) <- typeCheckValue mval (t', nt')
    let failure = \msg -> TCFailedOnInstr instr (SomeHST i) msg Nothing
    proof <- maybe (throwError $ failure "Operations in constant are not allowed")
            pure (opAbsense t)
    case proof of
      Dict -> pure $ i :/ PUSH val ::: ((t, nt, vn) ::& i)

typeCheckInstr (U.SOME tn vn fn) i@((at, an, _) ::& rs) = do
  let n = mkNotes (NTOption tn fn an)
  pure (i :/ SOME ::: ((STOption at, n, vn) ::& rs))

typeCheckInstr instr@(U.NONE tn vn fn elMt) i = do
  withSomeSingT (fromUType elMt) $ \elT -> do
    let t = STOption elT
    notes <- liftEither $ extractNotes (U.Type (U.TOption fn elMt) tn) t
              `onLeft` typeCheckInstrTypeErr instr i "wrong none type:"
    pure $ i :/ NONE ::: ((t, notes, vn) ::& i)

typeCheckInstr (U.UNIT tn vn) i = do
  let ns = mkNotes $ NTUnit tn
  pure $ i :/ UNIT ::: ((STUnit, ns, vn) ::& i)

typeCheckInstr (U.IF_NONE mp mq) i@((STOption a, ons, ovn) ::& rs) = do
  let (an, avn) = deriveNsOption ons ovn
  genericIf IF_NONE U.IF_NONE mp mq rs ((a, an, avn) ::& rs) i

typeCheckInstr (U.PAIR tn vn pfn qfn) i@((a, an, avn) ::&
                                             (b, bn, bvn) ::& rs) = do
  let (vn', pfn', qfn') = deriveSpecialFNs pfn qfn avn bvn
      ns = mkNotes $ NTPair tn pfn' qfn' an bn
  pure (i :/ PAIR ::: ((STPair a b, ns, vn `orAnn` vn') ::& rs))

typeCheckInstr (U.CAR vn _) i@((STPair a _, NStar, _) ::& rs) =
  pure (i :/ CAR ::: ((a, NStar, vn) ::& rs))
typeCheckInstr instr@(U.CAR vn fn)
            (i@(( STPair a b
                       , N (NTPair pairTN pfn qfn pns qns)
                       , pairVN ) ::& rs)) = do
  pfn' <- liftEither $ convergeAnns fn pfn
              `onLeft` typeCheckInstrAnnErr instr i "wrong car type:"
  let vn' = deriveSpecialVN vn pfn' pairVN
      i' = ( STPair a b
            , N (NTPair pairTN pfn' qfn pns qns)
            , pairVN ) ::& rs
  pure $ i' :/ CAR ::: ((a, pns, vn') ::& rs)

typeCheckInstr (U.CDR vn _) i@((STPair _ b, NStar, _) ::& rs) =
  pure (i :/ CDR ::: ((b, NStar, vn) ::& rs))
typeCheckInstr instr@(U.CDR vn fn)
          (i@(( STPair a b
                      , N (NTPair pairTN pfn qfn pns qns)
                      , pairVN ) ::& rs)) = do
  qfn' <- liftEither $ convergeAnns fn qfn
              `onLeft` typeCheckInstrAnnErr instr i "wrong cdr type:"

  let vn' = deriveSpecialVN vn qfn' pairVN
      i' = ( STPair a b
            , N (NTPair pairTN pfn qfn' pns qns)
            , pairVN ) ::& rs
  pure $ i' :/ CDR ::: ((b, qns, vn') ::& rs)

typeCheckInstr instr@(U.LEFT tn vn pfn qfn bMt) i@((a, an, _) ::& rs) =
  withSomeSingT (fromUType bMt) $ \b -> do
    bn <- liftEither $ extractNotes bMt b
              `onLeft` typeCheckInstrTypeErr instr i "wrong left type:"
    let ns = mkNotes $ NTOr tn pfn qfn an bn
    pure (i :/ LEFT ::: ((STOr a b, ns, vn) ::& rs))

typeCheckInstr instr@(U.RIGHT tn vn pfn qfn aMt) i@((b, bn, _) ::& rs) =
  withSomeSingT (fromUType aMt) $ \a -> do
    an <- liftEither $ extractNotes aMt a
              `onLeft` typeCheckInstrTypeErr instr i "wrong right type:"
    let ns = mkNotes $ NTOr tn pfn qfn an bn
    pure (i :/ RIGHT ::: ((STOr a b, ns, vn) ::& rs))

typeCheckInstr (U.IF_LEFT mp mq) i@((STOr a b, ons, ovn) ::& rs) = do
  let (an, bn, avn, bvn) = deriveNsOr ons ovn
      ait = (a, an, avn) ::& rs
      bit = (b, bn, bvn) ::& rs
  genericIf IF_LEFT U.IF_LEFT mp mq ait bit i

typeCheckInstr instr@(U.NIL tn vn elMt) i =
  withSomeSingT (fromUType elMt) $ \elT -> liftEither $ do
    let t = STList elT
    notes <- extractNotes (U.Type (U.TList elMt) tn) t
              `onLeft` typeCheckInstrTypeErr instr i "wrong nil type:"
    pure $ i :/ NIL ::: ((t, notes, vn) ::& i)

typeCheckInstr instr@(U.CONS vn) i@((((at :: Sing a), an, _)
                              ::& (STList (_ :: Sing a'), ln, _) ::& rs)) =
  case eqType @a @a' of
    Right Refl -> liftEither $  do
      n <- converge ln (mkNotes $ NTList def an)
              `onLeft` typeCheckInstrAnnErr instr i "wrong cons type:"
      pure $ i :/ CONS ::: ((STList at, n, vn) ::& rs)
    Left m -> typeCheckInstrErrM instr (SomeHST i)
                ("list element type is different from one "
                <> "that is being CONSed:") m


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
    pure $ i :/ EMPTY_SET ::: ((STSet k, mkNotes $ NTSet tn ktn, vn) ::& i)

typeCheckInstr instr@(U.EMPTY_MAP tn vn (U.Comparable mk ktn) mv) i =
  withSomeSingT (fromUType mv) $ \v ->
  withSomeSingCT mk $ \k -> liftEither $ do
    vns <- extractNotes mv v
              `onLeft` typeCheckInstrTypeErr instr i "wrong empty_map type:"
    let ns = mkNotes $ NTMap tn ktn vns
    pure $ i :/ EMPTY_MAP ::: ((STMap k v, ns, vn) ::& i)

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
  _ :/ tp <- typeCheckList is rs
  liftEither $ case tp of
    subI ::: (o :: HST o) -> liftEither $ do
      case eqType @o @('Tc 'CBool ': rs) of
        Right Refl -> do
          let _ ::& rs' = o
          pure $ i :/ LOOP subI ::: rs'
        Left m -> typeCheckInstrErr instr (SomeHST i)
                    "iteration expression has wrong output stack type:" m
    AnyOutInstr subI ->
      pure $ i :/ LOOP subI ::: rs

typeCheckInstr instr@(U.LOOP_LEFT is)
           i@((STOr (at :: Sing a) (bt :: Sing b), ons, ovn)
                      ::& (rs :: HST rs))  = do
  let (an, bn, avn, bvn) = deriveNsOr ons ovn
      ait = (at, an, avn) ::& rs
  _ :/ tp <- typeCheckList is ait
  liftEither $ case tp of
    subI ::: (o :: HST o) -> liftEither $ do
      case (eqType @o @('TOr a b ': rs), o) of
        (Right Refl, ((STOr _ bt', ons', ovn') ::& rs')) -> do
            let (_, bn', _, bvn') = deriveNsOr ons' ovn'
            br <- convergeHSTEl (bt, bn, bvn) (bt', bn', bvn')
                    `onLeft` typeCheckInstrAnnErr instr i "wrong LOOP_LEFT input type:"
            pure $ i :/ LOOP_LEFT subI ::: (br ::& rs')
        (Left m, _) -> typeCheckInstrErr instr (SomeHST i)
                        "iteration expression has wrong output stack type:" m
    AnyOutInstr subI -> do
      let br = (bt, bn, bvn)
      pure $ i :/ LOOP_LEFT subI ::: (br ::& rs)

typeCheckInstr instr@(U.LAMBDA vn imt omt is) i = do
  withSomeSingT (fromUType imt) $ \(it :: Sing it) -> do
    withSomeSingT (fromUType omt) $ \(ot :: Sing ot) -> do
      ins <- liftEither $ extractNotes imt it
              `onLeft` typeCheckInstrTypeErr instr i "wrong lambda input type:"
      ons <- liftEither $ extractNotes omt ot
              `onLeft` typeCheckInstrTypeErr instr i "wrong lambda output type:"
      -- further processing is extracted into another function because
      -- otherwise I encountered some weird GHC error with that code
      -- located right here
      lamImpl instr is vn it ins ot ons i

typeCheckInstr instr@(U.EXEC vn) i@(((_ :: Sing t1), _, _)
                              ::& (STLambda (_ :: Sing t1') t2, ln, _)
                              ::& rs) = do
  let t2n = notesCase NStar (\(NTLambda _ _ n) -> n) ln
  case eqType @t1 @t1' of
    Right Refl -> pure $ i :/ EXEC ::: ((t2, t2n, vn) ::& rs)
    Left m -> typeCheckInstrErrM instr (SomeHST i)
                "lambda is given argument with wrong type:" m

typeCheckInstr instr@(U.DIP is) i@(a ::& (s :: HST s)) = do
  _ :/ tp <- typeCheckList is s
  case tp of
    subI ::: t ->
      pure $ i :/ DIP subI ::: (a ::& t)
    AnyOutInstr _ ->
      -- This may seem like we throw error because of despair, but in fact,
      -- the reference implementation seems to behave exactly in this way -
      -- if output stack of code block within @DIP@ occurs to be any, an
      -- error "FAILWITH must be at tail position" is raised.
      throwError $
        TCFailedOnInstr instr (SomeHST i)
          "Code within DIP instruction always fails, which is not allowed" Nothing

typeCheckInstr U.FAILWITH i@(_ ::& _) =
  pure $ i :/ AnyOutInstr FAILWITH

typeCheckInstr instr@(U.CAST vn mt)
           i@(((e :: Sing e), (en :: Notes e), evn) ::& rs) =
  withSomeSingT (fromUType mt) $ \(_ :: Sing e') ->
    case eqType @e @e' of
      Right Refl ->
        case extractNotes mt e of
          Right en' ->
            case converge en en' of
              Right ns ->
                pure $ i :/ CAST ::: ((e, ns, vn `orAnn` evn) ::& rs)
              Left m -> (err . AnnError) m
          Left m -> (err . ExtractionTypeMismatch) m
      Left m -> err m
    where
      err = \m -> typeCheckInstrErrM instr (SomeHST i)
                  "cast to incompatible type:" m

typeCheckInstr (U.RENAME vn) i@((at, an, _) ::& rs) =
  pure $ i :/ RENAME ::: ((at, an, vn) ::& rs)

typeCheckInstr instr@(U.UNPACK vn mt) i@((STc SCBytes, _, _) ::& rs) =
  withSomeSingT (fromUType mt) $ \t -> liftEither $ do
    tns <- extractNotes mt t
            `onLeft` typeCheckInstrTypeErr instr i "wrong unpack type"
    let ns = mkNotes $ NTOption def def tns
    case opAbsense t of
      Just Dict ->
        pure $ i :/ UNPACK ::: ((STOption t, ns, vn) ::& rs)
      Nothing ->
        throwError $ TCFailedOnInstr instr (SomeHST i)
                      "Operations cannot appear in serialized data" Nothing

typeCheckInstr instr@(U.PACK vn) i@((a, _, _) ::& rs) = do
  case opAbsense a of
    Just Dict ->
      pure $ i :/ PACK ::: ((STc SCBytes, NStar, vn) ::& rs)
    Nothing ->
      throwError $ TCFailedOnInstr instr (SomeHST i)
                    "Operations in serialized data are not allowed" Nothing

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
  pure $ i :/ ISNAT ::: ((STOption (STc SCNat), NStar, vn) ::& rs)

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
  pure $ i :/ INT ::: ((STc SCInt, NStar, vn) ::& rs)

typeCheckInstr instr@(U.SELF vn) shst@i = do
  cpType <- gets tcContractParam
  let t = fromUType cpType
  withSomeSingT t $ \(singcp :: Sing cp) -> do
    nt <- liftEither $ extractNotes cpType singcp `onLeft`
      typeCheckInstrTypeErr instr shst "wrong self type"
    pure $ i :/ SELF @cp ::: ((sing @('TContract cp), N (NTContract U.noAnn nt), vn) ::& i)

typeCheckInstr instr@(U.CONTRACT vn mt)
           i@((STc SCAddress, _, _) ::& rs) =
  withSomeSingT (fromUType mt) $ \t -> liftEither $ do
    tns <- extractNotes mt t
            `onLeft` typeCheckInstrTypeErr instr i "wrong contract command type"
    let ns = mkNotes $ NTOption def def $ mkNotes $ NTContract def tns
    pure $ i :/ CONTRACT tns ::: ((STOption $ STContract t, ns, vn) ::& rs)

typeCheckInstr instr@(U.TRANSFER_TOKENS vn) i@(((_ :: Sing p'), _, _)
  ::& (STc SCMutez, _, _) ::& (STContract (p :: Sing p), _, _) ::& rs) = do
  case (eqType @p @p', opAbsense p) of
    (Right Refl, Just Dict) ->
      pure $ i :/ TRANSFER_TOKENS ::: ((STOperation, NStar, vn) ::& rs)
    (Left m, _) ->
      typeCheckInstrErrM instr (SomeHST i)
        "mismatch of contract param type:" m
    (_, Nothing) ->
      typeCheckInstrErrM instr (SomeHST i)
        "contract param type cannot contain operation:" $ UnsupportedTypes [typeRep (Proxy @p)]

typeCheckInstr (U.SET_DELEGATE vn)
           i@((STOption (STc SCKeyHash), _, _) ::& rs) = do
  pure $ i :/ SET_DELEGATE ::: ((STOperation, NStar, vn) ::& rs)

typeCheckInstr (U.CREATE_ACCOUNT ovn avn)
           i@((STc SCKeyHash, _, _)
             ::& (STOption (STc SCKeyHash), _, _) ::& (STc SCBool, _, _)
             ::& (STc SCMutez, _, _) ::& rs) =
  pure $ i :/ CREATE_ACCOUNT ::: ((STOperation, NStar, ovn) ::&
                                   (STc SCAddress, NStar, avn) ::& rs)

typeCheckInstr instr@(U.CREATE_CONTRACT ovn avn contract)
           i@((STc SCKeyHash, _, _)
             ::& (STOption (STc SCKeyHash), _, _)
             ::& (STc SCBool, _, _)
             ::& (STc SCBool, _, _)
             ::& (STc SCMutez, _, _)
             ::& ((_ :: Sing g), gn, _) ::& rs) = do
  (SomeContract (contr :: Contract p' g') _ out) <-
      flip withExceptT (typeCheckContractImpl contract) id
  liftEither $ do
    Refl <- checkEqT @g @g' instr i "contract storage type mismatch"
    void $ converge gn (outNotes out) `onLeft`
      typeCheckInstrAnnErr instr i "contract storage type mismatch"
    pure $ i :/ CREATE_CONTRACT contr ::: ((STOperation, NStar, ovn) ::&
                                           (STc SCAddress, NStar, avn) ::& rs)
  where
    outNotes :: HST '[ 'TPair ('TList 'TOperation) g' ] -> Notes g'
    outNotes ((_, n, _) ::& SNil) =
      notesCase NStar (\(NTPair _ _ _ _ n') -> n') n

typeCheckInstr (U.IMPLICIT_ACCOUNT vn)
           i@((STc SCKeyHash, _, _) ::& rs) =
  pure $ i :/ IMPLICIT_ACCOUNT ::: ((STContract STUnit, NStar, vn) ::& rs)

typeCheckInstr (U.NOW vn) i =
  pure $ i :/ NOW ::: ((STc SCTimestamp, NStar, vn) ::& i)

typeCheckInstr (U.AMOUNT vn) i =
  pure $ i :/ AMOUNT ::: ((STc SCMutez, NStar, vn) ::& i)

typeCheckInstr (U.BALANCE vn) i =
  pure $ i :/ BALANCE ::: ((STc SCMutez, NStar, vn) ::& i)

typeCheckInstr (U.CHECK_SIGNATURE vn)
           i@((STKey, _, _)
             ::& (STSignature, _, _) ::& (STc SCBytes, _, _) ::& rs) =
  pure $ i :/ CHECK_SIGNATURE ::: ((STc SCBool, NStar, vn) ::& rs)

typeCheckInstr (U.SHA256 vn)
           i@((STc SCBytes, _, _) ::& rs) =
  pure $ i :/ SHA256 ::: ((STc SCBytes, NStar, vn) ::& rs)

typeCheckInstr (U.SHA512 vn)
           i@((STc SCBytes, _, _) ::& rs) =
  pure $ i :/ SHA512 ::: ((STc SCBytes, NStar, vn) ::& rs)

typeCheckInstr (U.BLAKE2B vn)
           i@((STc SCBytes, _, _) ::& rs) =
  pure $ i :/ BLAKE2B ::: ((STc SCBytes, NStar, vn) ::& rs)

typeCheckInstr (U.HASH_KEY vn)
           i@((STKey, _, _) ::& rs) =
  pure $ i :/ HASH_KEY ::: ((STc SCKeyHash, NStar, vn) ::& rs)

typeCheckInstr (U.STEPS_TO_QUOTA vn) i =
  pure $ i :/ STEPS_TO_QUOTA ::: ((STc SCNat, NStar, vn) ::& i)

typeCheckInstr (U.SOURCE vn) i =
  pure $ i :/ SOURCE ::: ((STc SCAddress, NStar, vn) ::& i)

typeCheckInstr (U.SENDER vn) i =
  pure $ i :/ SENDER ::: ((STc SCAddress, NStar, vn) ::& i)

typeCheckInstr (U.ADDRESS vn) i@((STContract _, _, _) ::& rs) =
  pure $ i :/ ADDRESS ::: ((STc SCAddress, NStar, vn) ::& rs)

typeCheckInstr instr sit = throwError ...
  TCFailedOnInstr instr (SomeHST sit) "unknown expression" Nothing

-- | Helper function for two-branch if where each branch is given a single
-- value.
genericIf
  :: forall bti bfi cond rs .
    (Typeable bti, Typeable bfi)
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
genericIf cons mCons mbt mbf bti bfi i@(_ ::& _) = do
  _ :/ pinstr <- typeCheckList mbt bti
  _ :/ qinstr <- typeCheckList mbf bfi
  liftEither . fmap (i :/) $ case (pinstr, qinstr) of
    (p ::: (po :: HST po), q ::: (qo :: HST qo)) -> do
      let instr = mCons mbt mbf
      Refl <- checkEqT @qo @po instr i
                    "branches have different output stack types:"
      o <- convergeHST po qo `onLeft`
        typeCheckInstrAnnErr instr i "branches have different output stack types:"
      pure $ cons p q ::: o
    (AnyOutInstr p, q ::: (qo :: HST qo)) -> do
      pure $ cons p q ::: qo
    (p ::: (po :: HST po), AnyOutInstr q) -> do
      pure $ cons p q ::: po
    (AnyOutInstr p, AnyOutInstr q) ->
      pure $ AnyOutInstr (cons p q)

mapImpl
  :: forall c rs .
    ( MapOp c
    , SingI (MapOpInp c)
    , Typeable (MapOpInp c)
    , Typeable (MapOpRes c)
    )
  => Notes (MapOpInp c)
  -> U.ExpandedInstr
  -> [U.ExpandedOp]
  -> HST (c ': rs)
  -> (forall v' . (Typeable v', SingI v') =>
        Sing v' -> Notes v' -> HST rs -> HST (MapOpRes c v' ': rs))
  -> TypeCheckT (SomeInstr (c ': rs))
mapImpl vn instr mp i@(_ ::& rs) mkRes = do
  _ :/ subp <- typeCheckList mp ((sing, vn, def) ::& rs)
  liftEither $ case subp of
    sub ::: subo ->
      case subo of
        (b, bn, _bvn) ::& (rs' :: HST rs') -> do
          Refl <- checkEqT @rs @rs' instr i $
                      "map expression has changed not only top of the stack"
          pure $ i :/ MAP sub ::: mkRes b bn rs'
        _ -> Left ... TCFailedOnInstr instr (SomeHST i)
             "map expression has wrong output stack type (empty stack)" Nothing
    AnyOutInstr _ ->
      Left $ TCFailedOnInstr instr (SomeHST i)
        "MAP code block always fails, which is not allowed" Nothing

iterImpl
  :: forall c rs .
    ( IterOp c
    , SingI (IterOpEl c)
    , Typeable (IterOpEl c)
    )
  => Notes (IterOpEl c)
  -> U.ExpandedInstr
  -> NonEmpty U.ExpandedOp
  -> HST (c ': rs)
  -> TypeCheckT (SomeInstr (c ': rs))
iterImpl en instr mp i@((_, _, lvn) ::& rs) = do
  let evn = deriveVN "elt" lvn
  _ :/ subp <- typeCheckNE mp ((sing, en, evn) ::& rs)
  liftEither $ case subp of
    subI ::: (o :: HST o) -> do
      Refl <- checkEqT @o @rs instr i
                "iteration expression has wrong output stack type"
      pure $ i :/ ITER subI ::: o
    AnyOutInstr _ ->
      Left $ TCFailedOnInstr instr (SomeHST i)
        "ITER code block always fails, which is not allowed" Nothing

lamImpl
  :: forall it ot ts .
    ( Typeable it, Typeable ts, Typeable ot
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
  when (any hasSelf is) $ throwError ... TCFailedOnInstr instr (SomeHST i)
    "The SELF instruction cannot appear in a lambda" Nothing
  _ :/ lamI <- typeCheckList is ((it, ins, def) ::& SNil)
  let lamNotes onsr = mkNotes $ NTLambda def ins onsr
  let lamSt onsr = (STLambda it ot, lamNotes onsr, vn) ::& i
  liftEither . fmap (i :/) $ case lamI of
    lam ::: (lo :: HST lo) -> do
      case eqType @'[ ot ] @lo of
        Right Refl -> do
            let (_, ons', _) ::& SNil = lo
            onsr <- converge ons ons' `onLeft`
              typeCheckInstrAnnErr instr i "wrong output type of lambda's expression:"
            pure (LAMBDA (VLam lam) ::: lamSt onsr)
        Left m -> typeCheckInstrErr instr (SomeHST i)
                    "wrong output type of lambda's expression:" m
    AnyOutInstr lam ->
      pure (LAMBDA (VLam lam) ::: lamSt ons)

  where
    hasSelf :: U.ExpandedOp -> Bool
    hasSelf = everything (||)
      (mkQ False
       (\x -> case x of
           (U.SELF _ :: U.InstrAbstract U.ExpandedOp) -> True
           _ -> False
       )
      )
