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
-- @Instr cp inp out@ along with @IT inp@ and @IT out@ all wrapped into
-- @SomeInstr cp@ data type. This wrapping is done to satsify Haskell type
-- system (which has no support for dependent types).
-- Functions @typeCheckInstr@, @typeCheckVal@ behave similarly.
--
-- When a recursive call is made within @typeCheck@, @typeCheckInstr@ or
-- @typeCheckVal@, result of a call is unwrapped from @SomeInstr@ and type
-- information from @IT inp@ and @IT out@ is being used to assert that
-- recursive call returned instruction of expected type
-- (error is thrown otherwise).

module Michelson.TypeCheck.Instr
    ( typeCheckContract
    , typeCheckVal
    ) where

import Prelude hiding (EQ, GT, LT)

import Control.Monad.Except (liftEither, throwError, withExceptT)
import Data.Default (def)
import Data.Singletons (SingI(sing))
import Data.Typeable ((:~:)(..))
import Fmt (Buildable)

import Michelson.TypeCheck.Helpers
import Michelson.TypeCheck.Types
import Michelson.TypeCheck.Value
import Michelson.Typed
  (Abs, And, CT(..), Contract, ContractInp, ContractOut, Eq', Ge, Gt, Instr(..), IterOp(..), Le,
  Lsl, Lsr, Lt, MapOp(..), Neg, Neq, Not, Notes(..), Notes'(..), Or, Sing(..), T(..), Val(..), Xor,
  converge, convergeAnns, extractNotes, fromMType, mkNotes, notesCase, orAnn, withSomeSingCT,
  withSomeSingT, ( # ))

import qualified Michelson.Untyped as M
import Michelson.Untyped.Annotation (VarAnn)

typeCheckContract
  :: forall nop. (Show nop, Buildable nop)
  => TcNopHandler nop
  -> M.Contract (M.Instr nop)
  -> Either (TCError nop) SomeContract
typeCheckContract nh = runTypeCheckT nh . typeCheckContractImpl

typeCheckContractImpl
  :: forall cp nop. (Show nop, Buildable nop)
  => M.Contract (M.Instr nop)
  -> TypeCheckT cp nop SomeContract
typeCheckContractImpl (M.Contract mParam mStorage pCode) = do
  code <- maybe (throwError $ TCOtherError "no instructions in contract code")
                pure (nonEmpty pCode)
  withSomeSingT (fromMType mParam) $ \(paramS :: Sing param) ->
    withSomeSingT (fromMType mStorage) $ \(storageS :: Sing st) -> do
      storageNote <-
        liftEither $ extractNotes mStorage storageS `onLeft` \m -> TCOtherError $
                        "failed to extract annotations for storage: " <> m
      paramNote <-
        liftEither $ extractNotes mParam paramS `onLeft` \m -> TCOtherError $
                        "failed to extract annotations for parameter: " <> m
      let inpNote = mkNotes (NT_pair def def def paramNote storageNote)
      let inp = (ST_pair paramS storageS, inpNote, def) ::& INil
      typeCheckNE @param code (SomeIT inp) >>= \case
        SiFail -> do
          let outNote = mkNotes (NT_pair def def def NStar storageNote)
              out = (ST_pair (ST_list ST_operation) storageS, outNote, def)
                      ::& INil
          pure $ SomeContract FAILWITH inp out
        instr ::: ((inp' :: IT inp), (out :: IT out)) -> do
          let mkIErr m = TCOtherError $
                          "contract input type violates convention: " <> m
          let mkOErr m = TCOtherError $
                          "contract output type violates convention: " <> m
          liftEither $ do
            Refl <- eqT' @out @(ContractOut st) `onLeft` mkOErr
            Refl <- eqT' @inp @(ContractInp param st) `onLeft` mkIErr
            let outN = outNotes out
            _ <- converge outN (N $ NT_pair def def def NStar storageNote)
                    `onLeft` mkOErr
            pure $ SomeContract instr inp' out
  where
    outNotes :: IT '[o] -> Notes o
    outNotes ((_, n, _) ::& INil) = n

-- | Like 'typeCheck', but for non-empty lists.
typeCheckNE
  :: forall cp nop. (Typeable cp, SingI cp, Show nop, Buildable nop)
  => NonEmpty (M.Instr nop)
  -> SomeIT
  -> TypeCheckT cp nop (SomeInstr cp)
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
  :: forall cp nop. (Typeable cp, SingI cp, Show nop, Buildable nop)
  => [M.Instr nop]
  -> SomeIT
  -> TypeCheckT cp nop (SomeInstr cp)
typeCheckList = typeCheckImpl typeCheckInstr

-- | Function @typeCheckVal@ converts a single Michelson value
-- given in representation from @Michelson.Type@ module to representation
-- in strictly typed GADT.
--
-- As a second argument, @typeCheckVal@ accepts expected type of value.
--
-- Type checking algorithm pattern-matches on parse value representation,
-- expected type @t@ and constructs @Val cp t@ value.
--
-- If there was no match on a given pair of value and expected type,
-- that is interpreted as input of wrong type and type check finishes with
-- error.
typeCheckVal
  :: forall cp nop. (Typeable cp, SingI cp, Show nop, Buildable nop)
  => M.Value (M.Op nop)
  -> T
  -> TypeCheckT cp nop (SomeVal cp)
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
  :: forall cp nop . (Typeable cp, SingI cp, Show nop, Buildable nop)
  => TcInstrHandler cp nop

typeCheckInstr (M.NOP nop) si@(SomeIT it) = do
  nh <- asks tcNopHandler
  Nop ::: (it, it) <$ liftEither (nh nop si)

typeCheckInstr M.DROP (SomeIT i@(_ ::& rs)) = pure (DROP ::: (i, rs))

typeCheckInstr (M.DUP _vn) (SomeIT i@(a ::& rs)) =
  pure (DUP ::: (i, (a ::& a::& rs)))

typeCheckInstr M.SWAP (SomeIT i@(a ::& b ::& rs)) =
  pure (SWAP ::: (i, b ::& a ::& rs))

typeCheckInstr instr@(M.PUSH vn mt mval) (SomeIT i) = do
  val :::: (t, n) <- typeCheckVal mval (fromMType mt)
  notes <- liftEither $ (extractNotes mt t >>= converge n)
              `onLeft` TCFailedOnInstr instr (SomeIT i)
  pure $ PUSH val ::: (i, (t, notes, vn) ::& i)

typeCheckInstr (M.SOME tn vn fn) (SomeIT i@((at, an, _) ::& rs)) = do
  let n = mkNotes (NT_option tn fn an)
  pure (SOME ::: (i, (ST_option at, n, vn) ::& rs))

typeCheckInstr instr@(M.NONE tn vn fn elMt) (SomeIT i) = do
  withSomeSingT (fromMType elMt) $ \elT -> do
    let t = ST_option elT
    notes <- liftEither $ extractNotes (M.Type (M.T_option fn elMt) tn) t
              `onLeft` TCFailedOnInstr instr (SomeIT i)
    pure $ NONE ::: (i, (t, notes, vn) ::& i)

typeCheckInstr (M.UNIT tn vn) (SomeIT i) = do
  let ns = mkNotes $ NT_unit tn
  pure $ UNIT ::: (i, (ST_unit, ns, vn) ::& i)

typeCheckInstr (M.IF_NONE mp mq) (SomeIT i@((ST_option a, ons, ovn) ::& rs) ) = do
  let (an, avn) = deriveNsOption ons ovn
  genericIf IF_NONE M.IF_NONE mp mq rs ((a, an, avn) ::& rs) i

typeCheckInstr (M.PAIR tn vn pfn qfn) (SomeIT i@((a, an, avn) ::&
                                             (b, bn, bvn) ::& rs)) = do
  let (vn', pfn', qfn') = deriveSpecialFNs pfn qfn avn bvn
      ns = mkNotes $ NT_pair tn pfn' qfn' an bn
  pure (PAIR ::: (i, (ST_pair a b, ns, vn `orAnn` vn') ::& rs))

typeCheckInstr (M.CAR vn _) (SomeIT i@((ST_pair a _, NStar, _) ::& rs)) =
  pure (CAR ::: (i, (a, NStar, vn) ::& rs))
typeCheckInstr instr@(M.CAR vn fn)
            (SomeIT i@(( ST_pair a b
                       , N (NT_pair pairTN pfn qfn pns qns)
                       , pairVN ) ::& rs)) = do
  pfn' <- liftEither $ convergeAnns fn pfn
              `onLeft` TCFailedOnInstr instr (SomeIT i)
  let vn' = deriveSpecialVN vn pfn' pairVN
      i' = ( ST_pair a b
            , N (NT_pair pairTN pfn' qfn pns qns)
            , pairVN ) ::& rs
  pure $ CAR ::: (i', (a, pns, vn') ::& rs)

typeCheckInstr (M.CDR vn _) (SomeIT i@((ST_pair _ b, NStar, _) ::& rs)) =
  pure (CDR ::: (i, (b, NStar, vn) ::& rs))
typeCheckInstr instr@(M.CDR vn fn)
          (SomeIT i@(( ST_pair a b
                      , N (NT_pair pairTN pfn qfn pns qns)
                      , pairVN ) ::& rs)) = do
  qfn' <- liftEither $ convergeAnns fn qfn
              `onLeft` TCFailedOnInstr instr (SomeIT i)
  let vn' = deriveSpecialVN vn qfn' pairVN
      i' = ( ST_pair a b
            , N (NT_pair pairTN pfn qfn' pns qns)
            , pairVN ) ::& rs
  pure $ CDR ::: (i', (b, qns, vn') ::& rs)

typeCheckInstr instr@(M.LEFT tn vn pfn qfn bMt) (SomeIT i@((a, an, _) ::& rs)) =
  withSomeSingT (fromMType bMt) $ \b -> do
    bn <- liftEither $ extractNotes bMt b
              `onLeft` TCFailedOnInstr instr (SomeIT i)
    let ns = mkNotes $ NT_or tn pfn qfn an bn
    pure (LEFT ::: (i, (ST_or a b, ns, vn) ::& rs))

typeCheckInstr instr@(M.RIGHT tn vn pfn qfn aMt) (SomeIT i@((b, bn, _) ::& rs)) =
  withSomeSingT (fromMType aMt) $ \a -> do
    an <- liftEither $ extractNotes aMt a
              `onLeft` TCFailedOnInstr instr (SomeIT i)
    let ns = mkNotes $ NT_or tn pfn qfn an bn
    pure (RIGHT ::: (i, (ST_or a b, ns, vn) ::& rs))

typeCheckInstr (M.IF_LEFT mp mq) (SomeIT i@((ST_or a b, ons, ovn) ::& rs) ) = do
  let (an, bn, avn, bvn) = deriveNsOr ons ovn
      ait = (a, an, avn) ::& rs
      bit = (b, bn, bvn) ::& rs
  genericIf IF_LEFT M.IF_LEFT mp mq ait bit i

typeCheckInstr (M.IF_RIGHT mq mp) (SomeIT i@((ST_or a b, ons, ovn) ::& rs) ) = do
  let (an, bn, avn, bvn) = deriveNsOr ons ovn
      ait = (a, an, avn) ::& rs
      bit = (b, bn, bvn) ::& rs
  genericIf IF_RIGHT M.IF_RIGHT mq mp bit ait i

typeCheckInstr instr@(M.NIL tn vn elMt) (SomeIT i) =
  withSomeSingT (fromMType elMt) $ \elT -> liftEither $ do
    let t = ST_list elT
    notes <- extractNotes (M.Type (M.T_list elMt) tn) t
              `onLeft` TCFailedOnInstr instr (SomeIT i)
    pure $ NIL ::: (i, (t, notes, vn) ::& i)

typeCheckInstr instr@(M.CONS vn) (SomeIT i@(((at :: Sing a), an, _)
                              ::& (ST_list (_ :: Sing a'), ln, _) ::& rs)) =
  case eqT' @a @a' of
    Right Refl -> liftEither $  do
      n <- converge ln (mkNotes $ NT_list def an)
              `onLeft` TCFailedOnInstr instr (SomeIT i)
      pure $ CONS ::: (i, (ST_list at, n, vn) ::& rs)
    Left m -> typeCheckInstrErrM instr (SomeIT i) $
                "list element type is different from one "
                <> "that is being CONSed: " <> m


typeCheckInstr (M.IF_CONS mp mq) (SomeIT i@((ST_list a, ns, vn) ::& rs) ) = do
  let an = notesCase NStar (\(NT_list _ an_) -> an_) ns
      ait =
        (a, an, vn <> "hd") ::& (ST_list a, ns, vn <> "tl") ::& rs
  genericIf IF_CONS M.IF_CONS mp mq ait rs i

typeCheckInstr (M.SIZE vn) (SomeIT i@((ST_list _, _, _) ::& _) ) = liftEither $ sizeImpl i vn
typeCheckInstr (M.SIZE vn) (SomeIT i@((ST_set _, _, _) ::& _) ) = liftEither $ sizeImpl i vn
typeCheckInstr (M.SIZE vn) (SomeIT i@((ST_map _ _, _, _) ::& _) ) = liftEither $ sizeImpl i vn
typeCheckInstr (M.SIZE vn) (SomeIT i@((ST_c ST_string, _, _) ::& _) ) = liftEither $  sizeImpl i vn
typeCheckInstr (M.SIZE vn) (SomeIT i@((ST_c ST_bytes, _, _) ::& _) ) = liftEither $ sizeImpl i vn

typeCheckInstr (M.EMPTY_SET tn vn (M.Comparable mk ktn)) (SomeIT i) =
  withSomeSingCT mk $ \k ->
    pure $ EMPTY_SET ::: (i, (ST_set k, mkNotes $ NT_set tn ktn, vn) ::& i)

typeCheckInstr instr@(M.EMPTY_MAP tn vn (M.Comparable mk ktn) mv) (SomeIT i) =
  withSomeSingT (fromMType mv) $ \v ->
  withSomeSingCT mk $ \k -> liftEither $ do
    vns <- extractNotes mv v
              `onLeft` TCFailedOnInstr instr (SomeIT i)
    let ns = mkNotes $ NT_map tn ktn vns
    pure $ EMPTY_MAP ::: (i, (ST_map k v, ns, vn) ::& i)

typeCheckInstr instr@(M.MAP vn mp) (SomeIT i@((ST_list v, ns, _vn) ::& _) ) = do
  let vns = notesCase NStar (\(NT_list _ v') -> v') ns
  mapImpl v vns instr mp i
          (\rt rn -> (::&) (ST_list rt, mkNotes $ NT_list def rn, vn))
typeCheckInstr instr@(M.MAP vn mp) (SomeIT i@((ST_map k v, ns, _vn) ::& _) ) = do
  let (kns, vns) = notesCase (def, NStar) (\(NT_map _ k' v') -> (k', v')) ns
      pns = mkNotes $ NT_pair def def def (mkNotes $ NT_c kns) vns
  mapImpl (ST_pair (ST_c k) v) pns instr mp i
          (\rt rn -> (::&) (ST_map k rt, mkNotes $ NT_map def kns rn, vn))

-- case `M.ITER []` is wrongly typed by definition
-- (as it is required to at least drop an element), so we don't consider it

typeCheckInstr instr@(M.ITER (i1 : ir)) (SomeIT i@((ST_set e, n, _) ::& _)) = do
  let en = notesCase NStar (\(NT_set _ en_) -> mkNotes $ NT_c en_) n
  iterImpl (ST_c e) en instr (i1 :| ir) i
typeCheckInstr instr@(M.ITER (i1 : ir)) (SomeIT i@((ST_list e, n, _) ::& _)) = do
  let en = notesCase NStar (\(NT_list _ en_) -> en_) n
  iterImpl e en instr (i1 :| ir) i
typeCheckInstr instr@(M.ITER (i1 : ir)) (SomeIT i@((ST_map k v, n, _) ::& _)) = do
  let en = notesCase NStar (\(NT_map _ kns vns) ->
              mkNotes $ NT_pair def def def (mkNotes $ NT_c kns) vns) n
  iterImpl (ST_pair (ST_c k) v) en instr (i1 :| ir) i

typeCheckInstr instr@(M.MEM vn)
           (SomeIT i@((ST_c _, _, _) ::& (ST_set _, _, _) ::& _)) =
  liftEither $ memImpl instr i vn
typeCheckInstr instr@(M.MEM vn)
           (SomeIT i@((ST_c _, _, _) ::& (ST_map _ _, _, _) ::& _)) =
  liftEither $ memImpl instr i vn
typeCheckInstr instr@(M.MEM vn)
           (SomeIT i@((ST_c _, _, _) ::& (ST_big_map _ _, _, _) ::& _)) =
  liftEither $ memImpl instr i vn

typeCheckInstr instr@(M.GET vn) (SomeIT i@(_ ::& (ST_map _ vt, cn, _) ::& _)) =
  liftEither $ getImpl instr i vt (notesCase NStar (\(NT_map _ _ v) -> v) cn) vn
typeCheckInstr instr@(M.GET vn) (SomeIT i@(_ ::& (ST_big_map _ vt, cn, _) ::& _)) =
  liftEither $ getImpl instr i vt (notesCase NStar (\(NT_big_map _ _ v) -> v) cn) vn

typeCheckInstr instr@M.UPDATE (SomeIT i@(_ ::& _ ::& (ST_map _ _, _, _) ::& _)) =
  liftEither $ updImpl instr i
typeCheckInstr instr@M.UPDATE (SomeIT i@(_ ::& _ ::& (ST_big_map _ _, _, _)
                                             ::& _)) =
  liftEither $ updImpl instr i
typeCheckInstr instr@M.UPDATE (SomeIT i@(_ ::& _ ::& (ST_set _, _, _) ::& _)) =
  liftEither $ updImpl instr i

typeCheckInstr (M.IF mp mq) (SomeIT i@((ST_c ST_bool, _, _) ::& rs) ) =
  genericIf IF M.IF mp mq rs rs i

typeCheckInstr instr@(M.LOOP is)
           (SomeIT i@((ST_c ST_bool, _, _) ::& (rs :: IT rs)) ) = do
  typeCheckList @cp (fmap M.unOp is) (SomeIT rs) >>= \case
    SiFail -> pure SiFail
    subI ::: ((_ :: IT rs'), (o :: IT o)) -> liftEither $ do
      Refl <- assertEqT @rs @rs' instr i
      case (eqT' @o @('T_c 'T_bool ': rs), SomeIT o) of
        (Right Refl, SomeIT (_ ::& rs' :: IT o')) -> do
            Refl <- assertEqT @o @o' instr i
            pure $ LOOP subI ::: (i, rs')
        (Left m, _) ->
          typeCheckInstrErr instr (SomeIT i) $
                    "iteration expression has wrong output stack type: " <> m
        _ -> typeCheckInstrErr instr (SomeIT i) $
                        "iteration expression has wrong output stack type"

typeCheckInstr instr@(M.LOOP_LEFT is)
           (SomeIT i@((ST_or (at :: Sing a) (bt :: Sing b), ons, ovn)
                      ::& (rs :: IT rs)) ) = do
  let (an, bn, avn, bvn) = deriveNsOr ons ovn
      ait = (at, an, avn) ::& rs
  typeCheckList @cp (fmap M.unOp is) (SomeIT ait) >>= \case
    SiFail -> pure SiFail
    subI ::: ((_ :: IT rs'), (o :: IT o)) -> liftEither $ do
      Refl <- assertEqT @(a ': rs) @rs' instr i
      case (eqT' @o @('T_or a b ': rs), SomeIT o) of
        (Right Refl, SomeIT ((ST_or _ bt', ons', ovn') ::& rs' :: IT o')) -> do
            Refl <- assertEqT @o @o' instr i
            let (_, bn', _, bvn') = deriveNsOr ons' ovn'
            br <- convergeITEl (bt, bn, bvn) (bt', bn', bvn')
                    `onLeft` TCFailedOnInstr instr (SomeIT i)
            pure $ LOOP_LEFT subI ::: (i, br ::& rs')
        (Left m, _) -> typeCheckInstrErr instr (SomeIT i) $
                        "iteration expression has wrong output stack type: " <> m
        _ -> typeCheckInstrErr instr (SomeIT i) $
                        "iteration expression has wrong output stack type"

typeCheckInstr instr@(M.LAMBDA vn imt omt is) (SomeIT i) = do
  withSomeSingT (fromMType imt) $ \(it :: Sing it) -> do
    withSomeSingT (fromMType omt) $ \(ot :: Sing ot) -> do
      ins <- liftEither $ extractNotes imt it
              `onLeft` TCFailedOnInstr instr (SomeIT i)
      ons <- liftEither $ extractNotes omt ot
              `onLeft` TCFailedOnInstr instr (SomeIT i)
      -- further processing is extracted into another function because
      -- otherwise I encountered some weird GHC error with that code
      -- located right here
      lamImpl instr is vn it ins ot ons i

typeCheckInstr instr@(M.EXEC vn) (SomeIT i@(((_ :: Sing t1), _, _)
                              ::& (ST_lambda (_ :: Sing t1') t2, ln, _)
                              ::& rs)) = do
  let t2n = notesCase NStar (\(NT_lambda _ _ n) -> n) ln
  case eqT' @t1 @t1' of
    Right Refl -> pure $ EXEC ::: (i, (t2, t2n, vn) ::& rs)
    Left m -> typeCheckInstrErrM instr (SomeIT i) $
                "lambda is given argument with wrong type: " <> m

typeCheckInstr instr@(M.DIP is) (SomeIT i@(a ::& (s :: IT s))) =
  typeCheckList @cp (fmap M.unOp is) (SomeIT s) >>= \case
    SiFail -> pure SiFail
    subI ::: ((_ :: IT s'), t) -> liftEither $ do
      Refl <- assertEqT @s @s' instr i
      pure $ DIP subI ::: (i, a ::& t)

typeCheckInstr M.FAILWITH _ = pure SiFail

typeCheckInstr instr@(M.CAST vn mt)
           (SomeIT i@(((e :: Sing e), (en :: Notes e), evn) ::& rs)) =
  withSomeSingT (fromMType mt) $ \(_ :: Sing e') ->
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
      err = \m -> typeCheckInstrErrM instr (SomeIT i) $
                  "cast to incompatible type: " <> m

typeCheckInstr (M.RENAME vn) (SomeIT i@((at, an, _) ::& rs)) =
  pure $ RENAME ::: (i, (at, an, vn) ::& rs)

typeCheckInstr instr@(M.UNPACK vn mt) (SomeIT i@((ST_c ST_bytes, _, _) ::& rs)) =
  withSomeSingT (fromMType mt) $ \t -> liftEither $ do
    tns <- extractNotes mt t
            `onLeft` TCFailedOnInstr instr (SomeIT i)
    let ns = mkNotes $ NT_option def def tns
    pure $ UNPACK ::: (i, (ST_option t, ns, vn) ::& rs)

typeCheckInstr (M.PACK vn) (SomeIT i@(_ ::& rs)) =
  pure $ PACK ::: (i, (ST_c ST_bytes, NStar, vn) ::& rs)

typeCheckInstr (M.CONCAT vn) (SomeIT i@((ST_c ST_bytes, _, _) ::&
                                    (ST_c ST_bytes, _, _) ::& _)) =
  liftEither $ concatImpl i vn
typeCheckInstr (M.CONCAT vn) (SomeIT i@((ST_c ST_string, _, _) ::&
                                    (ST_c ST_string, _, _) ::& _)) =
  liftEither $ concatImpl i vn
typeCheckInstr (M.CONCAT vn) (SomeIT i@((ST_list (ST_c ST_bytes), _, _) ::& _)) =
  liftEither $  concatImpl' i vn
typeCheckInstr (M.CONCAT vn) (SomeIT i@((ST_list (ST_c ST_string), _, _) ::& _)) =
  liftEither $ concatImpl' i vn


typeCheckInstr (M.SLICE vn) (SomeIT i@((ST_c ST_nat, _, _) ::&
                                   (ST_c ST_nat, _, _) ::&
                                   (ST_c ST_string, _, _) ::& _)) =
  liftEither $ sliceImpl i vn
typeCheckInstr (M.SLICE vn) (SomeIT i@((ST_c ST_nat, _, _) ::&
                                   (ST_c ST_nat, _, _) ::&
                                   (ST_c ST_bytes, _, _) ::& _)) =
  liftEither $ sliceImpl i vn

typeCheckInstr (M.ISNAT vn') (SomeIT i@((ST_c ST_int, _, oldVn) ::& rs)) = do
  let vn = vn' `orAnn` oldVn
  pure $ ISNAT ::: (i, (ST_option (ST_c ST_nat), NStar, vn) ::& rs)

typeCheckInstr (M.ADD vn) (SomeIT i@((ST_c a, _, _) ::&
                                 (ST_c b, _, _) ::& _)) = liftEither $ addImpl a b i vn

typeCheckInstr (M.SUB vn) (SomeIT i@((ST_c a, _, _) ::&
                                 (ST_c b, _, _) ::& _)) = liftEither $ subImpl a b i vn

typeCheckInstr (M.MUL vn) (SomeIT i@((ST_c a, _, _) ::&
                                 (ST_c b, _, _) ::& _)) = liftEither $ mulImpl a b i vn

typeCheckInstr (M.EDIV vn) (SomeIT i@((ST_c a, _, _) ::&
                                  (ST_c b, _, _) ::& _)) = liftEither $ edivImpl a b i vn

typeCheckInstr (M.ABS vn) (SomeIT i@((ST_c ST_int, _, _) ::& _)) =
  liftEither $ unaryArithImpl @Abs ABS i vn

typeCheckInstr M.NEG (SomeIT i@((ST_c ST_int, _, _) ::& _)) =
  liftEither $ unaryArithImpl @Neg NEG i def

typeCheckInstr (M.LSL vn) (SomeIT i@((ST_c ST_nat, _, _) ::&
                         (ST_c ST_nat, _, _) ::& _)) = liftEither $ arithImpl @Lsl LSL i vn

typeCheckInstr (M.LSR vn) (SomeIT i@((ST_c ST_nat, _, _) ::&
                         (ST_c ST_nat, _, _) ::& _)) = liftEither $ arithImpl @Lsr LSR i vn

typeCheckInstr (M.OR vn) (SomeIT i@((ST_c ST_bool, _, _) ::&
                         (ST_c ST_bool, _, _) ::& _)) = liftEither $ arithImpl @Or OR i vn
typeCheckInstr (M.OR vn) (SomeIT i@((ST_c ST_nat, _, _) ::&
                         (ST_c ST_nat, _, _) ::& _)) = liftEither $ arithImpl @Or OR i vn

typeCheckInstr (M.AND vn) (SomeIT i@((ST_c ST_int, _, _) ::&
                         (ST_c ST_nat, _, _) ::& _)) = liftEither $ arithImpl @And AND i vn
typeCheckInstr (M.AND vn) (SomeIT i@((ST_c ST_nat, _, _) ::&
                         (ST_c ST_nat, _, _) ::& _)) = liftEither $ arithImpl @And AND i vn
typeCheckInstr (M.AND vn) (SomeIT i@((ST_c ST_bool, _, _) ::&
                         (ST_c ST_bool, _, _) ::& _)) = liftEither $ arithImpl @And AND i vn

typeCheckInstr (M.XOR vn) (SomeIT i@((ST_c ST_bool, _, _) ::&
                         (ST_c ST_bool, _, _) ::& _)) = liftEither $ arithImpl @Xor XOR i vn
typeCheckInstr (M.XOR vn) (SomeIT i@((ST_c ST_nat, _, _) ::&
                         (ST_c ST_nat, _, _) ::& _)) = liftEither $ arithImpl @Xor XOR i vn

typeCheckInstr (M.NOT vn) (SomeIT i@((ST_c ST_nat, _, _) ::& _)) =
  liftEither $ unaryArithImpl @Not NOT i vn
typeCheckInstr (M.NOT vn) (SomeIT i@((ST_c ST_bool, _, _) ::& _)) =
  liftEither $ unaryArithImpl @Not NOT i vn
typeCheckInstr (M.NOT vn) (SomeIT i@((ST_c ST_int, _, _) ::& _)) =
  liftEither $ unaryArithImpl @Not NOT i vn

typeCheckInstr (M.COMPARE vn) (SomeIT i@((ST_c a, _, _) ::&
                                 (ST_c b, _, _) ::& _)) = liftEither $ compareImpl a b i vn

typeCheckInstr (M.EQ vn) (SomeIT i@((ST_c ST_int, _, _) ::& _)) =
  liftEither $ unaryArithImpl @Eq' EQ i vn

typeCheckInstr (M.NEQ vn) (SomeIT i@((ST_c ST_int, _, _) ::& _)) =
  liftEither $ unaryArithImpl @Neq NEQ i vn

typeCheckInstr (M.LT vn) (SomeIT i@((ST_c ST_int, _, _) ::& _)) =
  liftEither $ unaryArithImpl @Lt LT i vn

typeCheckInstr (M.GT vn) (SomeIT i@((ST_c ST_int, _, _) ::& _)) =
  liftEither $ unaryArithImpl @Gt GT i vn

typeCheckInstr (M.LE vn) (SomeIT i@((ST_c ST_int, _, _) ::& _)) =
  liftEither $ unaryArithImpl @Le LE i vn

typeCheckInstr (M.GE vn) (SomeIT i@((ST_c ST_int, _, _) ::& _)) =
  liftEither $ unaryArithImpl @Ge GE i vn

typeCheckInstr (M.INT vn) (SomeIT i@((ST_c ST_nat, _, _) ::& rs)) =
  pure $ INT ::: (i, (ST_c ST_int, NStar, vn) ::& rs)

typeCheckInstr (M.SELF vn) (SomeIT i) =
  pure $ SELF ::: (i, (sing, NStar, vn) ::& i)

typeCheckInstr instr@(M.CONTRACT vn mt)
           (SomeIT i@((ST_c ST_address, _, _) ::& rs)) =
  withSomeSingT (fromMType mt) $ \t -> liftEither $ do
    tns <- extractNotes mt t
            `onLeft` TCFailedOnInstr instr (SomeIT i)
    let ns = mkNotes $ NT_option def def $ mkNotes $ NT_contract def tns
    pure $ CONTRACT ::: (i, (ST_option $ ST_contract t, ns, vn) ::& rs)

typeCheckInstr instr@(M.TRANSFER_TOKENS vn) (SomeIT i@(((_ :: Sing p'), _, _)
  ::& (ST_c ST_mutez, _, _) ::& (ST_contract (_ :: Sing p), _, _) ::& rs)) = do
  case eqT' @p @p' of
    Right Refl ->
      pure $ TRANSFER_TOKENS ::: (i, (ST_operation, NStar, vn) ::& rs)
    Left m ->
      typeCheckInstrErrM instr (SomeIT i) $ "mismatch of contract param type: " <> m

typeCheckInstr (M.SET_DELEGATE vn)
           (SomeIT i@((ST_option (ST_c ST_key_hash), _, _) ::& rs)) = do
  pure $ SET_DELEGATE ::: (i, (ST_operation, NStar, vn) ::& rs)

typeCheckInstr (M.CREATE_ACCOUNT ovn avn)
           (SomeIT i@((ST_c ST_key_hash, _, _)
             ::& (ST_option (ST_c ST_key_hash), _, _) ::& (ST_c ST_bool, _, _)
             ::& (ST_c ST_mutez, _, _) ::& rs)) =
  pure $ CREATE_ACCOUNT ::: (i, (ST_operation, NStar, ovn) ::&
                                 (ST_c ST_address, NStar, avn) ::& rs)

typeCheckInstr instr@(M.CREATE_CONTRACT ovn avn)
           (SomeIT i@((ST_c ST_key_hash, _, _)
             ::& (ST_option (ST_c ST_key_hash), _, _)
             ::& (ST_c ST_bool, _, _) ::& (ST_c ST_bool, _, _)
             ::& (ST_c ST_mutez, _, _)
             ::& (ST_lambda (ST_pair _ (_ :: Sing g1))
                   (ST_pair (ST_list ST_operation) (_ :: Sing g2)), ln, _)
                        ::& ((_ :: Sing g3), gn3, _) ::& rs)) = do
  let (gn1, gn2) = notesCase (NStar, NStar)
                    (\(NT_lambda _ l r) ->
                      (,) (notesCase NStar (\(NT_pair _ _ _ _ n) -> n) l)
                          (notesCase NStar (\(NT_pair _ _ _ _ n) -> n) r)) ln
  liftEither $ either (\m -> typeCheckInstrErr instr (SomeIT i) $
                  "mismatch of contract storage type: " <> m) pure $ do
    Refl <- eqT' @g1 @g2
    Refl <- eqT' @g2 @g3
    gn12 <- converge gn1 gn2
    _ <- converge gn12 gn3
    pure $ CREATE_CONTRACT ::: (i, (ST_operation, NStar, ovn) ::&
                                     (ST_c ST_address, NStar, avn) ::& rs)

typeCheckInstr instr@(M.CREATE_CONTRACT2 ovn avn contract)
           (SomeIT i@((ST_c ST_key_hash, _, _)
             ::& (ST_option (ST_c ST_key_hash), _, _) ::& (ST_c ST_bool, _, _)
             ::& (ST_c ST_bool, _, _) ::& (ST_c ST_mutez, _, _)
             ::& ((_ :: Sing g), gn, _) ::& rs)) = do
  (SomeContract (contr :: Contract i' g') _ out) <-
      flip withExceptT (typeCheckContractImpl $ fmap M.unOp contract)
                       (\err -> TCFailedOnInstr instr (SomeIT i)
                                    ("failed to type check contract: " <> show err))
  liftEither $ do
    Refl <- checkEqT @g @g' instr i "contract storage type mismatch"
    void $ converge gn (outNotes out) `onLeft` TCFailedOnInstr instr (SomeIT i)
    pure $ CREATE_CONTRACT2 contr ::: (i, (ST_operation, NStar, ovn) ::&
                                          (ST_c ST_address, NStar, avn) ::& rs)
  where
    outNotes :: IT '[ 'T_pair ('T_list 'T_operation) g' ] -> Notes g'
    outNotes ((_, n, _) ::& INil) =
      notesCase NStar (\(NT_pair _ _ _ _ n') -> n') n

typeCheckInstr (M.IMPLICIT_ACCOUNT vn)
           (SomeIT i@((ST_c ST_key_hash, _, _) ::& rs)) =
  pure $ IMPLICIT_ACCOUNT ::: (i, (ST_contract ST_unit, NStar, vn) ::& rs)

typeCheckInstr (M.NOW vn) (SomeIT i) =
  pure $ NOW ::: (i, (ST_c ST_timestamp, NStar, vn) ::& i)

typeCheckInstr (M.AMOUNT vn) (SomeIT i) =
  pure $ AMOUNT ::: (i, (ST_c ST_mutez, NStar, vn) ::& i)

typeCheckInstr (M.BALANCE vn) (SomeIT i) =
  pure $ BALANCE ::: (i, (ST_c ST_mutez, NStar, vn) ::& i)

typeCheckInstr (M.CHECK_SIGNATURE vn)
           (SomeIT i@((ST_key, _, _)
             ::& (ST_signature, _, _) ::& (ST_c ST_bytes, _, _) ::& rs)) =
  pure $ CHECK_SIGNATURE ::: (i, (ST_c ST_bool, NStar, vn) ::& rs)

typeCheckInstr (M.SHA256 vn)
           (SomeIT i@((ST_c ST_bytes, _, _) ::& rs)) =
  pure $ SHA256 ::: (i, (ST_c ST_bytes, NStar, vn) ::& rs)

typeCheckInstr (M.SHA512 vn)
           (SomeIT i@((ST_c ST_bytes, _, _) ::& rs)) =
  pure $ SHA512 ::: (i, (ST_c ST_bytes, NStar, vn) ::& rs)

typeCheckInstr (M.BLAKE2B vn)
           (SomeIT i@((ST_c ST_bytes, _, _) ::& rs)) =
  pure $ BLAKE2B ::: (i, (ST_c ST_bytes, NStar, vn) ::& rs)

typeCheckInstr (M.HASH_KEY vn)
           (SomeIT i@((ST_key, _, _) ::& rs)) =
  pure $ HASH_KEY ::: (i, (ST_c ST_key_hash, NStar, vn) ::& rs)

typeCheckInstr (M.STEPS_TO_QUOTA vn) (SomeIT i) =
  pure $ STEPS_TO_QUOTA ::: (i, (ST_c ST_nat, NStar, vn) ::& i)

typeCheckInstr (M.SOURCE vn) (SomeIT i) =
  pure $ SOURCE ::: (i, (ST_c ST_address, NStar, vn) ::& i)

typeCheckInstr (M.SENDER vn) (SomeIT i) =
  pure $ SENDER ::: (i, (ST_c ST_address, NStar, vn) ::& i)

typeCheckInstr (M.ADDRESS vn) (SomeIT i@((ST_contract _, _, _) ::& rs)) =
  pure $ ADDRESS ::: (i, (ST_c ST_address, NStar, vn) ::& rs)

typeCheckInstr instr sit = typeCheckInstrErrM instr sit ""

-- | Helper function for two-branch if where each branch is given a single
-- value.
genericIf
  :: forall cp bti bfi cond rs nop.
    (Typeable cp, SingI cp, Typeable bti, Typeable bfi, Show nop, Buildable nop)
  => (forall s'. Instr cp bti s'
          -> Instr cp bfi s' -> Instr cp (cond ': rs) s')
  -> ([M.Op nop] -> [M.Op nop] -> M.Instr nop) -> [M.Op nop] -> [M.Op nop]
  -> IT bti
  -> IT bfi
  -> IT (cond ': rs)
  -> TypeCheckT cp nop (SomeInstr cp)
genericIf cons mCons mbt mbf bti bfi i@(_ ::& _) =
  liftA2 (,) (typeCheckList @cp (M.unOp <$> mbt) (SomeIT bti))
             (typeCheckList @cp (M.unOp <$> mbf) (SomeIT bfi)) >>= liftEither . \case
  (p ::: ((_ :: IT pi), (po :: IT po)), q ::: ((_ :: IT qi), (qo :: IT qo))) -> do
    Refl <- assertEqT @bti @pi instr i
    Refl <- assertEqT @bfi @qi instr i
    Refl <- checkEqT @qo @po instr i
                  "branches have different output stack types"
    o <- convergeIT po qo `onLeft` TCFailedOnInstr instr (SomeIT i)
    pure $ cons p q ::: (i, o)
  (SiFail, q ::: ((_ :: IT qi), (qo :: IT qo))) -> do
    Refl <- assertEqT @bfi @qi instr i
    -- TODO TM-27 There shall be no `PUSH VUnit`, rewrite this code
    pure $ cons (PUSH VUnit # FAILWITH) q ::: (i, qo)
  (p ::: ((_ :: IT pi), (po :: IT po)), SiFail) -> do
    Refl <- assertEqT @bti @pi instr i
    -- TODO TM-27 There shall be no `PUSH VUnit`, rewrite this code
    pure $ cons p (PUSH VUnit # FAILWITH) ::: (i, po)
  _ -> pure SiFail

  where
    instr = mCons mbt mbf

mapImpl
  :: forall cp c rs nop .
  ( MapOp c
  , Typeable (MapOpInp c)
  , Typeable cp, SingI cp
  , Typeable (MapOpRes c)
  , Show nop
  , Buildable nop
  )
  => Sing (MapOpInp c) -> Notes (MapOpInp c)
  -> M.Instr nop -> [M.Op nop] -> IT (c ': rs)
  -> (forall v' . Typeable v' =>
        Sing v' -> Notes v' -> IT rs -> IT (MapOpRes c v' ': rs))
  -> TypeCheckT cp nop (SomeInstr cp)
mapImpl pt pns instr mp i@(_ ::& rs) mkRes =
  typeCheckList @cp (M.unOp <$> mp)
                      (SomeIT $ (pt, pns, def) ::& rs) >>= \case
    SiFail -> pure SiFail
    sub ::: ((_ :: IT subi), (subo :: IT subo)) -> liftEither $ do
      Refl <- assertEqT @subi @(MapOpInp c ': rs) instr i
      case SomeIT subo of
        SomeIT ((b, bn, _bvn) ::& (rs' :: IT rs') :: IT subo') -> do
          Refl <- assertEqT @subo @subo' instr i
          Refl <- checkEqT @rs @rs' instr i $
                      "iteration expression has wrong output stack type"
          pure $ MAP sub ::: (i, mkRes b bn rs')
        _ -> typeCheckInstrErr instr (SomeIT i) $
              "iteration expression has wrong output stack type (empty stack)"

iterImpl
  :: forall cp c rs nop .
    ( IterOp c
    , Typeable (IterOpEl c)
    , Typeable cp, SingI cp
    , Show nop
    , Buildable nop
    )
  => Sing (IterOpEl c) -> Notes (IterOpEl c)
  -> M.Instr nop
  -> NonEmpty (M.Op nop)
  -> IT (c ': rs)
  -> TypeCheckT cp nop (SomeInstr cp)
iterImpl et en instr mp i@((_, _, lvn) ::& rs) = do
  let evn = deriveVN "elt" lvn
  typeCheckNE @cp (fmap M.unOp mp) (SomeIT ((et, en, evn) ::& rs)) >>= \case
    SiFail -> pure SiFail
    subI ::: ((_ :: IT i), (o :: IT o)) -> liftEither $ do
      Refl <- assertEqT @i @(IterOpEl c ': rs) instr i
      Refl <- checkEqT @o @rs instr i
                "iteration expression has wrong output stack type"
      pure $ ITER subI ::: (i, o)

lamImpl
  :: forall it ot cp ts nop.
    ( Typeable it, Typeable ts, Typeable ot
    , Typeable cp, SingI cp
    , Show nop, Buildable nop
    , SingI it, SingI ot
    )
  => M.Instr nop
  -> [M.Op nop]  -> VarAnn
  -> Sing it -> Notes it
  -> Sing ot -> Notes ot
  -> IT ts
  -> TypeCheckT cp nop (SomeInstr cp)
lamImpl instr is vn it ins ot ons i = do
  typeCheckList @cp (fmap M.unOp is) (SomeIT $ (it, ins, def) ::& INil) >>=
    \case
      SiFail -> pure SiFail
      lam ::: ((_ :: IT li), (lo :: IT lo)) -> liftEither $ do
        Refl <- assertEqT @'[ it ] @li instr i
        case (eqT' @'[ ot ] @lo, SomeIT lo) of
          (Right Refl, SomeIT ((_, ons', _) ::& INil :: IT lo')) -> do
              Refl <- assertEqT @lo @lo' instr i
              onsr <- converge ons ons'
                        `onLeft` TCFailedOnInstr instr (SomeIT i)
              let ns = mkNotes $ NT_lambda def ins onsr
              pure (LAMBDA (VLam lam) ::: (i, (ST_lambda it ot, ns, vn) ::& i))
          (Right Refl, _) ->
            typeCheckInstrErr instr (SomeIT i)
              "wrong output type of lambda's expression (wrong stack size)"
          (Left m, _) -> typeCheckInstrErr instr (SomeIT i) $
                          "wrong output type of lambda's expression: " <> m
