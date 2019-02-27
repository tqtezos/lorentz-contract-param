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
-- Functions @typeCheckI@, @typeCheckVal@ behave similarly.
--
-- When a recursive call is made within @typeCheck@, @typeCheckI@ or
-- @typeCheckVal@, result of a call is unwrapped from @SomeInstr@ and type
-- information from @IT inp@ and @IT out@ is being used to assert that
-- recursive call returned instruction of expected type
-- (error is thrown otherwise).

module Michelson.TypeCheck.Instr
    ( typeCheck
    , typeCheckContract
    , typeCheckVal
    ) where

import Data.Default (def)
import Data.Singletons (SingI(sing))
import Data.Typeable ((:~:)(..))
import Prelude hiding (EQ, GT, LT)

import Michelson.TypeCheck.Helpers
import Michelson.TypeCheck.Types
import Michelson.TypeCheck.Value
import Michelson.Typed
  (Abs, And, CT(..), Eq', Ge, Gt, Instr(..), IterOp(..), Le, Lsl, Lsr, Lt, MapOp(..), Neg, Neq,
  Not, Notes(..), Notes'(..), Or, Sing(..), T(..), Val(..), Xor, converge, convergeAnns,
  extractNotes, fromMType, mkNotes, notesCase, orAnn, withSomeSingCT, withSomeSingT)

import Michelson.Types (VarAnn)
import qualified Michelson.Types as M

-- | Like 'typeCheck', but for non-empty lists.
typeCheckNE
  :: forall cp.
    (Typeable cp, SingI cp)
  => NonEmpty M.Instr
  -> SomeIT
  -> Either TCError (SomeInstr cp)
typeCheckNE (a :| []) t = typeCheckI a t
typeCheckNE (p_ :| (r : rs)) (SomeIT (a :: IT a)) = do
  typeCheckI @cp p_ (SomeIT a) >>= \case
    p ::: ((_ :: IT a'), (b :: IT b)) ->
      typeCheckNE @cp (r :| rs) (SomeIT b) >>= \case
        q ::: ((_ :: IT b'), c) -> do
          Refl <- eqT' @a @a' `onLeft` TCOtherError
          Refl <- eqT' @b @b' `onLeft` TCOtherError
          pure $ (Seq p q) ::: (a, c)
        SiFail -> pure SiFail
    SiFail -> pure SiFail

-- | Function @typeCheck@ converts list of Michelson instructions
-- given in representation from @Michelson.Type@ module to representation
-- in strictly typed GADT.
--
-- Types are checked along the way which is neccessary to construct a
-- strictly typed value.
--
-- As a second argument, @typeCheck@ accepts input stack type representation.
typeCheck
  :: forall cp.
    (Typeable cp, SingI cp)
  => [M.Instr]
  -> SomeIT
  -> Either TCError (SomeInstr cp)
typeCheck [] (SomeIT s) = pure $ Nop ::: (s, s)
typeCheck (a : as) sit = typeCheckNE (a :| as) sit

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
  :: forall cp.
    (Typeable cp, SingI cp)
  => M.Value M.Op -> T -> Either TCError (SomeVal cp)
typeCheckVal = typeCheckValImpl typeCheck

-- | Function @typeCheckI@ converts a single Michelson instruction
-- given in representation from @Michelson.Type@ module to representation
-- in strictly typed GADT.
--
-- As a second argument, @typeCheckI@ accepts input stack type representation.
--
-- Type checking algorithm pattern-matches on given instruction, input stack
-- type and constructs strictly typed GADT value, checking necessary type
-- equalities when neccessary.
--
-- If there was no match on a given pair of instruction and input stack,
-- that is interpreted as input of wrong type and type check finishes with
-- error.
typeCheckI
  :: forall cp.
    (Typeable cp, SingI cp)
  => M.Instr -> SomeIT -> Either TCError (SomeInstr cp)

typeCheckI M.DROP (SomeIT i@(_ ::& rs)) = pure (DROP ::: (i, rs))

typeCheckI (M.DUP _vn) (SomeIT i@(a ::& rs)) =
  pure (DUP ::: (i, (a ::& a::& rs)))

typeCheckI M.SWAP (SomeIT i@(a ::& b ::& rs)) =
  pure (SWAP ::: (i, b ::& a ::& rs))

typeCheckI instr@(M.PUSH vn mt mval) (SomeIT i) = do
  val :::: (t, n) <- typeCheckVal mval (fromMType mt)
  notes <- (extractNotes mt t >>= converge n)
              `onLeft` TCFailedOnInstr instr (SomeIT i)
  pure $ PUSH val ::: (i, (t, notes, vn) ::& i)

typeCheckI (M.SOME tn vn fn) (SomeIT i@((at, an, _) ::& rs)) = do
  let n = mkNotes (NT_option tn fn an)
  pure (SOME ::: (i, (ST_option at, n, vn) ::& rs))

typeCheckI instr@(M.NONE tn vn fn elMt) (SomeIT i) = do
  withSomeSingT (fromMType elMt) $ \elT -> do
    let t = ST_option elT
    notes <- extractNotes (M.Type (M.T_option fn elMt) tn) t
              `onLeft` TCFailedOnInstr instr (SomeIT i)
    pure $ NONE ::: (i, (t, notes, vn) ::& i)

typeCheckI (M.UNIT tn vn) (SomeIT i) = do
  let ns = mkNotes $ NT_unit tn
  pure $ UNIT ::: (i, (ST_unit, ns, vn) ::& i)

typeCheckI (M.IF_NONE mp mq) (SomeIT i@((ST_option a, ons, ovn) ::& rs) ) = do
  let (an, avn) = deriveNsOption ons ovn
  genericIf IF_NONE M.IF_NONE mp mq rs ((a, an, avn) ::& rs) i

typeCheckI (M.PAIR tn vn pfn qfn) (SomeIT i@((a, an, avn) ::&
                                             (b, bn, bvn) ::& rs)) = do
  let (vn', pfn', qfn') = deriveSpecialFNs pfn qfn avn bvn
      ns = mkNotes $ NT_pair tn pfn' qfn' an bn
  pure (PAIR ::: (i, (ST_pair a b, ns, vn `orAnn` vn') ::& rs))

typeCheckI (M.CAR vn _) (SomeIT i@((ST_pair a _, NStar, _) ::& rs)) =
  pure (CAR ::: (i, (a, NStar, vn) ::& rs))
typeCheckI instr@(M.CAR vn fn)
            (SomeIT i@(( ST_pair a b
                       , N (NT_pair pairTN pfn qfn pns qns)
                       , pairVN ) ::& rs)) = do
  pfn' <- convergeAnns fn pfn
              `onLeft` TCFailedOnInstr instr (SomeIT i)
  let vn' = deriveSpecialVN vn pfn' pairVN
      i' = ( ST_pair a b
            , N (NT_pair pairTN pfn' qfn pns qns)
            , pairVN ) ::& rs
  pure $ CAR ::: (i', (a, pns, vn') ::& rs)

typeCheckI (M.CDR vn _) (SomeIT i@((ST_pair _ b, NStar, _) ::& rs)) =
  pure (CDR ::: (i, (b, NStar, vn) ::& rs))
typeCheckI instr@(M.CDR vn fn)
          (SomeIT i@(( ST_pair a b
                      , N (NT_pair pairTN pfn qfn pns qns)
                      , pairVN ) ::& rs)) = do
  qfn' <- convergeAnns fn qfn
              `onLeft` TCFailedOnInstr instr (SomeIT i)
  let vn' = deriveSpecialVN vn qfn' pairVN
      i' = ( ST_pair a b
            , N (NT_pair pairTN pfn qfn' pns qns)
            , pairVN ) ::& rs
  pure $ CDR ::: (i', (b, qns, vn') ::& rs)

typeCheckI instr@(M.LEFT tn vn pfn qfn bMt) (SomeIT i@((a, an, _) ::& rs)) =
  withSomeSingT (fromMType bMt) $ \b -> do
    bn <- extractNotes bMt b
              `onLeft` TCFailedOnInstr instr (SomeIT i)
    let ns = mkNotes $ NT_or tn pfn qfn an bn
    pure (LEFT ::: (i, (ST_or a b, ns, vn) ::& rs))

typeCheckI instr@(M.RIGHT tn vn pfn qfn aMt) (SomeIT i@((b, bn, _) ::& rs)) =
  withSomeSingT (fromMType aMt) $ \a -> do
    an <- extractNotes aMt a
              `onLeft` TCFailedOnInstr instr (SomeIT i)
    let ns = mkNotes $ NT_or tn pfn qfn an bn
    pure (RIGHT ::: (i, (ST_or a b, ns, vn) ::& rs))

typeCheckI (M.IF_LEFT mp mq) (SomeIT i@((ST_or a b, ons, ovn) ::& rs) ) = do
  let (an, bn, avn, bvn) = deriveNsOr ons ovn
      ait = (a, an, avn) ::& rs
      bit = (b, bn, bvn) ::& rs
  genericIf IF_LEFT M.IF_LEFT mp mq ait bit i

typeCheckI (M.IF_RIGHT mq mp) (SomeIT i@((ST_or a b, ons, ovn) ::& rs) ) = do
  let (an, bn, avn, bvn) = deriveNsOr ons ovn
      ait = (a, an, avn) ::& rs
      bit = (b, bn, bvn) ::& rs
  genericIf IF_RIGHT M.IF_RIGHT mq mp bit ait i

typeCheckI instr@(M.NIL tn vn elMt) (SomeIT i) =
  withSomeSingT (fromMType elMt) $ \elT -> do
    let t = ST_list elT
    notes <- extractNotes (M.Type (M.T_list elMt) tn) t
              `onLeft` TCFailedOnInstr instr (SomeIT i)
    pure $ NIL ::: (i, (t, notes, vn) ::& i)

typeCheckI instr@(M.CONS vn) (SomeIT i@(((at :: Sing a), an, _)
                              ::& (ST_list (_ :: Sing a'), ln, _) ::& rs)) =
  case eqT' @a @a' of
    Right Refl -> do
      n <- converge ln (mkNotes $ NT_list def an)
              `onLeft` TCFailedOnInstr instr (SomeIT i)
      pure $ CONS ::: (i, (ST_list at, n, vn) ::& rs)
    Left m -> typeCheckIErr instr (SomeIT i) $
                "list element type is different from one "
                <> "that is being CONSed: " <> m


typeCheckI (M.IF_CONS mp mq) (SomeIT i@((ST_list a, ns, vn) ::& rs) ) = do
  let an = notesCase NStar (\(NT_list _ an_) -> an_) ns
      ait =
        (a, an, vn <> "hd") ::& (ST_list a, ns, vn <> "tl") ::& rs
  genericIf IF_CONS M.IF_CONS mp mq ait rs i

typeCheckI (M.SIZE vn) (SomeIT i@((ST_list _, _, _) ::& _) ) = sizeImpl i vn
typeCheckI (M.SIZE vn) (SomeIT i@((ST_set _, _, _) ::& _) ) = sizeImpl i vn
typeCheckI (M.SIZE vn) (SomeIT i@((ST_map _ _, _, _) ::& _) ) = sizeImpl i vn
typeCheckI (M.SIZE vn) (SomeIT i@((ST_c ST_string, _, _) ::& _) ) =
  sizeImpl i vn
typeCheckI (M.SIZE vn) (SomeIT i@((ST_c ST_bytes, _, _) ::& _) ) =
  sizeImpl i vn

typeCheckI (M.EMPTY_SET tn vn (M.Comparable mk ktn)) (SomeIT i) =
  withSomeSingCT mk $ \k ->
    pure $ EMPTY_SET ::: (i, (ST_set k, mkNotes $ NT_set tn ktn, vn) ::& i)

typeCheckI instr@(M.EMPTY_MAP tn vn (M.Comparable mk ktn) mv) (SomeIT i) =
  withSomeSingT (fromMType mv) $ \v ->
  withSomeSingCT mk $ \k -> do
    vns <- extractNotes mv v
              `onLeft` TCFailedOnInstr instr (SomeIT i)
    let ns = mkNotes $ NT_map tn ktn vns
    pure $ EMPTY_MAP ::: (i, (ST_map k v, ns, vn) ::& i)

typeCheckI instr@(M.MAP vn mp) (SomeIT i@((ST_list v, ns, _vn) ::& _) ) = do
  let vns = notesCase NStar (\(NT_list _ v') -> v') ns
  mapImpl v vns instr mp i
          (\rt rn -> (::&) (ST_list rt, mkNotes $ NT_list def rn, vn))
typeCheckI instr@(M.MAP vn mp) (SomeIT i@((ST_map k v, ns, _vn) ::& _) ) = do
  let (kns, vns) = notesCase (def, NStar) (\(NT_map _ k' v') -> (k', v')) ns
      pns = mkNotes $ NT_pair def def def (mkNotes $ NT_c kns) vns
  mapImpl (ST_pair (ST_c k) v) pns instr mp i
          (\rt rn -> (::&) (ST_map k rt, mkNotes $ NT_map def kns rn, vn))

-- case `M.ITER []` is wrongly typed by definition
-- (as it is required to at least drop an element), so we don't consider it

typeCheckI instr@(M.ITER (i1 : ir)) (SomeIT i@((ST_set e, n, _) ::& _)) = do
  let en = notesCase NStar (\(NT_set _ en_) -> mkNotes $ NT_c en_) n
  iterImpl (ST_c e) en instr (i1 :| ir) i
typeCheckI instr@(M.ITER (i1 : ir)) (SomeIT i@((ST_list e, n, _) ::& _)) = do
  let en = notesCase NStar (\(NT_list _ en_) -> en_) n
  iterImpl e en instr (i1 :| ir) i
typeCheckI instr@(M.ITER (i1 : ir)) (SomeIT i@((ST_map k v, n, _) ::& _)) = do
  let en = notesCase NStar (\(NT_map _ kns vns) ->
              mkNotes $ NT_pair def def def (mkNotes $ NT_c kns) vns) n
  iterImpl (ST_pair (ST_c k) v) en instr (i1 :| ir) i

typeCheckI instr@(M.MEM vn)
           (SomeIT i@((ST_c _, _, _) ::& (ST_set _, _, _) ::& _)) =
  memImpl instr i vn
typeCheckI instr@(M.MEM vn)
           (SomeIT i@((ST_c _, _, _) ::& (ST_map _ _, _, _) ::& _)) =
  memImpl instr i vn
typeCheckI instr@(M.MEM vn)
           (SomeIT i@((ST_c _, _, _) ::& (ST_big_map _ _, _, _) ::& _)) =
  memImpl instr i vn

typeCheckI instr@(M.GET vn) (SomeIT i@(_ ::& (ST_map _ vt, cn, _) ::& _)) =
  getImpl instr i vt (notesCase NStar (\(NT_map _ _ v) -> v) cn) vn
typeCheckI instr@(M.GET vn) (SomeIT i@(_ ::& (ST_big_map _ vt, cn, _) ::& _)) =
  getImpl instr i vt (notesCase NStar (\(NT_big_map _ _ v) -> v) cn) vn

typeCheckI instr@M.UPDATE (SomeIT i@(_ ::& _ ::& (ST_map _ _, _, _) ::& _)) =
  updImpl instr i
typeCheckI instr@M.UPDATE (SomeIT i@(_ ::& _ ::& (ST_big_map _ _, _, _)
                                             ::& _)) =
  updImpl instr i
typeCheckI instr@M.UPDATE (SomeIT i@(_ ::& _ ::& (ST_set _, _, _) ::& _)) =
  updImpl instr i

typeCheckI (M.IF mp mq) (SomeIT i@((ST_c ST_bool, _, _) ::& rs) ) =
  genericIf IF M.IF mp mq rs rs i

typeCheckI instr@(M.LOOP is)
           (SomeIT i@((ST_c ST_bool, _, _) ::& (rs :: IT rs)) ) = do
  typeCheck @cp (fmap M.unOp is) (SomeIT rs) >>= \case
    SiFail -> pure SiFail
    subI ::: ((_ :: IT rs'), (o :: IT o)) -> do
      Refl <- assertEqT @rs @rs' instr i
      case (eqT' @o @('T_c 'T_bool ': rs), SomeIT o) of
        (Right Refl, SomeIT (_ ::& rs' :: IT o')) -> do
            Refl <- assertEqT @o @o' instr i
            pure $ LOOP subI ::: (i, rs')
        (Left m, _) ->
          typeCheckIErr instr (SomeIT i) $
                    "iteration expression has wrong output stack type: " <> m
        _ -> typeCheckIErr instr (SomeIT i) $
                        "iteration expression has wrong output stack type"

typeCheckI instr@(M.LOOP_LEFT is)
           (SomeIT i@((ST_or (at :: Sing a) (bt :: Sing b), ons, ovn)
                      ::& (rs :: IT rs)) ) = do
  let (an, bn, avn, bvn) = deriveNsOr ons ovn
      ait = (at, an, avn) ::& rs
  typeCheck @cp (fmap M.unOp is) (SomeIT ait) >>= \case
    SiFail -> pure SiFail
    subI ::: ((_ :: IT rs'), (o :: IT o)) -> do
      Refl <- assertEqT @(a ': rs) @rs' instr i
      case (eqT' @o @('T_or a b ': rs), SomeIT o) of
        (Right Refl, SomeIT ((ST_or _ bt', ons', ovn') ::& rs' :: IT o')) -> do
            Refl <- assertEqT @o @o' instr i
            let (_, bn', _, bvn') = deriveNsOr ons' ovn'
            br <- convergeITEl (bt, bn, bvn) (bt', bn', bvn')
                    `onLeft` TCFailedOnInstr instr (SomeIT i)
            pure $ LOOP_LEFT subI ::: (i, br ::& rs')
        (Left m, _) -> typeCheckIErr instr (SomeIT i) $
                        "iteration expression has wrong output stack type: " <> m
        _ -> typeCheckIErr instr (SomeIT i) $
                        "iteration expression has wrong output stack type"

typeCheckI instr@(M.LAMBDA vn imt omt is) (SomeIT i) = do
  withSomeSingT (fromMType imt) $ \(it :: Sing it) -> do
    withSomeSingT (fromMType omt) $ \(ot :: Sing ot) -> do
      ins <- extractNotes imt it
              `onLeft` TCFailedOnInstr instr (SomeIT i)
      ons <- extractNotes omt ot
              `onLeft` TCFailedOnInstr instr (SomeIT i)
      -- further processing is extracted into another function because
      -- otherwise I encountered some weird GHC error with that code
      -- located right here
      lamImpl instr is vn it ins ot ons i

typeCheckI instr@(M.EXEC vn) (SomeIT i@(((_ :: Sing t1), _, _)
                              ::& (ST_lambda (_ :: Sing t1') t2, ln, _)
                              ::& rs)) = do
  let t2n = notesCase NStar (\(NT_lambda _ _ n) -> n) ln
  case eqT' @t1 @t1' of
    Right Refl -> pure $ EXEC ::: (i, (t2, t2n, vn) ::& rs)
    Left m -> typeCheckIErr instr (SomeIT i) $
                "lambda is given argument with wrong type: " <> m

typeCheckI instr@(M.DIP is) (SomeIT i@(a ::& (s :: IT s))) =
  typeCheck @cp (fmap M.unOp is) (SomeIT s) >>= \case
    SiFail -> pure SiFail
    subI ::: ((_ :: IT s'), t) -> do
      Refl <- assertEqT @s @s' instr i
      pure $ DIP subI ::: (i, a ::& t)

typeCheckI M.FAILWITH _ = pure SiFail

typeCheckI instr@(M.CAST vn mt)
           (SomeIT i@(((e :: Sing e), en, evn) ::& rs)) = do
  withSomeSingT (fromMType mt) $ \(_ :: Sing e') -> do
    let check = do
          Refl <- eqT' @e @e'
          en' <- extractNotes mt e
          converge en' en
    case check of
      Right ns ->
        pure $ CAST ::: (i, (e, ns, vn `orAnn` evn) ::& rs)
      Left m -> typeCheckIErr instr (SomeIT i) $
                  "cast to incompatible type: " <> m

typeCheckI (M.RENAME vn) (SomeIT i@((at, an, _) ::& rs)) =
  pure $ RENAME ::: (i, (at, an, vn) ::& rs)

typeCheckI instr@(M.UNPACK vn mt) (SomeIT i@((ST_c ST_bytes, _, _) ::& rs)) =
  withSomeSingT (fromMType mt) $ \t -> do
    tns <- extractNotes mt t
            `onLeft` TCFailedOnInstr instr (SomeIT i)
    let ns = mkNotes $ NT_option def def tns
    pure $ UNPACK ::: (i, (ST_option t, ns, vn) ::& rs)

typeCheckI (M.PACK vn) (SomeIT i@(_ ::& rs)) =
  pure $ PACK ::: (i, (ST_c ST_bytes, NStar, vn) ::& rs)

typeCheckI (M.CONCAT vn) (SomeIT i@((ST_c ST_bytes, _, _) ::&
                                    (ST_c ST_bytes, _, _) ::& _)) =
  concatImpl i vn
typeCheckI (M.CONCAT vn) (SomeIT i@((ST_c ST_string, _, _) ::&
                                    (ST_c ST_string, _, _) ::& _)) =
  concatImpl i vn
typeCheckI (M.CONCAT vn) (SomeIT i@((ST_list (ST_c ST_bytes), _, _) ::& _)) =
  concatImpl' i vn
typeCheckI (M.CONCAT vn) (SomeIT i@((ST_list (ST_c ST_string), _, _) ::& _)) =
  concatImpl' i vn


typeCheckI (M.SLICE vn) (SomeIT i@((ST_c ST_nat, _, _) ::&
                                   (ST_c ST_nat, _, _) ::&
                                   (ST_c ST_string, _, _) ::& _)) =
  sliceImpl i vn
typeCheckI (M.SLICE vn) (SomeIT i@((ST_c ST_nat, _, _) ::&
                                   (ST_c ST_nat, _, _) ::&
                                   (ST_c ST_bytes, _, _) ::& _)) =
  sliceImpl i vn

typeCheckI (M.ISNAT vn') (SomeIT i@((ST_c ST_int, _, oldVn) ::& rs)) = do
  let vn = vn' `orAnn` oldVn
  pure $ ISNAT ::: (i, (ST_option (ST_c ST_nat), NStar, vn) ::& rs)

typeCheckI (M.ADD vn) (SomeIT i@((ST_c a, _, _) ::&
                                 (ST_c b, _, _) ::& _)) = addImpl a b i vn

typeCheckI (M.SUB vn) (SomeIT i@((ST_c a, _, _) ::&
                                 (ST_c b, _, _) ::& _)) = subImpl a b i vn

typeCheckI (M.MUL vn) (SomeIT i@((ST_c a, _, _) ::&
                                 (ST_c b, _, _) ::& _)) = mulImpl a b i vn

typeCheckI (M.EDIV vn) (SomeIT i@((ST_c a, _, _) ::&
                                  (ST_c b, _, _) ::& _)) = edivImpl a b i vn

typeCheckI (M.ABS vn) (SomeIT i@((ST_c ST_int, _, _) ::& _)) =
  unaryArithImpl @Abs ABS i vn

typeCheckI M.NEG (SomeIT i@((ST_c ST_int, _, _) ::& _)) =
  unaryArithImpl @Neg NEG i def

typeCheckI (M.LSL vn) (SomeIT i@((ST_c ST_nat, _, _) ::&
                         (ST_c ST_nat, _, _) ::& _)) = arithImpl @Lsl LSL i vn

typeCheckI (M.LSR vn) (SomeIT i@((ST_c ST_nat, _, _) ::&
                         (ST_c ST_nat, _, _) ::& _)) = arithImpl @Lsr LSR i vn

typeCheckI (M.OR vn) (SomeIT i@((ST_c ST_bool, _, _) ::&
                         (ST_c ST_bool, _, _) ::& _)) = arithImpl @Or OR i vn
typeCheckI (M.OR vn) (SomeIT i@((ST_c ST_nat, _, _) ::&
                         (ST_c ST_nat, _, _) ::& _)) = arithImpl @Or OR i vn

typeCheckI (M.AND vn) (SomeIT i@((ST_c ST_int, _, _) ::&
                         (ST_c ST_nat, _, _) ::& _)) = arithImpl @And AND i vn
typeCheckI (M.AND vn) (SomeIT i@((ST_c ST_nat, _, _) ::&
                         (ST_c ST_nat, _, _) ::& _)) = arithImpl @And AND i vn
typeCheckI (M.AND vn) (SomeIT i@((ST_c ST_bool, _, _) ::&
                         (ST_c ST_bool, _, _) ::& _)) = arithImpl @And AND i vn

typeCheckI (M.XOR vn) (SomeIT i@((ST_c ST_bool, _, _) ::&
                         (ST_c ST_bool, _, _) ::& _)) = arithImpl @Xor XOR i vn
typeCheckI (M.XOR vn) (SomeIT i@((ST_c ST_nat, _, _) ::&
                         (ST_c ST_nat, _, _) ::& _)) = arithImpl @Xor XOR i vn

typeCheckI (M.NOT vn) (SomeIT i@((ST_c ST_nat, _, _) ::& _)) =
  unaryArithImpl @Not NOT i vn
typeCheckI (M.NOT vn) (SomeIT i@((ST_c ST_bool, _, _) ::& _)) =
  unaryArithImpl @Not NOT i vn
typeCheckI (M.NOT vn) (SomeIT i@((ST_c ST_int, _, _) ::& _)) =
  unaryArithImpl @Not NOT i vn

typeCheckI (M.COMPARE vn) (SomeIT i@((ST_c a, _, _) ::&
                                 (ST_c b, _, _) ::& _)) = compareImpl a b i vn

typeCheckI (M.EQ vn) (SomeIT i@((ST_c ST_int, _, _) ::& _)) =
  unaryArithImpl @Eq' EQ i vn

typeCheckI (M.NEQ vn) (SomeIT i@((ST_c ST_int, _, _) ::& _)) =
  unaryArithImpl @Neq NEQ i vn

typeCheckI (M.LT vn) (SomeIT i@((ST_c ST_int, _, _) ::& _)) =
  unaryArithImpl @Lt LT i vn

typeCheckI (M.GT vn) (SomeIT i@((ST_c ST_int, _, _) ::& _)) =
  unaryArithImpl @Gt GT i vn

typeCheckI (M.LE vn) (SomeIT i@((ST_c ST_int, _, _) ::& _)) =
  unaryArithImpl @Le LE i vn

typeCheckI (M.GE vn) (SomeIT i@((ST_c ST_int, _, _) ::& _)) =
  unaryArithImpl @Ge GE i vn

typeCheckI (M.INT vn) (SomeIT i@((ST_c ST_nat, _, _) ::& rs)) =
  pure $ INT ::: (i, (ST_c ST_int, NStar, vn) ::& rs)

typeCheckI (M.SELF vn) (SomeIT i) =
  pure $ SELF ::: (i, (sing, NStar, vn) ::& i)

typeCheckI instr@(M.CONTRACT vn mt)
           (SomeIT i@((ST_c ST_address, _, _) ::& rs)) =
  withSomeSingT (fromMType mt) $ \t -> do
    tns <- extractNotes mt t
            `onLeft` TCFailedOnInstr instr (SomeIT i)
    let ns = mkNotes $ NT_option def def $ mkNotes $ NT_contract def tns
    pure $ CONTRACT ::: (i, (ST_option $ ST_contract t, ns, vn) ::& rs)

typeCheckI instr@(M.TRANSFER_TOKENS vn) (SomeIT i@(((_ :: Sing p'), _, _)
  ::& (ST_c ST_mutez, _, _) ::& (ST_contract (_ :: Sing p), _, _) ::& rs)) = do
  case eqT' @p @p' of
    Right Refl ->
      pure $ TRANSFER_TOKENS ::: (i, (ST_operation, NStar, vn) ::& rs)
    Left m ->
      typeCheckIErr instr (SomeIT i) $ "mismatch of contract param type: " <> m

typeCheckI (M.SET_DELEGATE vn)
           (SomeIT i@((ST_option (ST_c ST_key_hash), _, _) ::& rs)) = do
  pure $ SET_DELEGATE ::: (i, (ST_operation, NStar, vn) ::& rs)

typeCheckI (M.CREATE_ACCOUNT ovn avn)
           (SomeIT i@((ST_c ST_key_hash, _, _)
             ::& (ST_option (ST_c ST_key_hash), _, _) ::& (ST_c ST_bool, _, _)
             ::& (ST_c ST_mutez, _, _) ::& rs)) =
  pure $ CREATE_ACCOUNT ::: (i, (ST_operation, NStar, ovn) ::&
                                 (ST_c ST_address, NStar, avn) ::& rs)

typeCheckI instr@(M.CREATE_CONTRACT ovn avn)
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
  either (\m -> typeCheckIErr instr (SomeIT i) $
                  "mismatch of contract storage type: " <> m) pure $ do
    Refl <- eqT' @g1 @g2
    Refl <- eqT' @g2 @g3
    gn12 <- converge gn1 gn2
    _ <- converge gn12 gn3
    pure $ CREATE_CONTRACT ::: (i, (ST_operation, NStar, ovn) ::&
                                     (ST_c ST_address, NStar, avn) ::& rs)

typeCheckI instr@(M.CREATE_CONTRACT2 ovn avn contract)
           (SomeIT i@((ST_c ST_key_hash, _, _)
             ::& (ST_option (ST_c ST_key_hash), _, _) ::& (ST_c ST_bool, _, _)
             ::& (ST_c ST_bool, _, _) ::& (ST_c ST_mutez, _, _)
             ::& ((_ :: Sing g), gn, _) ::& rs)) = do
  (SomeContract contr _ (out :: IT (ContractOut g'))) <-
      typeCheckContract (fmap M.unOp contract)
        `onLeft` \err -> TCFailedOnInstr instr (SomeIT i)
                          ("failed to type check contract: " <> show err)
  Refl <- checkEqT @g @g' instr i "contract storage type mismatch"
  converge gn (outNotes out) `onLeft` TCFailedOnInstr instr (SomeIT i)
  pure $ CREATE_CONTRACT2 contr
          ::: (i, (ST_operation, NStar, ovn) ::&
                  (ST_c ST_address, NStar, avn) ::& rs)
  where
    outNotes :: IT '[ 'T_pair ('T_list 'T_operation) g' ] -> Notes g'
    outNotes ((_, n, _) ::& INil) =
      notesCase NStar (\(NT_pair _ _ _ _ n') -> n') n

typeCheckI (M.IMPLICIT_ACCOUNT vn)
           (SomeIT i@((ST_c ST_key_hash, _, _) ::& rs)) =
  pure $ IMPLICIT_ACCOUNT ::: (i, (ST_contract ST_unit, NStar, vn) ::& rs)

typeCheckI (M.NOW vn) (SomeIT i) =
  pure $ NOW ::: (i, (ST_c ST_timestamp, NStar, vn) ::& i)

typeCheckI (M.AMOUNT vn) (SomeIT i) =
  pure $ AMOUNT ::: (i, (ST_c ST_mutez, NStar, vn) ::& i)

typeCheckI (M.BALANCE vn) (SomeIT i) =
  pure $ BALANCE ::: (i, (ST_c ST_mutez, NStar, vn) ::& i)

typeCheckI (M.CHECK_SIGNATURE vn)
           (SomeIT i@((ST_key, _, _)
             ::& (ST_signature, _, _) ::& (ST_c ST_bytes, _, _) ::& rs)) =
  pure $ CHECK_SIGNATURE ::: (i, (ST_c ST_bool, NStar, vn) ::& rs)

typeCheckI (M.SHA256 vn)
           (SomeIT i@((ST_c ST_bytes, _, _) ::& rs)) =
  pure $ SHA256 ::: (i, (ST_c ST_bytes, NStar, vn) ::& rs)

typeCheckI (M.SHA512 vn)
           (SomeIT i@((ST_c ST_bytes, _, _) ::& rs)) =
  pure $ SHA512 ::: (i, (ST_c ST_bytes, NStar, vn) ::& rs)

typeCheckI (M.BLAKE2B vn)
           (SomeIT i@((ST_c ST_bytes, _, _) ::& rs)) =
  pure $ BLAKE2B ::: (i, (ST_c ST_bytes, NStar, vn) ::& rs)

typeCheckI (M.HASH_KEY vn)
           (SomeIT i@((ST_key, _, _) ::& rs)) =
  pure $ HASH_KEY ::: (i, (ST_c ST_key_hash, NStar, vn) ::& rs)

typeCheckI (M.STEPS_TO_QUOTA vn) (SomeIT i) =
  pure $ STEPS_TO_QUOTA ::: (i, (ST_c ST_nat, NStar, vn) ::& i)

typeCheckI (M.SOURCE vn) (SomeIT i) =
  pure $ SOURCE ::: (i, (ST_c ST_address, NStar, vn) ::& i)

typeCheckI (M.SENDER vn) (SomeIT i) =
  pure $ SENDER ::: (i, (ST_c ST_address, NStar, vn) ::& i)

typeCheckI (M.ADDRESS vn) (SomeIT i@((ST_contract _, _, _) ::& rs)) =
  pure $ ADDRESS ::: (i, (ST_c ST_address, NStar, vn) ::& rs)

typeCheckI instr sit = typeCheckIErr instr sit ""

typeCheckContract
  :: forall. M.Contract M.Instr -> Either TCError (SomeContract)
typeCheckContract (M.Contract mParam mStorage pCode) = do
    code <- maybe (Left $ TCOtherError "no instructions in contract code")
                  pure (nonEmpty pCode)
    withSomeSingT (fromMType mParam) $ \(paramS :: Sing param) ->
      withSomeSingT (fromMType mStorage) $ \(storageS :: Sing st) -> do
        storageNote <-
          extractNotes mStorage storageS `onLeft` \m -> TCOtherError $
                          "failed to extract annotations for storage: " <> m
        paramNote <-
          extractNotes mParam paramS `onLeft` \m -> TCOtherError $
                          "failed to extract annotations for parameter: " <> m
        let inpNote =
              mkNotes (NT_pair def def def paramNote storageNote)
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
            Refl <- eqT' @out @(ContractOut st) `onLeft` mkOErr
            Refl <- eqT' @inp @(ContractInp param st) `onLeft` mkIErr
            let outN = outNotes out
            _ <- converge outN (N $ NT_pair def def def NStar storageNote)
                   `onLeft` mkOErr
            pure $ SomeContract instr inp' out
  where
    outNotes :: IT '[o] -> Notes o
    outNotes ((_, n, _) ::& INil) = n

-- | Helper function for two-branch if where each branch is given a single
-- value.
genericIf
  :: forall cp bti bfi cond rs.
    (Typeable cp, SingI cp, Typeable bti, Typeable bfi)
  => (forall s'. Instr cp bti s'
         -> Instr cp bfi s' -> Instr cp (cond ': rs) s')
  -> ([M.Op] -> [M.Op] -> M.Instr) -> [M.Op] -> [M.Op]
  -> IT bti
  -> IT bfi
  -> IT (cond ': rs) -> Either TCError (SomeInstr cp)
genericIf cons mCons mbt mbf bti bfi i@(_ ::& _) =
  liftA2 (,) (typeCheck @cp (M.unOp <$> mbt) (SomeIT bti))
             (typeCheck @cp (M.unOp <$> mbf) (SomeIT bfi)) >>= \case
  (p ::: ((_ :: IT pi), (po :: IT po)), q ::: ((_ :: IT qi), (qo :: IT qo))) ->
    do
      Refl <- assertEqT @bti @pi instr i
      Refl <- assertEqT @bfi @qi instr i
      Refl <- checkEqT @qo @po instr i
                    "branches have different output stack types"
      o <- convergeIT po qo `onLeft` TCFailedOnInstr instr (SomeIT i)
      pure $ cons p q ::: (i, o)
  (SiFail, q ::: ((_ :: IT qi), (qo :: IT qo))) -> do
      Refl <- assertEqT @bfi @qi instr i
      pure $ cons FAILWITH q ::: (i, qo)
  (p ::: ((_ :: IT pi), (po :: IT po)), SiFail) -> do
      Refl <- assertEqT @bti @pi instr i
      pure $ cons p FAILWITH ::: (i, po)
  _ -> pure SiFail

  where
    instr = mCons mbt mbf

mapImpl
  :: forall cp c rs.
    ( MapOp c
    , Typeable (MapOpInp c)
    , Typeable cp, SingI cp
    , Typeable (MapOpRes c)
    )
  => Sing (MapOpInp c) -> Notes (MapOpInp c)
  -> M.Instr -> [M.Op] -> IT (c ': rs)
  -> (forall v' . Typeable v' =>
        Sing v' -> Notes v' -> IT rs -> IT (MapOpRes c v' ': rs))
  -> Either TCError (SomeInstr cp)
mapImpl pt pns instr mp i@(_ ::& rs) mkRes =
  typeCheck @cp (M.unOp <$> mp)
                      (SomeIT $ (pt, pns, def) ::& rs) >>= \case
    SiFail -> pure SiFail
    sub ::: ((_ :: IT subi), (subo :: IT subo)) -> do
      Refl <- assertEqT @subi @(MapOpInp c ': rs) instr i
      case SomeIT subo of
        SomeIT ((b, bn, _bvn) ::& (rs' :: IT rs') :: IT subo') -> do
          Refl <- assertEqT @subo @subo' instr i
          Refl <- checkEqT @rs @rs' instr i $
                      "iteration expression has wrong output stack type"
          pure $ MAP sub ::: (i, mkRes b bn rs')
        _ -> typeCheckIErr instr (SomeIT i) $
              "iteration expression has wrong output stack type (empty stack)"

iterImpl
  :: forall cp c rs.
    ( IterOp c
    , Typeable (IterOpEl c)
    , Typeable cp, SingI cp
    )
  => Sing (IterOpEl c) -> Notes (IterOpEl c)
  -> M.Instr -> NonEmpty M.Op -> IT (c ': rs)
  -> Either TCError (SomeInstr cp)
iterImpl et en instr mp i@((_, _, lvn) ::& rs) = do
  let evn = deriveVN "elt" lvn
  typeCheckNE @cp (fmap M.unOp mp) (SomeIT ((et, en, evn) ::& rs)) >>= \case
    SiFail -> pure SiFail
    subI ::: ((_ :: IT i), (o :: IT o)) -> do
      Refl <- assertEqT @i @(IterOpEl c ': rs) instr i
      Refl <- checkEqT @o @rs instr i
                "iteration expression has wrong output stack type"
      pure $ ITER subI ::: (i, o)

lamImpl
  :: forall it ot cp ts.
    ( Typeable it, Typeable ts, Typeable ot
    , Typeable cp, SingI cp
    )
  => M.Instr
  -> [M.Op]  -> VarAnn
  -> Sing it -> Notes it
  -> Sing ot -> Notes ot
  -> IT ts
  -> Either TCError (SomeInstr cp)
lamImpl instr is vn it ins ot ons i = do
  typeCheck @cp (fmap M.unOp is) (SomeIT $ (it, ins, def) ::& INil) >>=
    \case
      SiFail -> pure SiFail
      lam ::: ((_ :: IT li), (lo :: IT lo)) -> do
        Refl <- assertEqT @'[ it ] @li instr i
        case (eqT' @'[ ot ] @lo, SomeIT lo) of
          (Right Refl, SomeIT ((_, ons', _) ::& INil :: IT lo')) -> do
              Refl <- assertEqT @lo @lo' instr i
              onsr <- converge ons ons'
                        `onLeft` TCFailedOnInstr instr (SomeIT i)
              let ns = mkNotes $ NT_lambda def ins onsr
              pure (LAMBDA (VLam lam) ::: (i, (ST_lambda it ot, ns, vn) ::& i))
          (Right Refl, _) ->
            typeCheckIErr instr (SomeIT i)
              "wrong output type of lambda's expression (wrong stack size)"
          (Left m, _) -> typeCheckIErr instr (SomeIT i) $
                          "wrong output type of lambda's expression: " <> m
