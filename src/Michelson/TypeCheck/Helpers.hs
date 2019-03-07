module Michelson.TypeCheck.Helpers
    ( onLeft
    , deriveSpecialVN
    , deriveSpecialFNs
    , deriveVN
    , deriveNsOr
    , deriveNsOption
    , convergeITEl
    , convergeIT

    , eqT'
    , assertEqT
    , checkEqT
    , typeCheckInstrErr
    , typeCheckInstrErrM
    , typeCheckImpl

    , memImpl
    , getImpl
    , updImpl
    , sliceImpl
    , concatImpl
    , concatImpl'
    , sizeImpl
    , arithImpl
    , addImpl
    , subImpl
    , mulImpl
    , edivImpl
    , compareImpl
    , unaryArithImpl
    ) where

import Prelude hiding (EQ, GT, LT)

import Control.Monad.Except (liftEither, throwError)
import Data.Default (def)
import Data.Singletons (SingI(sing))
import qualified Data.Text as T
import Data.Typeable ((:~:)(..), eqT, typeRep)

import Michelson.TypeCheck.Types
import Michelson.Typed
  (CT(..), Instr(..), Notes(..), Notes'(..), Sing(..), T(..), converge, mkNotes, notesCase, orAnn)
import Michelson.Typed.Arith (Add, ArithOp(..), Compare, Mul, Sub, UnaryArithOp(..))
import Michelson.Typed.Polymorphic
  (ConcatOp, EDivOp(..), GetOp(..), MemOp(..), SizeOp, SliceOp, UpdOp(..))

import qualified Michelson.Untyped as M
import Michelson.Untyped.Annotation (FieldAnn, VarAnn)

-- | Function which derives special annotations
-- for PAIR instruction.
--
-- Namely, it does following transformation:
-- @
--  PAIR %@@ %@@ [ @@p.a int : @@p.b int : .. ]
--  ~
--  [ @@p (pair (int %a) (int %b) : .. ]
-- @
--
-- All relevant cases (e.g. @PAIR %myf %@@ @)
-- are handled as they should be according to spec.
deriveSpecialFNs
  :: FieldAnn -> FieldAnn
  -> VarAnn -> VarAnn
  -> (VarAnn, FieldAnn, FieldAnn)
deriveSpecialFNs "@" "@" (M.Annotation pvn) (M.Annotation qvn) = (vn, pfn, qfn)
  where
    ps = T.splitOn "." pvn
    qs = T.splitOn "." qvn
    fns = fst <$> takeWhile (\(a, b) -> a == b) (zip ps qs)
    vn = M.Annotation $ T.intercalate "." fns
    pfn = M.Annotation $ T.intercalate "." $ drop (length fns) ps
    qfn = M.Annotation $ T.intercalate "." $ drop (length fns) qs
deriveSpecialFNs "@" qfn pvn _   = (def, M.convAnn pvn, qfn)
deriveSpecialFNs pfn "@" _ qvn   = (def, pfn, M.convAnn qvn)
deriveSpecialFNs pfn qfn _ _     = (def, pfn, qfn)

-- | Function which derives special annotations
-- for CDR / CAR instructions.
deriveSpecialVN :: VarAnn -> FieldAnn -> VarAnn -> VarAnn
deriveSpecialVN vn elFn pairVN
  | vn == "%" = M.convAnn elFn
  | vn == "%%" && elFn /= def = pairVN <> M.convAnn elFn
  | otherwise = vn

-- | Append suffix to variable annotation (if it's not empty)
deriveVN :: VarAnn -> VarAnn -> VarAnn
deriveVN suffix vn = bool (suffix <> vn) def (vn == def)

-- | Function which extracts annotations for @or@ type
-- (for left and right parts).
--
-- It extracts field/type annotations and also auto-generates variable
-- annotations if variable annotation is not provided as second argument.
deriveNsOr :: Notes ('T_or a b) -> VarAnn -> (Notes a, Notes b, VarAnn, VarAnn)
deriveNsOr ons ovn =
  let (an, bn, afn, bfn) =
        notesCase (NStar, NStar, def, def)
          (\(NT_or _ afn_ bfn_ an_ bn_) -> (an_, bn_, afn_, bfn_)) ons
      avn = deriveVN (M.convAnn afn `orAnn` "left") ovn
      bvn = deriveVN (M.convAnn bfn `orAnn` "right") ovn
   in (an, bn, avn, bvn)

-- | Function which extracts annotations for @option t@ type.
--
-- It extracts field/type annotations and also auto-generates variable
-- annotation for @Some@ case if it is not provided as second argument.
deriveNsOption :: Notes ('T_option a) -> VarAnn -> (Notes a, VarAnn)
deriveNsOption ons ovn =
  let (an, fn) = notesCase (NStar, def)
                    (\(NT_option _ fn_ an_) -> (an_, fn_)) ons
      avn = deriveVN (M.convAnn fn `orAnn` "some") ovn
   in (an, avn)

convergeITEl
  :: (Sing t, Notes t, VarAnn)
  -> (Sing t, Notes t, VarAnn)
  -> Either Text (Sing t, Notes t, VarAnn)
convergeITEl (at, an, avn) (_, bn, bvn) =
  (,,) at <$> converge an bn
          <*> pure (bool def avn $ avn == bvn)

-- | Combine annotations from two given stack types
convergeIT :: IT ts -> IT ts -> Either Text (IT ts)
convergeIT INil INil = pure INil
convergeIT (a ::& as) (b ::& bs) =
    liftA2 (::&) (convergeITEl a b) (convergeIT as bs)

-- TODO move to Util module
onLeft :: Either a c -> (a -> b) -> Either b c
onLeft = flip first

--------------------------------------------
-- Typechecker auxiliary
--------------------------------------------

checkEqT
  :: forall a b ts nop . (Typeable a, Typeable b, Typeable ts)
  => M.Instr nop
  -> IT ts
  -> Text
  -> Either (TCError nop) (a :~: b)
checkEqT instr i m =
  eqT' @a @b `onLeft` (TCFailedOnInstr instr (SomeIT i) . ((m <> ": ") <>))

assertEqT
  :: forall a b ts nop . (Typeable a, Typeable b, Typeable ts)
  => M.Instr nop
  -> IT ts
  -> Either (TCError nop) (a :~: b)
assertEqT instr i = checkEqT instr i "unexpected"

-- | Function @eqT'@ is a simple wrapper around @Data.Typeable.eqT@ suited
-- for use within @Either Text a@ applicative.
eqT' :: forall a b . (Typeable a, Typeable b) => Either Text (a :~: b)
eqT' = maybe (Left $
                "types not equal: "
                  <> show (typeRep (Proxy @a))
                  <> " /= "
                  <> show (typeRep (Proxy @b))
                  ) pure eqT

typeCheckInstrErr :: M.Instr nop -> SomeIT -> Text -> Either (TCError nop) a
typeCheckInstrErr = Left ... TCFailedOnInstr

typeCheckInstrErrM :: M.Instr nop -> SomeIT -> Text -> TypeCheckT cp nop a
typeCheckInstrErrM = throwError ... TCFailedOnInstr

typeCheckImpl
  :: forall cp nop . (Typeable cp, SingI cp)
  => TcInstrHandler cp nop
  -> [M.Instr nop]
  -> SomeIT
  -> TypeCheckT cp nop (SomeInstr cp)
typeCheckImpl tcInstr [a] t = tcInstr a t
typeCheckImpl tcInstr (p_ : (r : rs)) (SomeIT (a :: IT a)) = do
  tcInstr p_ (SomeIT a) >>= \case
    p ::: ((_ :: IT a'), (b :: IT b)) ->
      typeCheckImpl @cp tcInstr (r : rs) (SomeIT b) >>= \case
        q ::: ((_ :: IT b'), c) -> do
          Refl <- liftEither $ eqT' @a @a' `onLeft` TCOtherError
          Refl <- liftEither $ eqT' @b @b' `onLeft` TCOtherError
          pure $ (Seq p q) ::: (a, c)
        SiFail -> pure SiFail
    SiFail -> pure SiFail
typeCheckImpl _ [] (SomeIT s) = pure $ Nop ::: (s, s)

--------------------------------------------
-- Some generic instruction implementation
--------------------------------------------

-- | Generic implementation for MEMeration
memImpl
  :: forall (q :: CT) (c :: T) ts cp nop.
    (Typeable ts, Typeable q, Typeable (MemOpKey c), MemOp c)
  => M.Instr nop -> IT ('T_c q ': c ': ts) -> VarAnn
  -> Either (TCError nop) (SomeInstr cp)
memImpl instr i@(_ ::& _ ::& rs) vn =
  case eqT' @q @(MemOpKey c) of
    Right Refl -> pure (MEM ::: (i, (ST_c ST_bool, NStar, vn) ::& rs))
    Left m     -> typeCheckInstrErr instr (SomeIT i) $
                "query element type is not equal to set's element type: " <> m

getImpl
  :: forall c cp getKey rs nop .
    ( GetOp c, Typeable (GetOpKey c)
    , Typeable (GetOpVal c)
    )
  => M.Instr nop
  -> IT (getKey ': c ': rs)
  -> Sing (GetOpVal c)
  -> Notes (GetOpVal c)
  -> VarAnn
  -> Either (TCError nop) (SomeInstr cp)
getImpl instr i@(_ ::& _ ::& rs) rt vns vn = do
  case eqT' @getKey @('T_c (GetOpKey c)) of
    Right Refl -> do
      let rn = mkNotes $ NT_option def def vns
      pure $ GET ::: (i, (ST_option rt, rn, vn) ::& rs)
    Left m -> typeCheckInstrErr instr (SomeIT i) $
                    "wrong key stack type" <> m

updImpl
  :: forall c cp updKey updParams rs nop .
    (UpdOp c, Typeable (UpdOpKey c), Typeable (UpdOpParams c))
  => M.Instr nop
  -> IT (updKey ': updParams ': c ': rs)
  -> Either (TCError nop) (SomeInstr cp)
updImpl instr i@(_ ::& _ ::& crs) = do
  case (eqT' @updKey @('T_c (UpdOpKey c)), eqT' @updParams @(UpdOpParams c)) of
    (Right Refl, Right Refl) -> pure $ UPDATE ::: (i, crs)
    (Left m, _) -> typeCheckInstrErr instr (SomeIT i) $
                      "wrong key stack type" <> m
    (_, Left m) -> typeCheckInstrErr instr (SomeIT i) $
                      "wrong update value stack type" <> m

sizeImpl
  :: SizeOp c
  => IT (c ': rs) -> VarAnn -> Either (TCError nop) (SomeInstr cp)
sizeImpl i@(_ ::& rs) vn =
  pure $ SIZE ::: (i, (ST_c ST_nat, NStar, vn) ::& rs)

sliceImpl
  :: (SliceOp c, Typeable c)
  => IT ('T_c 'T_nat : 'T_c 'T_nat : c : rs)
  -> M.VarAnn -> Either (TCError nop) (SomeInstr cp)
sliceImpl i@(_ ::& _ ::& (c, cn, cvn) ::& rs) vn = do
  let vn' = vn `orAnn` deriveVN "slice" cvn
      rn = mkNotes $ NT_option def def cn
  pure $ SLICE ::: (i, (ST_option c, rn, vn') ::& rs)

concatImpl'
  :: (ConcatOp c, Typeable c)
  => IT ('T_list c : rs)
  -> M.VarAnn -> Either (TCError nop) (SomeInstr cp)
concatImpl' i@((ST_list c, ln, _) ::& rs) vn = do
  let cn = notesCase NStar (\(NT_list _ n) -> n) ln
  pure $ CONCAT' ::: (i, (c, cn, vn) ::& rs)

concatImpl
  :: (ConcatOp c, Typeable c)
  => IT (c : c : rs)
  -> M.VarAnn -> Either (TCError nop) (SomeInstr cp)
concatImpl i@((c, cn1, _) ::& (_, cn2, _) ::& rs) vn = do
  cn <- converge cn1 cn2 `onLeft` TCFailedOnInstr (M.CONCAT vn) (SomeIT i)
  pure $ CONCAT ::: (i, (c, cn, vn) ::& rs)

-- | Helper function to construct instructions for binary arithmetic
--erations.
arithImpl
  :: ( Typeable (ArithRes aop n m)
     , SingI (ArithRes aop n m)
     , Typeable ('T_c (ArithRes aop n m) ': s)
     )
  => Instr cp ('T_c n ': 'T_c m ': s) ('T_c (ArithRes aop n m) ': s)
  -> IT ('T_c n ': 'T_c m ': s)
  -> VarAnn
  -> Either (TCError nop) (SomeInstr cp)
arithImpl mkInstr i@(_ ::& _ ::& rs) vn =
  pure $ mkInstr ::: (i, (sing, NStar, vn) ::& rs)

addImpl
  :: (Typeable rs, Typeable a, Typeable b)
  => Sing a -> Sing b
  -> IT ('T_c a ': 'T_c b ': rs) -> VarAnn -> Either (TCError nop) (SomeInstr cp)
addImpl ST_int ST_int = arithImpl @Add ADD
addImpl ST_int ST_nat = arithImpl @Add ADD
addImpl ST_nat ST_int = arithImpl @Add ADD
addImpl ST_nat ST_nat = arithImpl @Add ADD
addImpl ST_int ST_timestamp = arithImpl @Add ADD
addImpl ST_timestamp ST_int = arithImpl @Add ADD
addImpl ST_mutez ST_mutez = arithImpl @Add ADD
addImpl _ _ = \i vn -> typeCheckInstrErr (M.ADD vn) (SomeIT i) ""

edivImpl
  :: (Typeable rs, Typeable a, Typeable b)
  => Sing a -> Sing b
  -> IT ('T_c a ': 'T_c b ': rs) -> VarAnn -> Either (TCError nop) (SomeInstr cp)
edivImpl ST_int ST_int = edivImplDo
edivImpl ST_int ST_nat = edivImplDo
edivImpl ST_nat ST_int = edivImplDo
edivImpl ST_nat ST_nat = edivImplDo
edivImpl ST_mutez ST_mutez = edivImplDo
edivImpl ST_mutez ST_nat = edivImplDo
edivImpl _ _ = \i vn -> typeCheckInstrErr (M.EDIV vn) (SomeIT i) ""

edivImplDo
  :: ( EDivOp n m
     , SingI (EModOpRes n m)
     , Typeable (EModOpRes n m)
     , SingI (EDivOpRes n m)
     , Typeable (EDivOpRes n m)
     )
  => IT ('T_c n ': 'T_c m ': s)
  -> VarAnn
  -> Either (TCError nop) (SomeInstr cp)
edivImplDo i@(_ ::& _ ::& rs) vn =
  pure $ EDIV ::: (i, (sing, NStar, vn) ::& rs)

subImpl
  :: (Typeable rs, Typeable a, Typeable b)
  => Sing a -> Sing b
  -> IT ('T_c a ': 'T_c b ': rs) -> VarAnn -> Either (TCError nop) (SomeInstr cp)
subImpl ST_int ST_int = arithImpl @Sub SUB
subImpl ST_int ST_nat = arithImpl @Sub SUB
subImpl ST_nat ST_int = arithImpl @Sub SUB
subImpl ST_nat ST_nat = arithImpl @Sub SUB
subImpl ST_timestamp ST_timestamp = arithImpl @Sub SUB
subImpl ST_timestamp ST_int = arithImpl @Sub SUB
subImpl ST_mutez ST_mutez = arithImpl @Sub SUB
subImpl _ _ = \i vn -> typeCheckInstrErr (M.SUB vn) (SomeIT i) ""

mulImpl
  :: (Typeable rs, Typeable a, Typeable b)
  => Sing a -> Sing b
  -> IT ('T_c a ': 'T_c b ': rs) -> VarAnn -> Either (TCError nop) (SomeInstr cp)
mulImpl ST_int ST_int = arithImpl @Mul MUL
mulImpl ST_int ST_nat = arithImpl @Mul MUL
mulImpl ST_nat ST_int = arithImpl @Mul MUL
mulImpl ST_nat ST_nat = arithImpl @Mul MUL
mulImpl ST_nat ST_mutez = arithImpl @Mul MUL
mulImpl ST_mutez ST_nat = arithImpl @Mul MUL
mulImpl _ _ = \i vn -> typeCheckInstrErr (M.MUL vn) (SomeIT i) ""

compareImpl
  :: (Typeable rs, Typeable a, Typeable b)
  => Sing a -> Sing b
  -> IT ('T_c a ': 'T_c b ': rs) -> VarAnn -> Either (TCError nop) (SomeInstr cp)
compareImpl ST_bool ST_bool = arithImpl @Compare COMPARE
compareImpl ST_nat ST_nat = arithImpl @Compare COMPARE
compareImpl ST_address ST_address = arithImpl @Compare COMPARE
compareImpl ST_int ST_int = arithImpl @Compare COMPARE
compareImpl ST_string ST_string = arithImpl @Compare COMPARE
compareImpl ST_bytes ST_bytes = arithImpl @Compare COMPARE
compareImpl ST_timestamp ST_timestamp = arithImpl @Compare COMPARE
compareImpl ST_key_hash ST_key_hash = arithImpl @Compare COMPARE
compareImpl ST_mutez ST_mutez = arithImpl @Compare COMPARE
compareImpl _ _ = \i vn -> typeCheckInstrErr (M.COMPARE vn) (SomeIT i) ""

-- | Helper function to construct instructions for binary arithmetic
--erations.
unaryArithImpl
  :: ( Typeable (UnaryArithRes aop n)
     , SingI (UnaryArithRes aop n)
     , Typeable ('T_c (UnaryArithRes aop n) ': s)
     )
  => Instr cp ('T_c n ': s) ('T_c (UnaryArithRes aop n) ': s)
  -> IT ('T_c n ': s)
  -> VarAnn
  -> Either (TCError nop) (SomeInstr cp)
unaryArithImpl mkInstr i@(_ ::& rs) vn =
  pure $ mkInstr ::: (i, (sing, NStar, vn) ::& rs)
