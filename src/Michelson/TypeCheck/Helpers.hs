module Michelson.TypeCheck.Helpers
    ( onLeft
    , deriveSpecialVN
    , deriveSpecialFNs
    , deriveVN
    , deriveNsOr
    , deriveNsOption
    , convergeHSTEl
    , convergeHST

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

import qualified Michelson.Untyped as Un
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
deriveSpecialFNs "@" "@" (Un.Annotation pvn) (Un.Annotation qvn) = (vn, pfn, qfn)
  where
    ps = T.splitOn "." pvn
    qs = T.splitOn "." qvn
    fns = fst <$> takeWhile (\(a, b) -> a == b) (zip ps qs)
    vn = Un.Annotation $ T.intercalate "." fns
    pfn = Un.Annotation $ T.intercalate "." $ drop (length fns) ps
    qfn = Un.Annotation $ T.intercalate "." $ drop (length fns) qs
deriveSpecialFNs "@" qfn pvn _   = (def, Un.convAnn pvn, qfn)
deriveSpecialFNs pfn "@" _ qvn   = (def, pfn, Un.convAnn qvn)
deriveSpecialFNs pfn qfn _ _     = (def, pfn, qfn)

-- | Function which derives special annotations
-- for CDR / CAR instructions.
deriveSpecialVN :: VarAnn -> FieldAnn -> VarAnn -> VarAnn
deriveSpecialVN vn elFn pairVN
  | vn == "%" = Un.convAnn elFn
  | vn == "%%" && elFn /= def = pairVN <> Un.convAnn elFn
  | otherwise = vn

-- | Append suffix to variable annotation (if it's not empty)
deriveVN :: VarAnn -> VarAnn -> VarAnn
deriveVN suffix vn = bool (suffix <> vn) def (vn == def)

-- | Function which extracts annotations for @or@ type
-- (for left and right parts).
--
-- It extracts field/type annotations and also auto-generates variable
-- annotations if variable annotation is not provided as second argument.
deriveNsOr :: Notes ('TOr a b) -> VarAnn -> (Notes a, Notes b, VarAnn, VarAnn)
deriveNsOr ons ovn =
  let (an, bn, afn, bfn) =
        notesCase (NStar, NStar, def, def)
          (\(NTOr _ afn_ bfn_ an_ bn_) -> (an_, bn_, afn_, bfn_)) ons
      avn = deriveVN (Un.convAnn afn `orAnn` "left") ovn
      bvn = deriveVN (Un.convAnn bfn `orAnn` "right") ovn
   in (an, bn, avn, bvn)

-- | Function which extracts annotations for @option t@ type.
--
-- It extracts field/type annotations and also auto-generates variable
-- annotation for @Some@ case if it is not provided as second argument.
deriveNsOption :: Notes ('TOption a) -> VarAnn -> (Notes a, VarAnn)
deriveNsOption ons ovn =
  let (an, fn) = notesCase (NStar, def)
                    (\(NTOption _ fn_ an_) -> (an_, fn_)) ons
      avn = deriveVN (Un.convAnn fn `orAnn` "some") ovn
   in (an, avn)

convergeHSTEl
  :: (Sing t, Notes t, VarAnn)
  -> (Sing t, Notes t, VarAnn)
  -> Either Text (Sing t, Notes t, VarAnn)
convergeHSTEl (at, an, avn) (_, bn, bvn) =
  (,,) at <$> converge an bn
          <*> pure (bool def avn $ avn == bvn)

-- | Combine annotations from two given stack types
convergeHST :: HST ts -> HST ts -> Either Text (HST ts)
convergeHST SNil SNil = pure SNil
convergeHST (a ::& as) (b ::& bs) =
    liftA2 (::&) (convergeHSTEl a b) (convergeHST as bs)

-- TODO move to Util module
onLeft :: Either a c -> (a -> b) -> Either b c
onLeft = flip first

--------------------------------------------
-- Typechecker auxiliary
--------------------------------------------

checkEqT
  :: forall a b ts . (Typeable a, Typeable b, Typeable ts)
  => Un.Instr
  -> HST ts
  -> Text
  -> Either TCError (a :~: b)
checkEqT instr i m =
  eqT' @a @b `onLeft` (TCFailedOnInstr instr (SomeHST i) . ((m <> ": ") <>))

assertEqT
  :: forall a b ts . (Typeable a, Typeable b, Typeable ts)
  => Un.Instr
  -> HST ts
  -> Either TCError (a :~: b)
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

typeCheckInstrErr :: Un.Instr -> SomeHST -> Text -> Either TCError a
typeCheckInstrErr = Left ... TCFailedOnInstr

typeCheckInstrErrM :: Un.Instr -> SomeHST -> Text -> TypeCheckT a
typeCheckInstrErrM = throwError ... TCFailedOnInstr

typeCheckImpl
  :: TcInstrHandler
  -> [Un.Instr]
  -> SomeHST
  -> TypeCheckT SomeInstr
typeCheckImpl tcInstr [a] t = tcInstr a t
typeCheckImpl tcInstr (p_ : (r : rs)) (SomeHST (a :: HST a)) = do
  tcInstr p_ (SomeHST a) >>= \case
    p ::: ((_ :: HST a'), (b :: HST b)) ->
      typeCheckImpl tcInstr (r : rs) (SomeHST b) >>= \case
        q ::: ((_ :: HST b'), c) -> do
          Refl <- liftEither $ eqT' @a @a' `onLeft` TCOtherError
          Refl <- liftEither $ eqT' @b @b' `onLeft` TCOtherError
          pure $ (Seq p q) ::: (a, c)
        SiFail -> pure SiFail
    SiFail -> pure SiFail
typeCheckImpl _ [] (SomeHST s) = pure $ Nop ::: (s, s)

--------------------------------------------
-- Some generic instruction implementation
--------------------------------------------

-- | Generic implementation for MEMeration
memImpl
  :: forall (q :: CT) (c :: T) ts .
    (Typeable ts, Typeable q, Typeable (MemOpKey c), MemOp c)
  => Un.Instr
  -> HST ('Tc q ': c ': ts)
  -> VarAnn
  -> TcResult
memImpl instr i@(_ ::& _ ::& rs) vn =
  case eqT' @q @(MemOpKey c) of
    Right Refl -> pure (MEM ::: (i, (STc SCBool, NStar, vn) ::& rs))
    Left m     -> typeCheckInstrErr instr (SomeHST i) $
                "query element type is not equal to set's element type: " <> m

getImpl
  :: forall c getKey rs .
    ( GetOp c, Typeable (GetOpKey c)
    , Typeable (GetOpVal c)
    , SingI (GetOpVal c)
    )
  => Un.Instr
  -> HST (getKey ': c ': rs)
  -> Sing (GetOpVal c)
  -> Notes (GetOpVal c)
  -> VarAnn
  -> TcResult
getImpl instr i@(_ ::& _ ::& rs) rt vns vn = do
  case eqT' @getKey @('Tc (GetOpKey c)) of
    Right Refl -> do
      let rn = mkNotes $ NTOption def def vns
      pure $ GET ::: (i, (STOption rt, rn, vn) ::& rs)
    Left m -> typeCheckInstrErr instr (SomeHST i) $
                    "wrong key stack type" <> m

updImpl
  :: forall c updKey updParams rs .
    (UpdOp c, Typeable (UpdOpKey c), Typeable (UpdOpParams c))
  => Un.Instr
  -> HST (updKey ': updParams ': c ': rs)
  -> TcResult
updImpl instr i@(_ ::& _ ::& crs) = do
  case (eqT' @updKey @('Tc (UpdOpKey c)), eqT' @updParams @(UpdOpParams c)) of
    (Right Refl, Right Refl) -> pure $ UPDATE ::: (i, crs)
    (Left m, _) -> typeCheckInstrErr instr (SomeHST i) $
                      "wrong key stack type" <> m
    (_, Left m) -> typeCheckInstrErr instr (SomeHST i) $
                      "wrong update value stack type" <> m

sizeImpl
  :: SizeOp c
  => HST (c ': rs)
  -> VarAnn
  -> TcResult
sizeImpl i@(_ ::& rs) vn =
  pure $ SIZE ::: (i, (STc SCNat, NStar, vn) ::& rs)

sliceImpl
  :: (SliceOp c, Typeable c)
  => HST ('Tc 'CNat : 'Tc 'CNat : c : rs)
  -> Un.VarAnn
  -> TcResult
sliceImpl i@(_ ::& _ ::& (c, cn, cvn) ::& rs) vn = do
  let vn' = vn `orAnn` deriveVN "slice" cvn
      rn = mkNotes $ NTOption def def cn
  pure $ SLICE ::: (i, (STOption c, rn, vn') ::& rs)

concatImpl'
  :: (ConcatOp c, Typeable c)
  => HST ('TList c : rs)
  -> Un.VarAnn
  -> TcResult
concatImpl' i@((STList c, ln, _) ::& rs) vn = do
  let cn = notesCase NStar (\(NTList _ n) -> n) ln
  pure $ CONCAT' ::: (i, (c, cn, vn) ::& rs)

concatImpl
  :: (ConcatOp c, Typeable c)
  => HST (c : c : rs)
  -> Un.VarAnn
  -> TcResult
concatImpl i@((c, cn1, _) ::& (_, cn2, _) ::& rs) vn = do
  cn <- converge cn1 cn2 `onLeft` TCFailedOnInstr (Un.CONCAT vn) (SomeHST i)
  pure $ CONCAT ::: (i, (c, cn, vn) ::& rs)

-- | Helper function to construct instructions for binary arithmetic
--erations.
arithImpl
  :: ( Typeable (ArithRes aop n m)
     , SingI (ArithRes aop n m)
     , Typeable ('Tc (ArithRes aop n m) ': s)
     )
  => Instr ('Tc n ': 'Tc m ': s) ('Tc (ArithRes aop n m) ': s)
  -> HST ('Tc n ': 'Tc m ': s)
  -> VarAnn
  -> TcResult
arithImpl mkInstr i@(_ ::& _ ::& rs) vn =
  pure $ mkInstr ::: (i, (sing, NStar, vn) ::& rs)

addImpl
  :: (Typeable rs, Typeable a, Typeable b)
  => Sing a -> Sing b
  -> HST ('Tc a ': 'Tc b ': rs)
  -> VarAnn
  -> TcResult
addImpl SCInt SCInt = arithImpl @Add ADD
addImpl SCInt SCNat = arithImpl @Add ADD
addImpl SCNat SCInt = arithImpl @Add ADD
addImpl SCNat SCNat = arithImpl @Add ADD
addImpl SCInt SCTimestamp = arithImpl @Add ADD
addImpl SCTimestamp SCInt = arithImpl @Add ADD
addImpl SCMutez SCMutez = arithImpl @Add ADD
addImpl _ _ = \i vn -> typeCheckInstrErr (Un.ADD vn) (SomeHST i) ""

edivImpl
  :: (Typeable rs, Typeable a, Typeable b)
  => Sing a -> Sing b
  -> HST ('Tc a ': 'Tc b ': rs)
  -> VarAnn
  -> TcResult
edivImpl SCInt SCInt = edivImplDo
edivImpl SCInt SCNat = edivImplDo
edivImpl SCNat SCInt = edivImplDo
edivImpl SCNat SCNat = edivImplDo
edivImpl SCMutez SCMutez = edivImplDo
edivImpl SCMutez SCNat = edivImplDo
edivImpl _ _ = \i vn -> typeCheckInstrErr (Un.EDIV vn) (SomeHST i) ""

edivImplDo
  :: ( EDivOp n m
     , SingI (EModOpRes n m)
     , Typeable (EModOpRes n m)
     , SingI (EDivOpRes n m)
     , Typeable (EDivOpRes n m)
     )
  => HST ('Tc n ': 'Tc m ': s)
  -> VarAnn
  -> TcResult
edivImplDo i@(_ ::& _ ::& rs) vn =
  pure $ EDIV ::: (i, (sing, NStar, vn) ::& rs)

subImpl
  :: (Typeable rs, Typeable a, Typeable b)
  => Sing a -> Sing b
  -> HST ('Tc a ': 'Tc b ': rs)
  -> VarAnn
  -> TcResult
subImpl SCInt SCInt = arithImpl @Sub SUB
subImpl SCInt SCNat = arithImpl @Sub SUB
subImpl SCNat SCInt = arithImpl @Sub SUB
subImpl SCNat SCNat = arithImpl @Sub SUB
subImpl SCTimestamp SCTimestamp = arithImpl @Sub SUB
subImpl SCTimestamp SCInt = arithImpl @Sub SUB
subImpl SCMutez SCMutez = arithImpl @Sub SUB
subImpl _ _ = \i vn -> typeCheckInstrErr (Un.SUB vn) (SomeHST i) ""

mulImpl
  :: (Typeable rs, Typeable a, Typeable b)
  => Sing a -> Sing b
  -> HST ('Tc a ': 'Tc b ': rs)
  -> VarAnn
  -> TcResult
mulImpl SCInt SCInt = arithImpl @Mul MUL
mulImpl SCInt SCNat = arithImpl @Mul MUL
mulImpl SCNat SCInt = arithImpl @Mul MUL
mulImpl SCNat SCNat = arithImpl @Mul MUL
mulImpl SCNat SCMutez = arithImpl @Mul MUL
mulImpl SCMutez SCNat = arithImpl @Mul MUL
mulImpl _ _ = \i vn -> typeCheckInstrErr (Un.MUL vn) (SomeHST i) ""

compareImpl
  :: (Typeable rs, Typeable a, Typeable b)
  => Sing a -> Sing b
  -> HST ('Tc a ': 'Tc b ': rs)
  -> VarAnn
  -> TcResult
compareImpl SCBool SCBool = arithImpl @Compare COMPARE
compareImpl SCNat SCNat = arithImpl @Compare COMPARE
compareImpl SCAddress SCAddress = arithImpl @Compare COMPARE
compareImpl SCInt SCInt = arithImpl @Compare COMPARE
compareImpl SCString SCString = arithImpl @Compare COMPARE
compareImpl SCBytes SCBytes = arithImpl @Compare COMPARE
compareImpl SCTimestamp SCTimestamp = arithImpl @Compare COMPARE
compareImpl SCKeyHash SCKeyHash = arithImpl @Compare COMPARE
compareImpl SCMutez SCMutez = arithImpl @Compare COMPARE
compareImpl _ _ = \i vn -> typeCheckInstrErr (Un.COMPARE vn) (SomeHST i) ""

-- | Helper function to construct instructions for binary arithmetic
--erations.
unaryArithImpl
  :: ( Typeable (UnaryArithRes aop n)
     , SingI (UnaryArithRes aop n)
     , Typeable ('Tc (UnaryArithRes aop n) ': s)
     )
  => Instr ('Tc n ': s) ('Tc (UnaryArithRes aop n) ': s)
  -> HST ('Tc n ': s)
  -> VarAnn
  -> TcResult
unaryArithImpl mkInstr i@(_ ::& rs) vn =
  pure $ mkInstr ::: (i, (sing, NStar, vn) ::& rs)
