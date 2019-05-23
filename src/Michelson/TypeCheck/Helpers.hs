module Michelson.TypeCheck.Helpers
    ( onLeft
    , deriveSpecialVN
    , deriveSpecialFNs
    , deriveVN
    , deriveNsOr
    , deriveNsOption
    , convergeHSTEl
    , convergeHST
    , hstToTs
    , eqHST
    , eqHST1
    , lengthHST

    , ensureDistinctAsc
    , eqType
    , checkEqT
    , checkEqHST
    , onTypeCheckInstrAnnErr
    , onTypeCheckInstrTypeErr
    , onTypeCheckInstrErr
    , typeCheckInstrErr
    , typeCheckImpl
    , compareTypes

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

import Control.Monad.Except (MonadError, liftEither, throwError)
import Data.Default (def)
import Data.Singletons (SingI(sing), demote)
import qualified Data.Text as T
import Data.Typeable ((:~:)(..), eqT)
import Fmt ((+||), (||+))

import Michelson.ErrorPos (InstrCallStack)
import Michelson.TypeCheck.Error (TCError(..), TCTypeError(..))
import Michelson.TypeCheck.TypeCheck
import Michelson.TypeCheck.Types
import Michelson.Typed
  (CT(..), Instr(..), Notes(..), Notes'(..), Sing(..), T(..), converge, extractNotes, fromSingT,
  fromUType, mkNotes, notesCase, orAnn, withSomeSingT)
import Michelson.Typed.Annotation (AnnConvergeError)
import Michelson.Typed.Arith (Add, ArithOp(..), Compare, Mul, Sub, UnaryArithOp(..))
import Michelson.Typed.Extract (TypeConvergeError)
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
    fns = fst <$> takeWhile (uncurry (==)) (zip ps qs)
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
  -> Either AnnConvergeError (Sing t, Notes t, VarAnn)
convergeHSTEl (at, an, avn) (_, bn, bvn) =
  (,,) at <$> converge an bn
          <*> pure (bool def avn $ avn == bvn)

-- | Combine annotations from two given stack types
convergeHST :: HST ts -> HST ts -> Either AnnConvergeError (HST ts)
convergeHST SNil SNil = pure SNil
convergeHST (a ::& as) (b ::& bs) =
    liftA2 (::&) (convergeHSTEl a b) (convergeHST as bs)

-- TODO move to Util module
onLeft :: Either a c -> (a -> b) -> Either b c
onLeft = flip first

-- | Extract singleton for each single type of the given stack.
hstToTs :: HST st -> [T]
hstToTs = \case
  SNil -> []
  (s, _, _) ::& hst -> fromSingT s : hstToTs hst

-- | Check whether the given stack types are equal.
eqHST
  :: forall as bs.
      (Typeable as, Typeable bs)
  => HST as -> HST bs -> Either TCTypeError (as :~: bs)
eqHST (hst :: HST xs) (hst' :: HST ys) = do
  case eqT @as @bs of
    Nothing -> Left $ StackEqError (hstToTs hst) (hstToTs hst')
    Just Refl -> do
      void $ convergeHST hst hst' `onLeft` AnnError
      return Refl

-- | Check whether the given stack has size 1 and its only element matches the
-- given type. This function is a specialized version of `eqHST`.
eqHST1
  :: forall t st.
      (Typeable st, Typeable t, SingI t)
  => HST st -> Either TCTypeError (st :~: '[t])
eqHST1 hst = do
  let hst' = sing @t -:& SNil
  case eqT @'[t] @st of
    Nothing -> Left $ StackEqError (hstToTs hst') (hstToTs hst)
    Just Refl -> Right Refl

lengthHST :: HST xs -> Natural
lengthHST (_ ::& xs) = 1 + lengthHST xs
lengthHST SNil = 0

--------------------------------------------
-- Typechecker auxiliary
--------------------------------------------

-- | Check whether elements go in strictly ascending order and
-- return the original list (to keep only one pass on the original list).
ensureDistinctAsc :: (Ord b, Show a) => (a -> b) -> [a] -> Either Text [a]
ensureDistinctAsc toCmp = \case
  (e1 : e2 : l) ->
    if toCmp e1 < toCmp e2
    then (e1 :) <$> ensureDistinctAsc toCmp (e2 : l)
    else Left $ "Entries are unordered (" +|| e1 ||+ " >= " +|| e2 ||+ ")"
  l -> Right l

checkEqT
  :: forall (a :: T) (b :: T) ts m .
  ( Each [Typeable, SingI] [a, b], Typeable ts
  , MonadReader InstrCallStack m, MonadError TCError m
  )
  => Un.ExpandedInstr
  -> HST ts
  -> Text
  -> m (a :~: b)
checkEqT instr i m = do
  pos <- ask
  liftEither $ eqType @a @b `onLeft` (TCFailedOnInstr instr (SomeHST i) (m <> ": ") pos . Just)

-- | Function @eqType@ is a simple wrapper around @Data.Typeable.eqT@ suited
-- for use within @Either TCTypeError a@ applicative.
eqType
  :: forall (a :: T) (b :: T).
      (Each [Typeable, SingI] [a, b])
  => Either TCTypeError (a :~: b)
eqType = maybe (Left $ TypeEqError (demote @a) (demote @b)) pure eqT

checkEqHST
  :: forall (a :: [T]) (b :: [T]) ts m .
  ( Typeable a, Typeable b, Typeable ts
  , MonadReader InstrCallStack m, MonadError TCError m
  )
  => HST a
  -> HST b
  -> Un.ExpandedInstr
  -> HST ts
  -> Text
  -> m (a :~: b)
checkEqHST a b instr i m = do
  pos <- ask
  liftEither $ eqHST a b `onLeft` (TCFailedOnInstr instr (SomeHST i) (m <> ": ") pos . Just)

onTypeCheckInstrErr
  :: (MonadReader InstrCallStack m, MonadError TCError m)
  => Un.ExpandedInstr -> SomeHST -> Text -> Either TCTypeError a -> m a
onTypeCheckInstrErr instr hst msg ei = do
  pos <- ask
  liftEither $ ei `onLeft` (TCFailedOnInstr instr hst msg pos . Just)

typeCheckInstrErr
  :: (MonadReader InstrCallStack m, MonadError TCError m)
  => Un.ExpandedInstr -> SomeHST -> Text -> m a
typeCheckInstrErr instr hst msg = do
  pos <- ask
  throwError $ TCFailedOnInstr instr hst msg pos Nothing

onTypeCheckInstrAnnErr
  :: (MonadReader InstrCallStack m, MonadError TCError m, Typeable ts)
  => Un.ExpandedInstr -> HST ts -> Text -> Either AnnConvergeError a -> m a
onTypeCheckInstrAnnErr instr i msg ei =
  onTypeCheckInstrErr instr (SomeHST i) msg (ei `onLeft` AnnError)

onTypeCheckInstrTypeErr
  :: (MonadReader InstrCallStack m, MonadError TCError m, Typeable ts)
  => Un.ExpandedInstr -> HST ts -> Text -> Either TypeConvergeError a -> m a
onTypeCheckInstrTypeErr instr i msg ei =
  onTypeCheckInstrErr instr (SomeHST i) msg (ei `onLeft` ExtractionTypeMismatch)

typeCheckImpl
  :: forall inp . Typeable inp
  => TcInstrHandler
  -> [Un.ExpandedOp]
  -> HST inp
  -> TypeCheckInstr (SomeInstr inp)
typeCheckImpl tcInstr instrs t@(a :: HST a) =
  case instrs of
    Un.WithSrcEx _ (i@(Un.WithSrcEx _ _)) : rs -> typeCheckImpl tcInstr (i : rs) t
    Un.WithSrcEx cs (Un.PrimEx i) : rs -> typeCheckPrim (Just cs) i rs
    Un.WithSrcEx cs (Un.SeqEx sq) : rs -> typeCheckSeq (Just cs) sq rs
    Un.PrimEx i : rs                 -> typeCheckPrim Nothing i rs
    Un.SeqEx sq : rs                 -> typeCheckSeq Nothing sq rs
    []                               -> pure $ a :/ Nop ::: a
  where
    typeCheckPrim (Just cs) i [] = local (const cs) $ tcInstr i t
    typeCheckPrim (Just cs) i rs = local (const cs) $ typeCheckImplDo (tcInstr i t) id rs
    typeCheckPrim Nothing i [] = tcInstr i t
    typeCheckPrim Nothing i rs = typeCheckImplDo (tcInstr i t) id rs

    typeCheckSeq (Just cs) sq = local (const cs) . typeCheckImplDo (typeCheckImpl tcInstr sq t) Nested
    typeCheckSeq Nothing sq = typeCheckImplDo (typeCheckImpl tcInstr sq t) Nested

    typeCheckImplDo
      :: TypeCheckInstr (SomeInstr inp)
      -> (forall inp' out . Instr inp' out -> Instr inp' out)
      -> [Un.ExpandedOp]
      -> TypeCheckInstr (SomeInstr inp)
    typeCheckImplDo f wrap rs = do
      _ :/ pi' <- f
      case pi' of
        p ::: b -> do
          _ :/ qi <- typeCheckImpl tcInstr rs b
          case qi of
            q ::: c ->
              pure $ a :/ Seq (wrap p) q ::: c
            AnyOutInstr q ->
              pure $ a :/ AnyOutInstr (Seq (wrap p) q)

        AnyOutInstr instr ->
          case rs of
            [] ->
              pure $ a :/ AnyOutInstr instr
            r : rr ->
              throwError $ TCUnreachableCode (extractInstrPos r) (r :| rr)

    extractInstrPos :: Un.ExpandedOp -> InstrCallStack
    extractInstrPos (Un.WithSrcEx cs _) = cs
    extractInstrPos _ = def

-- | Check whether typed and untyped types converge
compareTypes
  :: forall t.
      (Typeable t, SingI t)
  => (Sing t, Notes t) -> Un.Type -> Either TCTypeError ()
compareTypes (_, n) tp = withSomeSingT (fromUType tp) $ \(t :: Sing ct) -> do
  Refl <- eqType @t @ct
  cnotes <- extractNotes tp t `onLeft` ExtractionTypeMismatch
  void $ converge n cnotes `onLeft` AnnError

--------------------------------------------
-- Some generic instruction implementation
--------------------------------------------

-- | Generic implementation for MEMeration
memImpl
  :: forall (q :: CT) (c :: T) ts inp m .
    ( MonadReader InstrCallStack m, MonadError TCError m, Typeable ts
    , Typeable (MemOpKey c), SingI (MemOpKey c), MemOp c
    , inp ~ ('Tc q : c : ts)
    )
  => Un.ExpandedInstr
  -> HST inp
  -> VarAnn
  -> m (SomeInstr inp)
memImpl instr i@(_ ::& _ ::& rs) vn = do
  pos <- ask
  case eqType @('Tc q) @('Tc (MemOpKey c)) of
    Right Refl -> pure $ i :/ MEM ::: ((STc SCBool, NStar, vn) ::& rs)
    Left m     -> throwError $
      TCFailedOnInstr instr (SomeHST i) "query element type is not equal to set's element type" pos (Just m)

getImpl
  :: forall c getKey rs inp m .
    ( GetOp c, Typeable (GetOpKey c)
    , Typeable (GetOpVal c)
    , SingI (GetOpVal c), SingI (GetOpKey c)
    , inp ~ (getKey : c : rs)
    , MonadReader InstrCallStack m
    , MonadError TCError m
    )
  => Un.ExpandedInstr
  -> HST (getKey ': c ': rs)
  -> Sing (GetOpVal c)
  -> Notes (GetOpVal c)
  -> VarAnn
  -> m (SomeInstr inp)
getImpl instr i@(_ ::& _ ::& rs) rt vns vn = do
  pos <- ask
  case eqType @getKey @('Tc (GetOpKey c)) of
    Right Refl -> do
      let rn = mkNotes $ NTOption def def vns
      pure $ i :/ GET ::: ((STOption rt, rn, vn) ::& rs)
    Left m -> throwError $ TCFailedOnInstr instr (SomeHST i) "wrong key stack type" pos (Just m)

updImpl
  :: forall c updKey updParams rs inp m .
    ( UpdOp c
    , Typeable (UpdOpKey c), SingI (UpdOpKey c)
    , Typeable (UpdOpParams c), SingI (UpdOpParams c)
    , inp ~ (updKey : updParams : c : rs)
    , MonadReader InstrCallStack m
    , MonadError TCError m
    )
  => Un.ExpandedInstr
  -> HST (updKey ': updParams ': c ': rs)
  -> m (SomeInstr inp)
updImpl instr i@(_ ::& _ ::& crs) = do
  pos <- ask
  case (eqType @updKey @('Tc (UpdOpKey c)), eqType @updParams @(UpdOpParams c)) of
    (Right Refl, Right Refl) -> pure $ i :/ UPDATE ::: crs
    (Left m, _) -> throwError $ TCFailedOnInstr instr (SomeHST i)
                      "wrong key stack type" pos (Just m)
    (_, Left m) -> throwError $ TCFailedOnInstr instr (SomeHST i)
                      "wrong update value stack type" pos (Just m)

sizeImpl
  :: (SizeOp c, inp ~ (c ': rs), Monad m)
  => HST inp
  -> VarAnn
  -> m (SomeInstr inp)
sizeImpl i@(_ ::& rs) vn = pure $ i :/ SIZE ::: ((STc SCNat, NStar, vn) ::& rs)

sliceImpl
  :: (SliceOp c, Typeable c, inp ~ ('Tc 'CNat ': 'Tc 'CNat ': c ': rs), Monad m)
  => HST inp
  -> Un.VarAnn
  -> m (SomeInstr inp)
sliceImpl i@(_ ::& _ ::& (c, cn, cvn) ::& rs) vn = do
  let vn' = vn `orAnn` deriveVN "slice" cvn
      rn = mkNotes $ NTOption def def cn
  pure $ i :/ SLICE ::: ((STOption c, rn, vn') ::& rs)

concatImpl'
  :: (ConcatOp c, Typeable c, inp ~ ('TList c : rs), Monad m)
  => HST inp
  -> Un.VarAnn
  -> m (SomeInstr inp)
concatImpl' i@((STList c, ln, _) ::& rs) vn = do
  let cn = notesCase NStar (\(NTList _ n) -> n) ln
  pure $ i :/ CONCAT' ::: ((c, cn, vn) ::& rs)

concatImpl
  :: ( ConcatOp c, Typeable c, inp ~ (c ': c ': rs)
     , MonadReader InstrCallStack m
     , MonadError TCError m
     )
  => HST inp
  -> Un.VarAnn
  -> m (SomeInstr inp)
concatImpl i@((c, cn1, _) ::& (_, cn2, _) ::& rs) vn = do
  cn <- onTypeCheckInstrAnnErr (Un.CONCAT vn) i "wrong operand types for concat operation" (converge cn1 cn2)
  pure $ i :/ CONCAT ::: ((c, cn, vn) ::& rs)

-- | Helper function to construct instructions for binary arithmetic
-- operations.
arithImpl
  :: ( Typeable (ArithRes aop n m)
     , SingI (ArithRes aop n m)
     , Typeable ('Tc (ArithRes aop n m) ': s)
     , inp ~ ('Tc n ': 'Tc m ': s)
     , Monad t
     )
  => Instr inp ('Tc (ArithRes aop n m) ': s)
  -> HST inp
  -> VarAnn
  -> t (SomeInstr inp)
arithImpl mkInstr i@(_ ::& _ ::& rs) vn = pure $ i :/ mkInstr ::: ((sing, NStar, vn) ::& rs)

addImpl
  :: forall a b inp rs m.
     ( Typeable rs
     , Each [Typeable, SingI] [a, b]
     , inp ~ ('Tc a ': 'Tc b ': rs)
     , MonadReader InstrCallStack m
     , MonadError TCError m
     )
  => Sing a -> Sing b
  -> HST inp
  -> VarAnn
  -> m (SomeInstr inp)
addImpl SCInt SCInt = arithImpl @Add ADD
addImpl SCInt SCNat = arithImpl @Add ADD
addImpl SCNat SCInt = arithImpl @Add ADD
addImpl SCNat SCNat = arithImpl @Add ADD
addImpl SCInt SCTimestamp = arithImpl @Add ADD
addImpl SCTimestamp SCInt = arithImpl @Add ADD
addImpl SCMutez SCMutez = arithImpl @Add ADD
addImpl _ _ = \i vn -> onTypeCheckInstrErr (Un.ADD vn) (SomeHST i)
  "wrong operand types for add operation"
  (Left $ UnsupportedTypes [demote @('Tc a), demote @('Tc b)])

edivImpl
  :: forall a b inp rs m.
     ( Typeable rs
     , Each [Typeable, SingI] [a, b]
     , inp ~ ('Tc a ': 'Tc b ': rs)
     , MonadReader InstrCallStack m
     , MonadError TCError m
     )
  => Sing a -> Sing b
  -> HST inp
  -> VarAnn
  -> m (SomeInstr inp)
edivImpl SCInt SCInt = edivImplDo
edivImpl SCInt SCNat = edivImplDo
edivImpl SCNat SCInt = edivImplDo
edivImpl SCNat SCNat = edivImplDo
edivImpl SCMutez SCMutez = edivImplDo
edivImpl SCMutez SCNat = edivImplDo
edivImpl _ _ = \i vn -> onTypeCheckInstrErr (Un.EDIV vn) (SomeHST i)
  "wrong operand types for ediv operation"
  (Left $ UnsupportedTypes [demote @('Tc a), demote @('Tc b)])

edivImplDo
  :: ( EDivOp n m
     , SingI (EModOpRes n m)
     , Typeable (EModOpRes n m)
     , SingI (EDivOpRes n m)
     , Typeable (EDivOpRes n m)
     , inp ~ ('Tc n ': 'Tc m ': s)
     , Monad t
     )
  => HST inp
  -> VarAnn
  -> t (SomeInstr inp)
edivImplDo i@(_ ::& _ ::& rs) vn =
  pure $ i :/ EDIV ::: ((sing, NStar, vn) ::& rs)

subImpl
  :: forall a b inp rs m.
     ( Typeable rs
     , Each [Typeable, SingI] [a, b]
     , inp ~ ('Tc a ': 'Tc b ': rs)
     , MonadReader InstrCallStack m
     , MonadError TCError m
     )
  => Sing a -> Sing b
  -> HST inp
  -> VarAnn
  -> m (SomeInstr inp)
subImpl SCInt SCInt = arithImpl @Sub SUB
subImpl SCInt SCNat = arithImpl @Sub SUB
subImpl SCNat SCInt = arithImpl @Sub SUB
subImpl SCNat SCNat = arithImpl @Sub SUB
subImpl SCTimestamp SCTimestamp = arithImpl @Sub SUB
subImpl SCTimestamp SCInt = arithImpl @Sub SUB
subImpl SCMutez SCMutez = arithImpl @Sub SUB
subImpl _ _ = \i vn -> onTypeCheckInstrErr (Un.SUB vn) (SomeHST i)
  "wrong operand types for sub operation"
  (Left $ UnsupportedTypes [demote @('Tc a), demote @('Tc b)])

mulImpl
  :: forall a b inp rs m.
     ( Typeable rs
     , Each [Typeable, SingI] [a, b]
     , inp ~ ('Tc a ': 'Tc b ': rs)
     , MonadReader InstrCallStack m
     , MonadError TCError m
     )
  => Sing a -> Sing b
  -> HST inp
  -> VarAnn
  -> m (SomeInstr inp)
mulImpl SCInt SCInt = arithImpl @Mul MUL
mulImpl SCInt SCNat = arithImpl @Mul MUL
mulImpl SCNat SCInt = arithImpl @Mul MUL
mulImpl SCNat SCNat = arithImpl @Mul MUL
mulImpl SCNat SCMutez = arithImpl @Mul MUL
mulImpl SCMutez SCNat = arithImpl @Mul MUL
mulImpl _ _ = \i vn -> onTypeCheckInstrErr (Un.MUL vn) (SomeHST i)
  "wrong operand types for mul operation"
  (Left $ UnsupportedTypes [demote @('Tc a), demote @('Tc b)])

compareImpl
  :: forall a b inp rs m.
     ( Typeable rs
     , Each [Typeable, SingI] [a, b]
     , inp ~ ('Tc a ': 'Tc b ': rs)
     , MonadReader InstrCallStack m
     , MonadError TCError m
     )
  => Sing a -> Sing b
  -> HST inp
  -> VarAnn
  -> m (SomeInstr inp)
compareImpl SCBool SCBool = arithImpl @Compare COMPARE
compareImpl SCNat SCNat = arithImpl @Compare COMPARE
compareImpl SCAddress SCAddress = arithImpl @Compare COMPARE
compareImpl SCInt SCInt = arithImpl @Compare COMPARE
compareImpl SCString SCString = arithImpl @Compare COMPARE
compareImpl SCBytes SCBytes = arithImpl @Compare COMPARE
compareImpl SCTimestamp SCTimestamp = arithImpl @Compare COMPARE
compareImpl SCKeyHash SCKeyHash = arithImpl @Compare COMPARE
compareImpl SCMutez SCMutez = arithImpl @Compare COMPARE
compareImpl _ _ = \i vn -> onTypeCheckInstrErr (Un.COMPARE vn) (SomeHST i)
  "wrong operand types for compare operation"
  (Left $ UnsupportedTypes [demote @('Tc a), demote @('Tc b)])

-- | Helper function to construct instructions for binary arithmetic
-- operations.
unaryArithImpl
  :: ( Typeable (UnaryArithRes aop n)
     , SingI (UnaryArithRes aop n)
     , Typeable ('Tc (UnaryArithRes aop n) ': s)
     , inp ~ ('Tc n ': s)
     , Monad t
     )
  => Instr inp ('Tc (UnaryArithRes aop n) ': s)
  -> HST inp
  -> VarAnn
  -> t (SomeInstr inp)
unaryArithImpl mkInstr i@(_ ::& rs) vn =
  pure $ i :/ mkInstr ::: ((sing, NStar, vn) ::& rs)
