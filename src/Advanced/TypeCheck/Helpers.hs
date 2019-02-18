module Advanced.TypeCheck.Helpers
    ( fromBase58Text
    , eqT'
    , typeCheckCv
    , typeCheckIErr
    , typeCheckCvs
    , memImpl
    , getImpl
    , updImpl
    , sliceImpl
    , concatImpl
    , sizeImpl
    , deriveVN
    , deriveNsOr
    , deriveNsOption
    , convergeITEl
    , convergeIT
    , arithImpl
    , addImpl
    , subImpl
    , mulImpl
    , compareImpl
    , unaryArithImpl
    ) where

import Data.ByteString.Base58 (bitcoinAlphabet, decodeBase58)
import Data.Default (def)
import Data.Singletons (SingI(sing))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime (zonedTimeToUTC)
import Data.Time.RFC3339 (parseTimeRFC3339)
import Data.Typeable ((:~:)(..), eqT, typeRep)
import Prelude hiding (EQ, GT, LT)

import Advanced.Type
  (CT(..), Converge(converge), Notes(..), Notes'(..), Sing(..), T(..), mkNotes, notesCase, orAnn)
import Advanced.TypeCheck.Types
import Advanced.Value
  (Add, ArithOp(..), CVal(..), Compare, ConcatOp, GetOp(..), Instr(..), MemOp(..), Mul, SizeOp,
  SliceOp, Sub, UnaryArithOp(..), UpdOp(..))

import Michelson.Types (VarAnn)
import qualified Michelson.Types as M
import Tezos.Crypto (parseKeyHash)

fromBase58Text :: Text -> Maybe ByteString
fromBase58Text = decodeBase58 bitcoinAlphabet . encodeUtf8

typeCheckCv :: M.Value M.Op -> CT -> Either Text SomeValC
typeCheckCv (M.ValueInt i) T_int = pure $ CvInt i :--: ST_int
typeCheckCv (M.ValueInt i) T_nat
  | i >= 0 = pure $ CvNat (fromInteger i) :--: ST_nat
typeCheckCv (M.ValueInt (fromInteger -> i)) T_mutez
  | i <= maxBound && i >= minBound = pure $ CvMutez i :--: ST_mutez
typeCheckCv (M.ValueString s) T_string =
  pure $ CvString s :--: ST_string

typeCheckCv (M.ValueString (fromBase58Text -> Just s)) T_address =
  pure $ CvAddress s :--: ST_address
typeCheckCv (M.ValueBytes (M.InternalByteString s)) T_address =
  pure $ CvAddress s :--: ST_address

typeCheckCv (M.ValueString (parseKeyHash -> Right s)) T_key_hash =
  pure $ CvKeyHash s :--: ST_key_hash

typeCheckCv (M.ValueString (parseTimeRFC3339 -> Just zt)) T_timestamp =
  pure $ CvTimestamp (zonedTimeToUTC zt) :--: ST_timestamp
typeCheckCv (M.ValueInt i) T_timestamp = do
  let t = posixSecondsToUTCTime (fromInteger i)
  pure $ CvTimestamp t :--: ST_timestamp

typeCheckCv (M.ValueBytes (M.InternalByteString s)) T_bytes =
  pure $ CvBytes s :--: ST_bytes
typeCheckCv M.ValueTrue T_bool = pure $ CvBool True :--: ST_bool
typeCheckCv M.ValueFalse T_bool = pure $ CvBool False :--: ST_bool
typeCheckCv v t = Left $ "Error checking value " <> show v
                          <> " against type " <> show t

-- | Function @eqT'@ is a simple wrapper around @Data.Typeable.eqT@ suited
-- for use within @Either Text a@ applicative.
eqT' :: forall a_ b_ . (Typeable a_, Typeable b_) => Either Text (a_ :~: b_)
eqT' = maybe (Left $
                "types not equal: "
                  <> show (typeRep (Proxy @a_))
                  <> " /= "
                  <> show (typeRep (Proxy @b_))
                  ) pure eqT

typeCheckCvs
  :: forall t . Typeable t
  => [M.Value M.Op] -> CT -> Either Text [CVal t]
typeCheckCvs mvs t = traverse check mvs
  where
    check mv = do
      v :--: (_ :: Sing t') <- typeCheckCv mv t
      Refl <- eqT' @t @t'
      pure v

typeCheckIErr :: M.Instr -> SomeIT -> Text -> Either Text a
typeCheckIErr instr (SomeIT t) custom =
  Left $ "Error checking expression " <> show instr
          <> " against input stack type " <> show t
          <> bool (": " <> custom) "" (null custom)

-- | Generic implementation for MEM operation
memImpl
  :: forall (q :: CT) (c :: T) ts op cp.
    (Typeable ts, Typeable q, Typeable (MemOpKey c), MemOp c)
  => M.Instr -> IT ('T_c q ': c ': ts) -> VarAnn
  -> Either Text (SomeInstr op cp)
memImpl instr i@(_ ::& _ ::& rs) vn =
  case eqT' @q @(MemOpKey c) of
    Right Refl -> pure (MEM ::: (i, (ST_c ST_bool, NStar, vn) ::& rs))
    Left m -> typeCheckIErr instr (SomeIT i) $
                "query element type is not equal to set's element type: " <> m

getImpl
  :: forall c op cp getKey rs .
    ( GetOp c, Typeable (GetOpKey c)
    , Typeable (GetOpVal c), Converge (GetOpVal c)
    )
  => M.Instr
  -> IT (getKey ': c ': rs)
  -> Sing (GetOpVal c)
  -> Notes (GetOpVal c)
  -> VarAnn
  -> Either Text (SomeInstr op cp)
getImpl instr i@(_ ::& _ ::& rs) rt vns vn = do
  case eqT' @getKey @('T_c (GetOpKey c)) of
    Right Refl -> do
      let rn = mkNotes $ NT_option def def vns
      pure $ GET ::: (i, (ST_option rt, rn, vn) ::& rs)
    Left m -> typeCheckIErr instr (SomeIT i) $
                    "wrong key stack type" <> m

updImpl
  :: forall c op cp updKey updParams rs .
    (UpdOp c, Typeable (UpdOpKey c), Typeable (UpdOpParams c))
  => M.Instr
  -> IT (updKey ': updParams ': c ': rs)
  -> Either Text (SomeInstr op cp)
updImpl instr i@(_ ::& _ ::& crs) = do
  case (eqT' @updKey @('T_c (UpdOpKey c)), eqT' @updParams @(UpdOpParams c)) of
    (Right Refl, Right Refl) -> pure $ UPDATE ::: (i, crs)
    (Left m, _) -> typeCheckIErr instr (SomeIT i) $
                      "wrong key stack type" <> m
    (_, Left m) -> typeCheckIErr instr (SomeIT i) $
                      "wrong update value stack type" <> m

sizeImpl
  :: SizeOp c
  => IT (c ': rs) -> VarAnn -> Either Text (SomeInstr op cp)
sizeImpl i@(_ ::& rs) vn =
  pure $ SIZE ::: (i, (ST_c ST_nat, NStar, vn) ::& rs)

sliceImpl
  :: (SliceOp c, Typeable c, Converge c)
  => IT ('T_c 'T_nat : 'T_c 'T_nat : c : rs)
  -> M.VarAnn -> Either Text (SomeInstr op cp)
sliceImpl i@(_ ::& _ ::& (c, cn, cvn) ::& rs) vn = do
  let vn' = vn `orAnn` deriveVN "slice" cvn
      rn = mkNotes $ NT_option def def cn
  pure $ SLICE ::: (i, (ST_option c, rn, vn') ::& rs)

concatImpl
  :: (ConcatOp c, Typeable c, Converge c)
  => IT (c : c : rs)
  -> M.VarAnn -> Either Text (SomeInstr op cp)
concatImpl i@((c, cn1, _) ::& (_, cn2, _) ::& rs) vn = do
  cn <- converge cn1 cn2
  pure $ CONCAT ::: (i, (c, cn, vn) ::& rs)

-- | Append suffix to variable annotation (if it's not empty)
deriveVN :: VarAnn -> VarAnn -> VarAnn
deriveVN suffix vn = bool (suffix <> vn) def (vn == def)

deriveNsOr :: Notes ('T_or a b) -> VarAnn -> (Notes a, Notes b, VarAnn, VarAnn)
deriveNsOr ons ovn =
  let (an, bn, afn, bfn) =
        notesCase (NStar, NStar, def, def)
          (\(NT_or _ afn_ bfn_ an_ bn_) -> (an_, bn_, afn_, bfn_)) ons
      avn = deriveVN (M.convAnn afn `orAnn` "left") ovn
      bvn = deriveVN (M.convAnn bfn `orAnn` "right") ovn
   in (an, bn, avn, bvn)

deriveNsOption :: Notes ('T_option a) -> VarAnn -> (Notes a, VarAnn)
deriveNsOption ons ovn =
  let (an, fn) = notesCase (NStar, def)
                    (\(NT_option _ fn_ an_) -> (an_, fn_)) ons
      avn = deriveVN (M.convAnn fn `orAnn` "some") ovn
   in (an, avn)

convergeITEl
  :: Converge t
  => (Sing t, Notes t, VarAnn)
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

-- | Helper function to construct instructions for binary arithmetic
-- operations.
arithImpl
  :: ( Typeable (ArithResT aop n m)
     , SingI (ArithResT aop n m)
     , Typeable ('T_c (ArithResT aop n m) ': s)
     )
  => Instr op cp ('T_c n ': 'T_c m ': s) ('T_c (ArithResT aop n m) ': s)
  -> IT ('T_c n ': 'T_c m ': s)
  -> VarAnn
  -> Either Text (SomeInstr op cp)
arithImpl op i@(_ ::& _ ::& rs) vn =
  pure $ op ::: (i, (sing, NStar, vn) ::& rs)

addImpl
  :: (Typeable rs, Typeable a, Typeable b)
  => Sing a -> Sing b
  -> IT ('T_c a ': 'T_c b ': rs) -> VarAnn -> Either Text (SomeInstr op cp)
addImpl ST_int ST_int = arithImpl @Add ADD
addImpl ST_int ST_nat = arithImpl @Add ADD
addImpl ST_nat ST_int = arithImpl @Add ADD
addImpl ST_nat ST_nat = arithImpl @Add ADD
addImpl ST_int ST_timestamp = arithImpl @Add ADD
addImpl ST_timestamp ST_int = arithImpl @Add ADD
addImpl ST_mutez ST_mutez = arithImpl @Add ADD
addImpl _ _ = \i vn -> typeCheckIErr (M.ADD vn) (SomeIT i) ""

subImpl
  :: (Typeable rs, Typeable a, Typeable b)
  => Sing a -> Sing b
  -> IT ('T_c a ': 'T_c b ': rs) -> VarAnn -> Either Text (SomeInstr op cp)
subImpl ST_int ST_int = arithImpl @Sub SUB
subImpl ST_int ST_nat = arithImpl @Sub SUB
subImpl ST_nat ST_int = arithImpl @Sub SUB
subImpl ST_nat ST_nat = arithImpl @Sub SUB
subImpl ST_timestamp ST_timestamp = arithImpl @Sub SUB
subImpl ST_timestamp ST_int = arithImpl @Sub SUB
subImpl ST_mutez ST_mutez = arithImpl @Sub SUB
subImpl _ _ = \i vn -> typeCheckIErr (M.SUB vn) (SomeIT i) ""

mulImpl
  :: (Typeable rs, Typeable a, Typeable b)
  => Sing a -> Sing b
  -> IT ('T_c a ': 'T_c b ': rs) -> VarAnn -> Either Text (SomeInstr op cp)
mulImpl ST_int ST_int = arithImpl @Mul MUL
mulImpl ST_int ST_nat = arithImpl @Mul MUL
mulImpl ST_nat ST_int = arithImpl @Mul MUL
mulImpl ST_nat ST_nat = arithImpl @Mul MUL
mulImpl ST_nat ST_mutez = arithImpl @Mul MUL
mulImpl ST_mutez ST_nat = arithImpl @Mul MUL
mulImpl _ _ = \i vn -> typeCheckIErr (M.MUL vn) (SomeIT i) ""

compareImpl
  :: (Typeable rs, Typeable a, Typeable b)
  => Sing a -> Sing b
  -> IT ('T_c a ': 'T_c b ': rs) -> VarAnn -> Either Text (SomeInstr op cp)
compareImpl ST_nat ST_nat = arithImpl @Compare COMPARE
compareImpl ST_int ST_int = arithImpl @Compare COMPARE
compareImpl ST_string ST_string = arithImpl @Compare COMPARE
compareImpl ST_bytes ST_bytes = arithImpl @Compare COMPARE
compareImpl ST_timestamp ST_timestamp = arithImpl @Compare COMPARE
compareImpl ST_key_hash ST_key_hash = arithImpl @Compare COMPARE
compareImpl ST_mutez ST_mutez = arithImpl @Compare COMPARE
compareImpl _ _ = \i vn -> typeCheckIErr (M.COMPARE vn) (SomeIT i) ""

-- | Helper function to construct instructions for binary arithmetic
-- operations.
unaryArithImpl
  :: ( Typeable (UnaryArithResT aop n)
     , SingI (UnaryArithResT aop n)
     , Typeable ('T_c (UnaryArithResT aop n) ': s)
     )
  => Instr op cp ('T_c n ': s) ('T_c (UnaryArithResT aop n) ': s)
  -> IT ('T_c n ': s)
  -> VarAnn
  -> Either Text (SomeInstr op cp)
unaryArithImpl op i@(_ ::& rs) vn = pure $ op ::: (i, (sing, NStar, vn) ::& rs)
