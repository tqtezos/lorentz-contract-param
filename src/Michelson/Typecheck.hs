module Michelson.Typecheck
  ( Result
  , TypeError (..)
  , typecheckContract
  ) where

import Prelude hiding (EQ, GT, LT)

import Michelson.Types
  (Annotation(..), CT(..), Contract(..), Elt(..), FieldAnn, Instr, InstrAbstract(..), Op(..),
  T(..), pattern Taddress, pattern Tbool, pattern Tbytes, pattern Tint, pattern Tkey_hash,
  pattern Tmutez, pattern Tnat, pattern Tstring, pattern Ttimestamp, Type(..), TypeAnn, Value,
  VarAnn, ann, compToType, convAnn, disjoinVn, ifAnnUnified, isBool, isBytes, isComparable, isInt,
  isInteger, isKey, isKeyHash, isMutez, isNat, isSignature, isString, isTimestamp, noAnn, taddress,
  tbool, tbytes, tint, tkeyHash, tmutez, tnat, tstring, ttimestamp, typeToComp, unifyAnn)
import qualified Michelson.Types as M

data SType
  = SType VarAnn T TypeAnn
  | FailedType
  deriving Show

initSType :: Contract Op -> [SType]
initSType c = [SType noAnn (T_pair noAnn noAnn (para c) (stor c)) noAnn]

toType :: SType -> Type
toType (SType _ t tn) = Type t tn
toType FailedType     = error "unexpected failed type"

defStType :: Type -> SType
defStType (Type t tn) = SType noAnn t tn

toStType :: VarAnn -> Type -> SType
toStType vn (Type t tn) = SType vn t tn

type Result = Either TypeError [SType]
data TypeError =
    InvaliPush (Value Op) Type
  | NotUnifiedStacks [SType] [SType]
  | TypeError Instr [SType]
  | InvalidFinalStack [SType]
  deriving (Show)

instance Exception TypeError

typecheckContract :: Contract Op -> Either TypeError ()
typecheckContract c = typecheck (para c) (initSType c) (code c) >>= \case
  [FailedType]  -> pure ()
  [SType _ (T_pair _ _ (Type (T_list (Type T_operation _)) _) s) _]
    | s `isEqTypes` stor c -> pure ()
  stk  -> Left $ InvalidFinalStack stk

typecheck :: Type -> [SType] -> [Op] -> Result
typecheck _ stk [] = Right stk
typecheck param stk (Op i : ops) = do
  newStk <- applyInstr param i stk
  typecheck param newStk ops

applyInstr :: Type -> Instr -> [SType] -> Result
applyInstr param i stk = case (i, stk) of
  (_, [FailedType])      -> pure [FailedType]
  (DROP, _t:ts)          -> pure ts
  (DUP _, t:ts)          -> pure $ t:t:ts
  (SWAP, t:t':ts)        -> pure $ t':t:ts
  (PUSH vn tp val, ts)
    | valueOfType param val tp -> pure $ toStType vn tp :ts
    | otherwise                -> Left $ InvaliPush val tp
  (SOME tn vn fn, st:ts) -> pure $ SType vn (T_option fn (toType st)) tn : ts
  (NONE tn vn fn t, ts)  -> pure $ SType vn (T_option fn t) tn: ts
  (UNIT tn vn, ts)       -> pure $ SType vn T_unit tn :ts
  (IF_NONE bt bf, SType vn (T_option _ (Type a tn)) _ :ts) -> do
    st1 <- typecheck param ts bt
    st2 <- typecheck param (SType (vn <> ann "some") a tn : ts) bf
    unifyStacks st1 st2
  (PAIR tn vn fnl fnr, st1@(SType vnl _ _) : st2@(SType vnr _ _) : ts) -> do
    let (vresl, fresl) = fnl `applyFn` vnl
    let (vresr, fresr) = fnr `applyFn` vnr
    let vres = if vresl == vresr then vresl else noAnn
    pure $ SType (vres <> vn) (T_pair fresl fresr (toType st1) (toType st2)) tn:ts
  (CAR vn fn, SType vnp (T_pair fnl _ a _) _ : ts)
    | fn `ifAnnUnified` fnl -> pure $ case vn of
        Annotation "%"  -> toStType (convAnn fnl) a : ts
        Annotation "%%" -> toStType (vnp <> convAnn fnl) a : ts
        _               -> toStType vn a : ts
  (CDR vn fn, SType vnp (T_pair _ fnr _ b) _ : ts)
    | fn `ifAnnUnified` fnr -> pure $ case vn of
        Annotation "%"  -> toStType (convAnn fnr) b : ts
        Annotation "%%" -> toStType (vnp <> convAnn fnr) b : ts
        _               -> toStType vn b : ts
  (LEFT tn vn fnl fnr tr, s : ts) ->
    pure $ SType vn (T_or fnl fnr (toType s) tr) tn : ts
  (RIGHT tn vn fnl fnr tl, s : ts) ->
    pure $ SType vn (T_or fnl fnr tl (toType s)) tn : ts
  (IF_LEFT bt bf, SType _ (T_or _ _ ta tb) _ : ts) -> do
    st1 <- typecheck param (defStType ta : ts) bt
    st2 <- typecheck param (defStType tb : ts) bf
    unifyStacks st1 st2
  (IF_RIGHT bt bf, SType _ (T_or _ _ ta tb) _ : ts) -> do
    st1 <- typecheck param (defStType tb : ts) bt
    st2 <- typecheck param (defStType ta : ts) bf
    unifyStacks st1 st2
  (NIL tn vn t, st)      -> pure $ SType vn (T_list t) tn : st
  (CONS vn, h : SType _ (T_list tl) tn : ts)
    | toType h `isEqTypes` tl -> pure $ SType vn (T_list tl) tn : ts
  (IF_CONS bt bf, lt@(SType _ (T_list t) _) : ts) -> do
    st1 <- typecheck param (defStType t : lt : ts) bt
    st2 <- typecheck param ts bf
    unifyStacks st1 st2
  (SIZE vn, SType _ Tstring _ : ts) ->
    pushStk ts vn tnat
  (SIZE vn, SType _ Tbytes _ : ts) ->
    pushStk ts vn tnat
  (SIZE vn, SType _ (T_list _) _ : ts) ->
    pushStk ts vn tnat
  (SIZE vn, SType _ (T_set _) _ : ts) ->
    pushStk ts vn tnat
  (SIZE vn, SType _ (T_map _ _) _ : ts) ->
    pushStk ts vn tnat
  (EMPTY_SET tn vn cm, ts) -> pure $ SType vn (T_set cm) tn : ts
  (EMPTY_MAP tn vn cm t, ts) -> pure $ SType vn (T_map cm t) tn : ts
  (MAP vn body, SType _ (T_list t) _ : ts) -> do
    typecheck param (defStType t : ts) body >>= \case
      b : res -> (SType vn (T_list (toType b)) noAnn :) <$> unifyStacks res ts
      _       -> Left $ TypeError i stk
  (MAP vn body, SType _ (T_map cm t) _ : ts) -> do
    let pair = Type (T_pair noAnn noAnn (compToType cm) t) noAnn
    typecheck param (defStType pair : ts) body >>= \case
      b : res -> (SType vn (T_map cm (toType b)) noAnn :) <$> unifyStacks res ts
      _ -> Left $ TypeError i stk
  (ITER body, SType _ (T_list t) _ : ts) ->
    typecheck param (defStType t : ts) body >>= unifyStacks ts
  (ITER body, SType _ (T_set cm) _ : ts) ->
    typecheck param (defStType (compToType cm) : ts) body >>= unifyStacks ts
  (ITER body, SType _ (T_map cm t) _ : ts) -> do
    let pair = T_pair noAnn noAnn (compToType cm) t
    typecheck param (SType noAnn  pair noAnn : ts) body >>= unifyStacks ts
  (MEM vn, k : SType _ (T_set cm) _ : ts)
    | toType k `isEqTypes` compToType cm ->
        pushStk ts vn tbool
  (MEM vn, k : SType _ (T_map cm _) _ : ts)
    | toType k `isEqTypes` compToType cm ->
        pushStk ts vn tbool
  (MEM vn, k : SType _ (T_big_map cm _) _ : ts)
    | toType k `isEqTypes` compToType cm ->
        pushStk ts vn tbool
  (GET vn, k : SType _ (T_map cm tv) _ : ts)
    | toType k `isEqTypes` compToType cm ->
        pushStk ts vn (T_option noAnn tv)
  (GET vn, k : SType _ (T_big_map cm tv) _ : ts)
    | toType k `isEqTypes` compToType cm ->
        pushStk ts vn (T_option noAnn tv)
  (UPDATE, k : SType _ Tbool _ : se@(SType _ (T_set cm) _) : ts)
    | toType k `isEqTypes` compToType cm ->
        pure $ se : ts
  (UPDATE, k : SType _ (T_option _ tv) _ : ma@(SType _ (T_map cm t) _) : ts)
    | toType k `isEqTypes` compToType cm && tv `isEqTypes` t ->
        pure $ ma : ts
  (UPDATE, k : SType _ (T_option _ tv) _ : ma@(SType _ (T_big_map cm t) _) : ts)
    | toType k `isEqTypes` compToType cm && tv `isEqTypes` t ->
        pure $ ma : ts
  (IF bt bf, SType _ Tbool _ : ts) -> do
    st1 <- typecheck param ts bt
    st2 <- typecheck param ts bf
    unifyStacks st1 st2
  (LOOP body, s1@(SType _ Tbool _) : ts) ->
    typecheck param ts body >>= \case
      s2 : ts' | toType s1 `isEqTypes` toType s2 && isRight (unifyStacks ts ts')
                -> pure ts
      _         -> Left $ TypeError i stk
  (LOOP_LEFT body, s1@(SType _ (T_or _ _ ta tb) _) : ts) ->
    typecheck param (defStType ta : ts) body >>= \case
      s2 : ts'
        | toType s1 `isEqTypes` toType s2 && isRight (unifyStacks ts ts') ->
          pure $ defStType tb : ts
      _         -> Left $ TypeError i stk
  (LAMBDA vn a b body, ts) -> typecheck param [defStType a] body >>= \case
    [b'] | toType b' `isEqTypes` b -> pushStk ts vn (T_lambda a b)
    _                              -> Left $ TypeError i stk
  (EXEC vn, st : SType _ (T_lambda a b) _ : ts)
    | toType st `isEqTypes` a -> pure $ toStType vn b : ts
  (DIP body, x:ts) -> (x:) <$> typecheck param ts body
  (FAILWITH, _:_)   -> pure [FailedType]
  (CAST vn t, a : ts)
    | toType a `isEqTypes` t ->
        pure $ toStType (vn `orv` "cast") t : ts
  (RENAME vn, s : ts) -> pure $ toStType vn (toType s) : ts
  (PACK vn, _ : ts)   -> pure $ SType vn tbytes noAnn : ts
  (UNPACK vn t, s : ts)
    | isBytes (toType s) ->
        pushStk ts vn (T_option noAnn t)
  (CONCAT vn, SType _ (T_list t) _ : ts)
    | isString t -> pushStk ts (vn `orv` "concat") tstring
    | isBytes t  -> pushStk ts (vn `orv` "concat") tbytes
  (CONCAT vn, _) ->
    either (const $ binOp isString (vn `orv` "concat")) Right $ binOp isBytes (vn `orv` "concat")
  (SLICE vn, off : len : s : ts)
    | isNat (toType off) && isNat (toType len) &&
      (isString (toType s) || isBytes (toType s)) &&
      toType off `isEqTypes` toType len ->
        pushStk ts (vn `orv` "slice") (T_option noAnn (toType s))
  (ISNAT, s : ts)
    | isInt (toType s) ->
      pushStk ts (ann "isnat") (T_option noAnn (Type tnat noAnn))
  (ADD vn, (toType -> a@(Type t _)) : (toType -> b) : ts)
    | isInt a && isNat b ||
      isInt b && isNat a ||
      isInt a && isInt b -> pushStk ts (vn `orv` "add") tint
    | isNat a && isNat b ||
      isMutez a && isMutez b     -> pushStk ts (vn `orv` "add") t
    | isTimestamp a && isInt b ||
      isInt a && isTimestamp b  -> pushStk ts (vn `orv` "add") ttimestamp
  (SUB vn, (toType -> a) : (toType -> b) : ts)
    | isInt a && isNat b ||
      isInt b && isNat a ||
      isNat a && isNat b     ||
      isTimestamp a && isTimestamp b -> pushStk ts (vn `orv` "sub") tint
    | isTimestamp a && isInt b       -> pushStk ts (vn `orv` "sub") ttimestamp
    | isMutez a && isMutez b         -> pushStk ts (vn `orv` "sub") tmutez
  (MUL vn, (toType -> a) : (toType -> b) : ts)
    | isInt a && isNat b ||
      isInt b && isNat a ||
      isInt a && isInt b -> pushStk ts (vn `orv` "mul") tint
    | isNat a && isNat b -> pushStk ts (vn `orv` "mul") tnat
    | isMutez a && isNat b ||
      isNat a && isMutez b -> pushStk ts (vn `orv` "mul") tmutez
  (EDIV vn, (toType -> a) : (toType -> b) : ts)
    | isNat a && isNat b         ->
        pushStk ts vn (T_option noAnn (Type (T_pair noAnn noAnn a a) noAnn))
    | isInteger a && isInteger b -> do
        let intType = Type tint noAnn
        let natType = Type tnat noAnn
        pushStk ts vn (T_option noAnn (Type (T_pair noAnn noAnn intType natType) noAnn))
  (ABS vn, (toType -> a) : ts)
    | isInt a -> pushStk ts (vn `orv` "abs") tnat
  (NEG, (toType -> a) : ts)
    | isInteger a -> pushStk ts (ann "neg") tint
  (INT vn, (toType -> a) : ts)
    | isNat a     -> pushStk ts vn (T_comparable T_int)
  (LSL vn, _) -> binOp isNat (vn `orv` "lsl")
  (LSR vn, _) -> binOp isNat (vn `orv` "lsr")
  (OR vn, _)  -> binOp isBool (vn `orv` "or")
  (AND vn, _) -> binOp isBool (vn `orv` "and")
  (XOR vn, _) -> binOp isBool (vn `orv` "xor")
  (NOT vn, (toType -> a) : ts)
    | isBool a -> pushStk ts (vn `orv` "not") tbool
  (COMPARE vn, (toType -> a) : (toType -> b) : ts)
    | isComparable a && isComparable b && a `isEqTypes` b ->
        pushStk ts vn tint
  (EQ vn, _)  -> cmpOp (vn `orv` "eq")
  (NEQ vn, _) -> cmpOp (vn `orv` "neq")
  (LT vn, _)  -> cmpOp (vn `orv` "lt")
  (GT vn, _)  -> cmpOp (vn `orv` "gt")
  (LE vn, _)  -> cmpOp (vn `orv` "le")
  (GE vn, _)  -> cmpOp (vn `orv` "ge")
  (SELF vn, ts) -> pushStk ts (vn `orv` "self") (T_contract param)
  (CONTRACT t, SType _ Taddress _ : ts) ->
    pushStk ts (ann "contract") $ T_option noAnn (Type (T_contract t) noAnn)
  (TRANSFER_TOKENS vn, (toType -> p') : (toType -> mt) : (SType _ (T_contract p) _) : ts)
    | isMutez mt && p' `isEqTypes` p ->
        pushStk ts (vn `orv` "transfer_tokens") T_operation
  (SET_DELEGATE, SType _ (T_option _ (Type Tkey_hash _)) _ : ts) ->
      pushStk ts (ann "set_delegate") T_operation
  (CREATE_ACCOUNT vn1 vn2, kh :
                            SType _ (T_option _ (Type Tkey_hash _)) _ :
                            bl : mt : ts)
    | isKeyHash (toType kh) && isBool (toType bl) && isMutez (toType mt) ->
        pure $ SType vn1 T_operation noAnn :
               SType vn2 taddress noAnn :
               ts
  (CREATE_CONTRACT vn1 vn2,  kh : SType _ (T_option _ kh2) _ : bl1 : bl2 : mt :
      SType _ (T_lambda (Type (T_pair _ _ _ g1) _)
                        (Type (T_pair _ _ (Type (T_list (Type T_operation _)) _) g2) _)) _ : g3 : ts)
    | isKeyHash (toType kh) && isKeyHash kh2 && isBool (toType bl1) &&
      isBool (toType bl2) && isMutez (toType mt) && toType g3 `isEqTypes` g1 && g1 `isEqTypes` g2 ->
        pure $ SType vn1 T_operation noAnn :
               SType vn2 taddress noAnn :
               ts
  (CREATE_CONTRACT2 vn1 vn2 c, kh : SType _ (T_option _ kh2) _ : bl1 : bl2 : mt : g : ts)
    | isKeyHash (toType kh) && isKeyHash kh2 && isBool (toType bl1) &&
      isBool (toType bl2) && isMutez (toType mt) && toType g `isEqTypes` stor c ->
        pure $ SType vn1 T_operation noAnn :
               SType vn2 taddress noAnn :
               ts
  (IMPLICIT_ACCOUNT vn, (toType -> a) : ts)
    | isKeyHash a ->
      pushStk ts (vn `orv` "implicit_account") (T_contract (Type T_unit noAnn))
  (NOW vn, ts) ->
    pushStk ts (vn `orv` "now") ttimestamp
  (AMOUNT vn, ts) ->
    pushStk ts (vn `orv` "amount") tmutez
  (BALANCE vn, ts) ->
    pushStk ts (vn `orv` "balance") tmutez
  (CHECK_SIGNATURE vn, (toType -> k) : (toType -> sig) : (toType -> bts) : ts)
    | isKey k && isSignature sig && isBytes bts ->
        pushStk ts (vn `orv` "check_sig") tbool
  (SHA256 vn, (toType -> a) : ts)
    | isBytes a ->
        pushStk ts (vn `orv` "sha256") tbytes
  (SHA512 vn, (toType -> a) : ts)
    | isBytes a ->
        pushStk ts (vn `orv` "sha512") tbytes
  (BLAKE2B vn, (toType -> a) : ts)
    | isBytes a ->
        pushStk ts (vn `orv` "blake2b") tbytes
  (HASH_KEY vn, (toType -> a) : ts)
    | isKey a -> pushStk ts (vn `orv` "b58check") tkeyHash
  (STEPS_TO_QUOTA vn, ts) -> pushStk ts (vn `orv` "steps") tnat
  (SOURCE  vn, ts) -> pushStk ts (vn `orv` "source") taddress
  (SENDER vn, ts)  -> pushStk ts (vn `orv` "sender") taddress
  (ADDRESS vn, SType _ (T_contract _) _ : ts) ->
    pushStk ts (vn `orv` "address") taddress
  _ -> Left $ TypeError i stk
  where
  applyFn :: FieldAnn -> VarAnn -> (VarAnn, FieldAnn)
  applyFn fn vn
    | fn == ann "@" = convAnn <$> disjoinVn vn
    | otherwise     = (vn, fn)

  pushStk :: [SType] -> VarAnn -> T -> Result
  pushStk ts vn t = pure $ SType vn t noAnn : ts

  binOp :: (Type -> Bool) -> VarAnn -> Result
  binOp isType varAnn = case stk of
    (toType -> a) : (toType -> b) : ts
      | isType a && isType b -> case unifyTypes a b of
        Nothing -> Left $ TypeError i stk
        Just t  -> pure $ toStType varAnn t : ts
    _           -> Left $ TypeError i stk

  cmpOp :: VarAnn -> Result
  cmpOp vn = case stk of
    (toType -> a) : ts
      | isInteger a  -> pushStk ts vn tbool
    _                -> Left $ TypeError i stk

orv :: VarAnn -> Text -> VarAnn
orv vn t
  | vn == noAnn = ann t
  | otherwise   = vn

unifyTypes :: Type -> Type -> Maybe Type
unifyTypes (Type (T_option f1 t1) tn1) (Type (T_option f2 t2) tn2) =
  Type <$> (T_option <$> unifyAnn f1 f2 <*> unifyTypes t1 t2)
       <*> unifyAnn tn1 tn2
unifyTypes (Type (T_list t1) tn1) (Type (T_list t2) tn2) =
  Type <$> (T_list <$> unifyTypes t1 t2) <*> unifyAnn tn1 tn2
unifyTypes (Type (T_set c1) tn1) (Type (T_set c2) tn2) =
  Type <$> (T_set <$> (typeToComp =<< unifyTypes (compToType c1) (compToType c2)))
        <*> unifyAnn tn1 tn2
unifyTypes (Type (T_contract t1) tn1) (Type (T_contract t2) tn2) =
  Type <$> (T_contract <$> unifyTypes t1 t2) <*> unifyAnn tn1 tn2
unifyTypes (Type (T_pair fl1 fr1 tl1 tr1) tn1) (Type (T_pair fl2 fr2 tl2 tr2) tn2) =
  Type <$> (T_pair <$>
            unifyAnn fl1 fl2 <*>
            unifyAnn fr1 fr2 <*>
            unifyTypes tl1 tl2 <*>
            unifyTypes tr1 tr2)
        <*> unifyAnn tn1 tn2
unifyTypes (Type (T_or fl1 fr1 tl1 tr1) tn1) (Type (T_or fl2 fr2 tl2 tr2) tn2) =
  Type <$> (T_or <$>
            unifyAnn fl1 fl2 <*>
            unifyAnn fr1 fr2 <*>
            unifyTypes tl1 tl2 <*>
            unifyTypes tr1 tr2)
        <*> unifyAnn tn1 tn2
unifyTypes (Type (T_lambda ta1 tb1) tn1) (Type (T_lambda ta2 tb2) tn2) =
  Type <$> (T_lambda <$> unifyTypes ta1 ta2 <*> unifyTypes tb1 tb2)
        <*> unifyAnn tn1 tn2
unifyTypes (Type (T_map c1 t1) tn1) (Type (T_map c2 t2) tn2) =
  Type <$> (T_map <$> (typeToComp =<< unifyTypes (compToType c1) (compToType c2))
                  <*> unifyTypes t1 t2)
       <*> unifyAnn tn1 tn2
unifyTypes (Type (T_big_map c1 t1) tn1) (Type (T_big_map c2 t2) tn2) =
  Type <$> (T_big_map <$> (typeToComp =<< unifyTypes (compToType c1) (compToType c2))
                      <*> unifyTypes t1 t2)
       <*> unifyAnn tn1 tn2
unifyTypes (Type t1 tn1) (Type t2 tn2)
  | t1 == t2 = Type t1 <$> unifyAnn tn1 tn2
  | otherwise = Nothing

isEqTypes :: Type -> Type -> Bool
isEqTypes t1 t2 = isJust $ unifyTypes t1 t2

unifySTypes :: SType -> SType -> Maybe SType
unifySTypes FailedType _ = Just FailedType
unifySTypes _ FailedType = Just FailedType
unifySTypes s1@(SType f1 _ _) s2@(SType f2 _ _) =
  toStType (if f1 == f2 then f1 else noAnn) <$> unifyTypes (toType s1) (toType s2)

unifyStacks :: [SType] -> [SType] -> Either TypeError [SType]
unifyStacks [FailedType] b = pure b
unifyStacks a [FailedType] = pure a
unifyStacks s1 s2
  | length s1 == length s2 =
      maybe (Left $ NotUnifiedStacks s1 s2) Right $
      traverse (uncurry unifySTypes) (zip s1 s2)
  | otherwise = Left $ NotUnifiedStacks s1 s2

valueOfType :: Type -> Value Op -> Type -> Bool
valueOfType _ (M.ValueInt _) (Type Tint _)   = True
valueOfType _ (M.ValueInt _) (Type Tnat _)   = True
valueOfType _ (M.ValueInt _) (Type Tmutez _) = True
valueOfType _ (M.ValueInt _) (Type Ttimestamp _) = True
valueOfType _ (M.ValueString _) (Type Tstring _) = True
valueOfType _ (M.ValueString _) (Type Tkey_hash _) = True
valueOfType _ (M.ValueBytes _) (Type Tbytes _) = True
valueOfType _ M.ValueUnit (Type T_unit _) = True
valueOfType _ M.ValueTrue (Type Tbool _) = True
valueOfType _ M.ValueFalse (Type Tbool _) = True
valueOfType param (M.ValuePair l r) (Type (T_pair _ _ a b) _) = valueOfType param l a && valueOfType param r b
valueOfType param (M.ValueLeft val) (Type (T_or _ _ a _) _) = valueOfType param val a
valueOfType param (M.ValueRight val) (Type (T_or _ _ _ b) _) = valueOfType param val b
valueOfType param (M.ValueSome val) (Type (T_option _ a) _) = valueOfType param val a
valueOfType _ M.ValueNone (Type (T_option _ _) _) = True
valueOfType param (M.ValueSeq elts) (Type (T_list a) _) = all (flip (valueOfType param) a) elts
valueOfType param (M.ValueSeq elts) (Type (T_set a) _) = all (flip (valueOfType param) (compToType a)) elts
valueOfType param (M.ValueMap elts) (Type (T_map c v) _) =
  all ((\(Elt a b) -> valueOfType param a (compToType c) && valueOfType param b v)) elts
valueOfType param (M.ValueLambda ops) (Type (T_lambda a b) _) =
  case typecheck param [toStType (ann "lambda_param") a] ops of
    Right [b'] -> toType b' `isEqTypes` b
    _          -> False
valueOfType _ _ _ = False
