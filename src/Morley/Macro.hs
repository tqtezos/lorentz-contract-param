module Morley.Macro
  (
    -- * For utilities
    expandFlattenContract
  , expandValue

    -- * For parsing
  , mapLeaves

    -- * Internals exported for tests
  , expand
  , expandFlat
  , expandPapair
  , expandUnpapair
  , expandCadr
  , expandSetCadr
  , expandMapCadr
  , flatten

  ) where

import Generics.SYB (everywhere, mkT)

import Morley.Types
  (CadrStruct(..), Contract(..), Elt(..), ExpandedInstr, ExpandedOp(..), FieldAnn, Instr,
  InstrAbstract(..), LetMacro(..), Macro(..), Op(..), PairStruct(..), ParsedOp(..), TypeAnn,
  UExtInstrAbstract(..), Value(..), VarAnn, ann, noAnn)

expandFlat :: [ParsedOp] -> [Op]
expandFlat = fmap Op . concatMap flatten . fmap expand

-- | Expand and flatten and instructions in parsed contract.
expandFlattenContract :: Contract ParsedOp -> Contract Op
expandFlattenContract Contract {..} =
  Contract para stor (expandFlat $ code)

-- Probably, some SYB can be used here
expandValue :: Value ParsedOp -> Value Op
expandValue = \case
  ValuePair l r -> ValuePair (expandValue l) (expandValue r)
  ValueLeft x -> ValueLeft (expandValue x)
  ValueRight x -> ValueRight (expandValue x)
  ValueSome x -> ValueSome (expandValue x)
  ValueSeq valueList -> ValueSeq (map expandValue valueList)
  ValueMap eltList -> ValueMap (map expandElt eltList)
  ValueLambda opList -> ValueLambda (expandFlat $ opList)
  x -> fmap (unsafeCastPrim . expand) x

expandElt :: Elt ParsedOp -> Elt Op
expandElt (Elt l r) = Elt (expandValue l) (expandValue r)

flatten :: ExpandedOp -> [Instr]
flatten (SEQ_EX s) = concatMap flatten s
flatten (PRIM_EX o) = [flattenInstr o]

unsafeCastPrim :: ExpandedOp -> Op
unsafeCastPrim (PRIM_EX x) = Op (fmap unsafeCastPrim x)
unsafeCastPrim _           = error "unexpected constructor"

-- Here used SYB approach instead pattern matching
-- flattenInstr (IF_NONE l r) = IF_NONE (concatMap flatten l) (concatMap flatten r)
-- flattenInstr (IF_LEFT l r) = IF_LEFT (concatMap flatten l) (concatMap flatten r)
-- ...
flattenInstr :: ExpandedInstr -> Instr
flattenInstr = fmap unsafeCastPrim . everywhere (mkT flattenOps)
  where
    flattenOps :: [ExpandedOp] -> [ExpandedOp]
    flattenOps [] = []
    flattenOps (SEQ_EX s : xs) = s ++ flattenOps xs
    flattenOps (x@(PRIM_EX _) : xs) = x : flattenOps xs

expand :: ParsedOp -> ExpandedOp
expand (MAC m)  = SEQ_EX $ expandMacro m
expand (PRIM i) = PRIM_EX $ expand <$> i
expand (SEQ s)  = SEQ_EX $ expand <$> s
expand (LMAC l)  = SEQ_EX $ expandLetMac l
  where
    expandLetMac :: LetMacro -> [ExpandedOp]
    expandLetMac LetMacro {..} =
      [ PRIM_EX $ EXT (FN lmName lmSig)
      , SEQ_EX $ expand <$> lmExpr
      , PRIM_EX $ EXT FN_END
      ]

expandMacro :: Macro -> [ExpandedOp]
expandMacro = \case
  CMP i v            -> [PRIM_EX (COMPARE v), xo i]
  IFX i bt bf        -> [xo i, PRIM_EX (IF (xp bt) (xp bf))]
  IFCMP i v bt bf    -> PRIM_EX <$> [COMPARE v, expand <$> i, IF (xp bt) (xp bf)]
  IF_SOME bt bf      -> [PRIM_EX (IF_NONE (xp bf) (xp bt))]
  FAIL               -> PRIM_EX <$> [UNIT noAnn noAnn, FAILWITH]
  ASSERT             -> xol $ IF [] [MAC FAIL]
  ASSERTX i          -> [expand $ MAC $ IFX i [] [MAC FAIL]]
  ASSERT_CMP i       -> [expand $ MAC $ IFCMP i noAnn [] [MAC FAIL]]
  ASSERT_NONE        -> xol $ IF_NONE [] [MAC FAIL]
  ASSERT_SOME        -> xol $ IF_NONE [MAC FAIL] []
  ASSERT_LEFT        -> xol $ IF_LEFT [] [MAC FAIL]
  ASSERT_RIGHT       -> xol $ IF_LEFT [MAC FAIL] []
  PAPAIR ps t v      -> expand <$> expandPapair ps t v
  UNPAIR ps          -> expand <$> expandUnpapair ps
  CADR c v f         -> expand <$> expandCadr c v f
  SET_CADR c v f     -> expand <$> expandSetCadr c v f
  MAP_CADR c v f ops -> expand <$> expandMapCadr c v f ops
  DIIP 1 ops         -> [PRIM_EX $ DIP (xp ops)]
  DIIP n ops         -> xol $  DIP [MAC $ DIIP (n - 1) ops]
  DUUP 1 v           -> [PRIM_EX $ DUP v]
  DUUP n v           -> [xo (DIP [MAC $ DUUP (n - 1) v]), PRIM_EX SWAP]
  where
    xol = one . xo
    xo = PRIM_EX . fmap expand
    xp = fmap expand

-- the correctness of type-annotation expansion is currently untested, as these
-- expansions are not explicitly documented in the Michelson Specification
expandPapair :: PairStruct -> TypeAnn -> VarAnn -> [ParsedOp]
expandPapair ps t v = case ps of
  P (F a) (F b) -> [PRIM $ PAIR t v (snd a) (snd b)]
  P (F a) r     -> PRIM <$> [ DIP [MAC $ PAPAIR r noAnn noAnn]
                            , PAIR t v (snd a) noAnn]
  P l     (F b) -> [ MAC $ PAPAIR l noAnn noAnn
                   , PRIM $ PAIR t v noAnn (snd b)]
  P l     r     -> [ MAC $ PAPAIR l noAnn noAnn
                   , PRIM $ DIP [MAC $ PAPAIR r noAnn noAnn]
                   , PRIM $ PAIR t v noAnn noAnn]
  F _           -> [] -- Do nothing in this case.
  -- It's impossible from the structure of PairStruct and considered cases above,
  -- but if it accidentally happened let's just do nothing.

expandUnpapair :: PairStruct -> [ParsedOp]
expandUnpapair = \case
  P (F (v,f)) (F (w,g)) -> PRIM <$> [ DUP noAnn
                                    , CAR v f
                                    , DIP [PRIM $ CDR w g]]
  P (F (v, f)) r        -> PRIM <$> [ DUP noAnn
                                    , CAR v f
                                    , DIP [PRIM $ CDR noAnn noAnn,
                                           MAC $ UNPAIR r]]
  P l     (F (v, f))    -> [ PRIM (DUP noAnn)
                           , PRIM (DIP [PRIM $ CDR v f])
                           , PRIM $ CAR noAnn noAnn
                           , MAC $ UNPAIR l]
  P l      r            -> [ MAC unpairOne
                           , PRIM $ DIP [MAC $ UNPAIR r]
                           , MAC $ UNPAIR l]
  F _                   -> [] -- Do nothing in this case.
  -- It's impossible from the structure of PairStruct and considered cases above,
  -- but if it accidentally happened let's just do nothing.
  where
    unpairOne = UNPAIR (P fn fn)
    fn = F (noAnn, noAnn)

expandCadr :: [CadrStruct] -> VarAnn -> FieldAnn -> [ParsedOp]
expandCadr cs v f = case cs of
  []    -> []
  A:[]  -> [PRIM $ CAR v f]
  D:[]  -> [PRIM $ CDR v f]
  A:css -> [PRIM $ CAR noAnn noAnn, MAC $ CADR css v f]
  D:css -> [PRIM $ CDR noAnn noAnn, MAC $ CADR css v f]

expandSetCadr :: [CadrStruct] -> VarAnn -> FieldAnn -> [ParsedOp]
expandSetCadr cs v f = PRIM <$> case cs of
  []   -> []
  A:[] -> [DUP noAnn, CAR noAnn f, DROP,
           -- ↑ These operations just check that the left element of pair has %f
           CDR (ann "%%") noAnn, SWAP, PAIR noAnn v f (ann "@")]
  D:[] -> [DUP noAnn, CDR noAnn f, DROP,
           -- ↑ These operations just check that the right element of pair has %f
           CAR (ann "%%") noAnn, PAIR noAnn v (ann "@") f]
  A:css -> [DUP noAnn, DIP [PRIM carN, MAC $ SET_CADR css noAnn f], cdrN, SWAP, pairN]
  D:css -> [DUP noAnn, DIP [PRIM cdrN, MAC $ SET_CADR css noAnn f], carN, pairN]
  where
    carN = CAR noAnn noAnn
    cdrN = CDR noAnn noAnn
    pairN = PAIR noAnn v noAnn noAnn

expandMapCadr :: [CadrStruct] -> VarAnn -> FieldAnn -> [ParsedOp] -> [ParsedOp]
expandMapCadr cs v f ops = case cs of
  []    -> []
  A:[]  -> PRIM <$> [DUP noAnn, cdrN, DIP [PRIM $ CAR noAnn f, SEQ ops], SWAP, pairN]
  D:[]  -> concat [PRIM <$> [DUP noAnn, CDR noAnn f], [SEQ ops], PRIM <$> [SWAP, carN, pairN]]
  A:css -> PRIM <$> [DUP noAnn, DIP [PRIM $ carN, MAC $ MAP_CADR css noAnn f ops], cdrN, SWAP, pairN]
  D:css -> PRIM <$> [DUP noAnn, DIP [PRIM $ cdrN, MAC $ MAP_CADR css noAnn f ops], carN, pairN]
  where
    carN = CAR noAnn noAnn
    cdrN = CDR noAnn noAnn
    pairN = PAIR noAnn v noAnn noAnn

mapLeaves :: [(VarAnn, FieldAnn)] -> PairStruct -> PairStruct
mapLeaves fs p = evalState (leavesST p) fs

leavesST :: PairStruct -> State [(VarAnn, FieldAnn)] PairStruct
leavesST (P l r) = do
  l' <- leavesST l
  r' <- leavesST r
  return $ P l' r'
leavesST (F _) = do
  f <- state getLeaf
  return $ F f
  where
    getLeaf (a:as) = (a, as)
    getLeaf _      = ((noAnn, noAnn), [])
