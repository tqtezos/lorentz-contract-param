module Morley.Macro
  (
    -- * For utilities
    expandContract
  , expandValue

    -- * For parsing
  , mapLeaves

    -- * Internals exported for tests
  , expand
  , expandList
  , expandPapair
  , expandUnpapair
  , expandCadr
  , expandSetCadr
  , expandMapCadr
  ) where

import Michelson.Untyped (UntypedContract, UntypedValue)
import Morley.Types
  (CadrStruct(..), Contract(..), Elt(..), ExpandedOp(..), FieldAnn, InstrAbstract(..),
  LetMacro(..), Macro(..), PairStruct(..), ParsedOp(..), TypeAnn, UExtInstrAbstract(..), Value(..),
  VarAnn, ann, noAnn)

expandList :: [ParsedOp] -> [ExpandedOp]
expandList = fmap expand

-- | Expand all macros in parsed contract.
expandContract :: Contract ParsedOp -> UntypedContract
expandContract Contract {..} =
  Contract para stor (expandList $ code)

-- Probably, some SYB can be used here
expandValue :: Value ParsedOp -> UntypedValue
expandValue = \case
  ValuePair l r -> ValuePair (expandValue l) (expandValue r)
  ValueLeft x -> ValueLeft (expandValue x)
  ValueRight x -> ValueRight (expandValue x)
  ValueSome x -> ValueSome (expandValue x)
  ValueNil -> ValueNil
  ValueSeq valueList -> ValueSeq (map expandValue valueList)
  ValueMap eltList -> ValueMap (map expandElt eltList)
  ValueLambda opList ->
    maybe ValueNil ValueLambda $
    nonEmpty (expandList $ toList opList)
  x -> fmap expand x

expandElt :: Elt ParsedOp -> Elt ExpandedOp
expandElt (Elt l r) = Elt (expandValue l) (expandValue r)

expand :: ParsedOp -> ExpandedOp
-- We handle this case specially, because it's essentially just PAIR.
-- It's needed because we have a hack in parser: we parse PAIR as PAPAIR.
-- We need to do something better eventually.
expand (Mac (PAPAIR (P (F a) (F b)) t v)) =
  PrimEx $ PAIR t v (snd a) (snd b)
expand (Mac m)  = SeqEx $ expandMacro m
expand (Prim i) = PrimEx $ expand <$> i
expand (Seq s)  = SeqEx $ expand <$> s
expand (LMac l)  = SeqEx $ expandLetMac l
  where
    expandLetMac :: LetMacro -> [ExpandedOp]
    expandLetMac LetMacro {..} =
      [ PrimEx $ EXT (FN lmName lmSig)
      , SeqEx $ expand <$> lmExpr
      , PrimEx $ EXT FN_END
      ]

expandMacro :: Macro -> [ExpandedOp]
expandMacro = \case
  CMP i v            -> [PrimEx (COMPARE v), xo i]
  IFX i bt bf        -> [xo i, PrimEx (IF (xp bt) (xp bf))]
  IFCMP i v bt bf    -> PrimEx <$> [COMPARE v, expand <$> i, IF (xp bt) (xp bf)]
  IF_SOME bt bf      -> [PrimEx (IF_NONE (xp bf) (xp bt))]
  FAIL               -> PrimEx <$> [UNIT noAnn noAnn, FAILWITH]
  ASSERT             -> xol $ IF [] [Mac FAIL]
  ASSERTX i          -> [expand $ Mac $ IFX i [] [Mac FAIL]]
  ASSERT_CMP i       -> [expand $ Mac $ IFCMP i noAnn [] [Mac FAIL]]
  ASSERT_NONE        -> xol $ IF_NONE [] [Mac FAIL]
  ASSERT_SOME        -> xol $ IF_NONE [Mac FAIL] []
  ASSERT_LEFT        -> xol $ IF_LEFT [] [Mac FAIL]
  ASSERT_RIGHT       -> xol $ IF_LEFT [Mac FAIL] []
  PAPAIR ps t v      -> expand <$> expandPapair ps t v
  UNPAIR ps          -> expand <$> expandUnpapair ps
  CADR c v f         -> expand <$> expandCadr c v f
  SET_CADR c v f     -> expand <$> expandSetCadr c v f
  MAP_CADR c v f ops -> expand <$> expandMapCadr c v f ops
  DIIP 1 ops         -> [PrimEx $ DIP (xp ops)]
  DIIP n ops         -> xol $  DIP [Mac $ DIIP (n - 1) ops]
  DUUP 1 v           -> [PrimEx $ DUP v]
  DUUP n v           -> [xo (DIP [Mac $ DUUP (n - 1) v]), PrimEx SWAP]
  where
    xol = one . xo
    xo = PrimEx . fmap expand
    xp = fmap expand

-- the correctness of type-annotation expansion is currently untested, as these
-- expansions are not explicitly documented in the Michelson Specification
expandPapair :: PairStruct -> TypeAnn -> VarAnn -> [ParsedOp]
expandPapair ps t v = case ps of
  P (F a) (F b) -> [Prim $ PAIR t v (snd a) (snd b)]
  P (F a) r     -> Prim <$> [ DIP [Mac $ PAPAIR r noAnn noAnn]
                            , PAIR t v (snd a) noAnn]
  P l     (F b) -> [ Mac $ PAPAIR l noAnn noAnn
                   , Prim $ PAIR t v noAnn (snd b)]
  P l     r     -> [ Mac $ PAPAIR l noAnn noAnn
                   , Prim $ DIP [Mac $ PAPAIR r noAnn noAnn]
                   , Prim $ PAIR t v noAnn noAnn]
  F _           -> [] -- Do nothing in this case.
  -- It's impossible from the structure of PairStruct and considered cases above,
  -- but if it accidentally happened let's just do nothing.

expandUnpapair :: PairStruct -> [ParsedOp]
expandUnpapair = \case
  P (F (v,f)) (F (w,g)) -> Prim <$> [ DUP noAnn
                                    , CAR v f
                                    , DIP [Prim $ CDR w g]]
  P (F (v, f)) r        -> Prim <$> [ DUP noAnn
                                    , CAR v f
                                    , DIP [Prim $ CDR noAnn noAnn,
                                           Mac $ UNPAIR r]]
  P l     (F (v, f))    -> [ Prim (DUP noAnn)
                           , Prim (DIP [Prim $ CDR v f])
                           , Prim $ CAR noAnn noAnn
                           , Mac $ UNPAIR l]
  P l      r            -> [ Mac unpairOne
                           , Prim $ DIP [Mac $ UNPAIR r]
                           , Mac $ UNPAIR l]
  F _                   -> [] -- Do nothing in this case.
  -- It's impossible from the structure of PairStruct and considered cases above,
  -- but if it accidentally happened let's just do nothing.
  where
    unpairOne = UNPAIR (P fn fn)
    fn = F (noAnn, noAnn)

expandCadr :: [CadrStruct] -> VarAnn -> FieldAnn -> [ParsedOp]
expandCadr cs v f = case cs of
  []    -> []
  [A]  -> [Prim $ CAR v f]
  [D]  -> [Prim $ CDR v f]
  A:css -> [Prim $ CAR noAnn noAnn, Mac $ CADR css v f]
  D:css -> [Prim $ CDR noAnn noAnn, Mac $ CADR css v f]

expandSetCadr :: [CadrStruct] -> VarAnn -> FieldAnn -> [ParsedOp]
expandSetCadr cs v f = Prim <$> case cs of
  []   -> []
  [A] -> [DUP noAnn, CAR noAnn f, DROP,
           -- ↑ These operations just check that the left element of pair has %f
           CDR (ann "%%") noAnn, SWAP, PAIR noAnn v f (ann "@")]
  [D] -> [DUP noAnn, CDR noAnn f, DROP,
           -- ↑ These operations just check that the right element of pair has %f
           CAR (ann "%%") noAnn, PAIR noAnn v (ann "@") f]
  A:css -> [DUP noAnn, DIP [Prim carN, Mac $ SET_CADR css noAnn f], cdrN, SWAP, pairN]
  D:css -> [DUP noAnn, DIP [Prim cdrN, Mac $ SET_CADR css noAnn f], carN, pairN]
  where
    carN = CAR noAnn noAnn
    cdrN = CDR noAnn noAnn
    pairN = PAIR noAnn v noAnn noAnn

expandMapCadr :: [CadrStruct] -> VarAnn -> FieldAnn -> [ParsedOp] -> [ParsedOp]
expandMapCadr cs v f ops = case cs of
  []    -> []
  [A]  -> Prim <$> [DUP noAnn, cdrN, DIP [Prim $ CAR noAnn f, Seq ops], SWAP, pairN]
  [D]  -> concat [Prim <$> [DUP noAnn, CDR noAnn f], [Seq ops], Prim <$> [SWAP, carN, pairN]]
  A:css -> Prim <$> [DUP noAnn, DIP [Prim $ carN, Mac $ MAP_CADR css noAnn f ops], cdrN, SWAP, pairN]
  D:css -> Prim <$> [DUP noAnn, DIP [Prim $ cdrN, Mac $ MAP_CADR css noAnn f ops], carN, pairN]
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
