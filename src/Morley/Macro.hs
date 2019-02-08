{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Morley.Macro
  ( expand
  , expandFlat
  , expandContractMacros
  , expandPapair
  , expandUnpapair
  , expandCadr
  , expandSetCadr
  , expandMapCadr
  , flatten
  , mapLeaves
  ) where

import Generics.SYB (everywhere, mkT)
import Morley.Types
  (CadrStruct(..), Contract(..), ExpandedInstr, ExpandedOp(..), FieldAnn, Instr,
  InstrAbstract(..), Macro(..), Op(..), PairStruct(..), ParsedOp(..), TypeAnn, VarAnn, noAnn)

expandFlat :: [ParsedOp] -> [Instr]
expandFlat = concatMap flatten . fmap expand

flatten :: ExpandedOp -> [Instr]
flatten (SEQ_EX s) = concatMap flatten s
flatten (PRIM_EX o) = [flattenInstr o]

-- Here used SYB approach instead pattern matching
-- flattenInstr (IF_NONE l r) = IF_NONE (concatMap flatten l) (concatMap flatten r)
-- flattenInstr (IF_LEFT l r) = IF_LEFT (concatMap flatten l) (concatMap flatten r)
-- ...
flattenInstr :: ExpandedInstr -> Instr
flattenInstr = fmap castPrim . everywhere (mkT flattenOps)
  where
    castPrim :: ExpandedOp -> Op
    castPrim (PRIM_EX x) = Op (castPrim <$> x)
    castPrim _           = error "unexpeted constructor"

    flattenOps :: [ExpandedOp] -> [ExpandedOp]
    flattenOps [] = []
    flattenOps (SEQ_EX s : xs) = s ++ flattenOps xs
    flattenOps (x@(PRIM_EX _) : xs) = x : flattenOps xs

expand :: ParsedOp -> ExpandedOp
expand (MAC m)  = SEQ_EX $ expandMacro m
expand (PRIM i) = PRIM_EX $ fmap expand i
expand (SEQ s)  = SEQ_EX $ expand <$> s

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
  DUUP n v           -> xol $ DIP [MAC $ DUUP (n - 1) v]
  where
    xol = one . xo
    xo = PRIM_EX . fmap expand
    xp = fmap expand

-- the correctness of type-annotation expansion is currently untested, as these
-- expansions are not explicitly documented in the Michelson Specification
expandPapair :: PairStruct -> TypeAnn -> VarAnn -> [ParsedOp]
expandPapair ps t v = case ps of
  P (F a) (F b) -> [PRIM $ PAIR t v (snd a) (snd b)]
  P (F a) r     -> PRIM <$> [DIP [MAC $ PAPAIR r noAnn noAnn], PAIR t v (snd a) noAnn]
  P l     (F b) -> [PRIM $ PAIR noAnn noAnn noAnn (snd b), MAC $ PAPAIR l t v]
  P l     r     -> [MAC $ PAPAIR r noAnn noAnn, MAC $ PAPAIR l noAnn noAnn, PRIM $ PAIR t v noAnn noAnn]

expandUnpapair :: PairStruct -> [ParsedOp]
expandUnpapair = \case
  P (F (v,f)) (F (w,g)) -> PRIM <$> [DUP noAnn, CAR v f, DIP [PRIM $ CDR w g]]
  P (F a) r             -> [MAC $ UNPAIR (F a), PRIM $ DIP [MAC $ UNPAIR r]]
  P l     (F b)         -> [MAC $ UNPAIR (F b), MAC $ UNPAIR l]
  P l      r            -> MAC <$> [UNPAIR (P fn fn), UNPAIR l, UNPAIR r]
  where
    fn = F (noAnn, noAnn)

expandCadr :: [CadrStruct] -> VarAnn -> FieldAnn -> [ParsedOp]
expandCadr cs v f = case cs of
  A:[] -> [PRIM $ CAR v f]
  D:[] -> [PRIM $ CDR v f]
  A:css -> [PRIM $ CAR noAnn noAnn, MAC $ CADR css v f]
  D:css -> [PRIM $ CDR noAnn noAnn, MAC $ CADR css v f]

expandSetCadr :: [CadrStruct] -> VarAnn -> FieldAnn -> [ParsedOp]
expandSetCadr cs v f = PRIM <$> case cs of
  A:[] -> [CDR v f, SWAP, pairN]
  D:[] -> [CAR v f, pairN]
  A:css -> [DUP noAnn, DIP [PRIM carN, MAC $ SET_CADR css v f], cdrN, SWAP, pairN]
  D:css -> [DUP noAnn, DIP [PRIM cdrN, MAC $ SET_CADR css v f], cdrN, SWAP, pairN]
  where
    carN = CAR noAnn noAnn
    cdrN = CDR noAnn noAnn
    pairN = PAIR noAnn noAnn noAnn noAnn

expandMapCadr :: [CadrStruct] -> VarAnn -> FieldAnn -> [ParsedOp] -> [ParsedOp]
expandMapCadr cs v f ops = case cs of
  A:[] -> PRIM <$> [DUP noAnn, cdrN, DIP [PRIM $ CAR v f, SEQ ops], SWAP, pairN]
  D:[] ->
    concat [PRIM <$> [DUP noAnn, CDR v f], [SEQ ops], PRIM <$> [SWAP, carN, pairN]]
  A:css ->
    PRIM <$> [DUP noAnn, DIP [PRIM $ carN, MAC $ MAP_CADR css v f ops], cdrN, pairN]
  D:css ->
    PRIM <$> [DUP noAnn, DIP [PRIM $ cdrN, MAC $ MAP_CADR css v f ops], carN, pairN]
  where
    carN = CAR noAnn noAnn
    cdrN = CDR noAnn noAnn
    pairN = PAIR noAnn noAnn noAnn noAnn

expandContractMacros :: Contract ParsedOp -> Contract ExpandedOp
expandContractMacros Contract{..} = Contract para stor (map expand code)

mapLeaves :: [(VarAnn, FieldAnn)] -> PairStruct -> PairStruct
mapLeaves fs p = evalState (leavesST p) fs

leavesST :: PairStruct -> State [(VarAnn, FieldAnn)] PairStruct
leavesST = \case
  (P (F _) (F _)) -> do f1 <- state getLeaf
                        f2 <- state getLeaf
                        return $ P (F f1) (F f2)

  (P (F _) r)     -> do f  <- state getLeaf
                        r' <- leavesST r
                        return $ P (F f) r'

  (P l (F _))     -> do l' <- leavesST l
                        f  <- state getLeaf
                        return $ P l' (F f)

  (P l r)         -> do l' <- leavesST l
                        r' <- leavesST r
                        return $ P l' r'
  where
    getLeaf (a:as) = (a, as)
    getLeaf _      = ((noAnn, noAnn), [])
