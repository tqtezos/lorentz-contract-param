{-# LANGUAGE LambdaCase #-}
--{-# LANGUAGE OverloadedStrings #-}

module Language.Michelson.Macro where

import qualified Data.Text as T

import Control.Monad.State.Lazy
import Language.Michelson.Types
  (CadrStruct(..), FieldNote, I(..), Macro(..), Op(..), PairStruct(..), VarNote(..))
import Language.Michelson.Types (Contract(..), code, para, stor)
import qualified Language.Michelson.Types as M

expandFlat :: [Op] -> [I]
expandFlat os = unPrim <$> (flatten $ expand <$> os)
  where
    unPrim (PRIM i) = i -- not actually partial, but still not great

flatten :: [Op] -> [Op]
flatten (SEQ s:ops) = s ++ (flatten ops)
flatten (o:os)      = o : (flatten os)

expand :: Op -> Op
expand (MAC m)  = SEQ $ expandMacro m
expand (PRIM i) = PRIM $ expandPrim i
expand (SEQ s)  = SEQ $ expand <$> s

expandMacro :: Macro -> [Op]
expandMacro = fmap expand . \case
  CMP i v            -> PRIM <$> [COMPARE v, i]
  IFX i bt bf        -> PRIM <$> [i, IF (xp bt) (xp bf)]
  IFCMP i v bt bf    -> PRIM <$> [COMPARE v, i, IF (xp bt) (xp bf)]
  IF_SOME bt bf      -> PRIM <$> [IF_NONE (xp bf) (xp bt)]
  FAIL               -> PRIM <$> [UNIT Nothing Nothing, FAILWITH]
  ASSERT             -> PRIM <$> [IF [] [MAC FAIL]]
  ASSERTX i          -> MAC  <$> [IFX i [] [MAC FAIL]]
  ASSERT_CMP i       -> MAC  <$> [IFCMP i Nothing [] [MAC FAIL]]
  ASSERT_NONE        -> PRIM <$> [IF_NONE [] [MAC FAIL]]
  ASSERT_SOME        -> PRIM <$> [IF_NONE [MAC FAIL] []]
  ASSERT_LEFT        -> PRIM <$> [IF_LEFT [] [MAC FAIL]]
  ASSERT_RIGHT       -> PRIM <$> [IF_LEFT [MAC FAIL] []]
  PAPAIR ps t v      -> expandPapair ps t v
  UNPAIR ps          -> expandUnpapair ps
  CADR c v f         -> expandCadr c v f
  SET_CADR c v f     -> expandSetCadr c v f
  MAP_CADR c v f ops -> expandMapCadr c v f ops
  DIIP 2 ops         -> [PRIM $ DIP [PRIM $ DIP (xp ops)]]
  DIIP n ops         -> [PRIM $ DIP [MAC $ DIIP (n - 1) (xp ops)]]
  DUUP 2 v           -> [PRIM $ DIP [PRIM $ DUP v]]
  DUUP n v           -> [PRIM $ DIP [MAC $ DUUP (n - 1) v]]
  where
    xp = fmap expand

-- the correctness of type-annotation expansion is currently untested, as these
-- expansions are not explicitly documented in the Michelson Specification
expandPapair :: PairStruct -> M.TypeNote -> VarNote -> [Op]
expandPapair ps t v = case ps of
  P (F a) (F b) -> [PRIM $ PAIR t v (snd a) (snd b)]
  P (F a) r     -> PRIM <$> [DIP [MAC $ PAPAIR r n n], PAIR t v (snd a) n]
  P l     (F b) -> [PRIM $ PAIR n n n (snd b), MAC $ PAPAIR l t v]
  P l     r     -> [MAC $ PAPAIR r n n, MAC $ PAPAIR l n n, PRIM $ PAIR t v n n]
  where
    n = Nothing

expandUnpapair :: PairStruct -> [Op]
expandUnpapair = \case
  P (F (v,f)) (F (w,g)) -> PRIM <$> [DUP Nothing, CAR v f, DIP [PRIM $ CDR w g]]
  P (F a) r             -> [MAC $ UNPAIR (F a), PRIM $ DIP [MAC $ UNPAIR r]]
  P l     (F b)         -> [MAC $ UNPAIR (F b), MAC $ UNPAIR l]
  P l      r            -> MAC <$> [UNPAIR (P fn fn), UNPAIR l, UNPAIR r]
  where
    fn = F (Nothing, Nothing)

expandCadr :: [CadrStruct] -> VarNote -> FieldNote -> [Op]
expandCadr cs v f = case cs of
  A:[] -> [PRIM $ CAR v f]
  D:[] -> [PRIM $ CDR v f]
  A:cs -> [PRIM $ CAR Nothing Nothing, MAC $ CADR cs v f]
  D:cs -> [PRIM $ CDR Nothing Nothing, MAC $ CADR cs v f]

expandSetCadr :: [CadrStruct] -> VarNote -> FieldNote -> [Op]
expandSetCadr cs v f = PRIM <$> case cs of
  A:[] -> [CDR v f, SWAP, pairN]
  D:[] -> [CAR v f, pairN]
  A:cs -> [DUP n, DIP [PRIM carN, MAC $ SET_CADR cs v f], cdrN, SWAP, pairN]
  D:cs -> [DUP n, DIP [PRIM cdrN, MAC $ SET_CADR cs v f], cdrN, SWAP, pairN]
  where
    n = Nothing
    carN = CAR n n
    cdrN = CDR n n
    pairN = PAIR n n n n

expandMapCadr :: [CadrStruct] -> VarNote -> FieldNote -> [Op] -> [Op]
expandMapCadr cs v f ops = case cs of
  A:[] -> PRIM <$> [DUP n, cdrN, DIP [PRIM $ CAR v f, SEQ ops], SWAP, pairN]
  D:[] ->
    concat [PRIM <$> [DUP n, CDR v f], [SEQ ops], PRIM <$> [SWAP, carN, pairN]]
  A:cs ->
    PRIM <$> [DUP n, DIP [PRIM $ carN, MAC $ MAP_CADR cs v f ops], cdrN, pairN]
  D:cs ->
    PRIM <$> [DUP n, DIP [PRIM $ cdrN, MAC $ MAP_CADR cs v f ops], carN, pairN]
  where
    n = Nothing
    carN = CAR n n
    cdrN = CDR n n
    pairN = PAIR n n n n

expandPrim :: I -> I
expandPrim = \case
  IF_NONE bt bf          -> IF_NONE (xp bt) (xp bf)
  IF_LEFT bt bf          -> IF_LEFT (xp bt) (xp bf)
  IF_RIGHT bt bf         -> IF_RIGHT (xp bt) (xp bf)
  MAP v ops              -> MAP v (xp ops)
  ITER v ops             -> ITER v (xp ops)
  IF bt bf               -> IF (xp bt) (xp bf)
  LOOP ops               -> LOOP (xp ops)
  LOOP_LEFT ops          -> LOOP_LEFT (xp ops)
  LAMBDA v a b ops       -> LAMBDA v a b (xp ops)
  DIP ops                -> DIP (xp ops)
  CREATE_CONTRACT2 v w c -> CREATE_CONTRACT2 v w (xp' c)
  x                      -> x
  where
    xp = fmap expand
    xp' c = Contract (para c) (stor c) (xp $ code c)

mapLeaves :: [(VarNote, FieldNote)] -> PairStruct -> PairStruct
mapLeaves fs p = evalState (leavesST p) fs

leavesST :: PairStruct -> State [(VarNote, FieldNote)] PairStruct
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
    getLeaf _      = ((Nothing, Nothing), [])
