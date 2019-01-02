{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Michelson.Macro where

import qualified Data.Text                as T

import           Control.Monad.State.Lazy
import           Language.Michelson.Types (CadrStruct (..), FieldNote, I (..),
                                           Macro (..), Op (..), PairStruct (..),
                                           VarNote (..))
import qualified Language.Michelson.Types as M

expand :: Op -> Op
expand (MAC m)  = (SEQ $ expandMacro m)
expand (PRIM i) = (PRIM $ expandPrim i)
expand (SEQ s)  = (SEQ $ expand <$> s)

expandMacro :: Macro -> [Op]
expandMacro = (fmap expand) . \case
  CMP i v             -> PRIM <$> [COMPARE v, i]
  IFX i bt bf         -> PRIM <$> [i, IF (xp bt) (xp bf)]
  IFCMP i v bt bf     -> PRIM <$> [COMPARE v, i, (IF (xp bt) (xp bf))]
  IF_SOME bt bf       -> PRIM <$> [IF_NONE (xp bf) (xp bt)]
  FAIL                -> PRIM <$> [UNIT Nothing Nothing, FAILWITH]
  ASSERT              -> PRIM <$> [IF [] [MAC FAIL]]
  ASSERTX i           -> MAC  <$> [IFX i [] [MAC FAIL]]
  ASSERT_CMP i        -> MAC  <$> [IFCMP i Nothing [] [MAC FAIL]]
  ASSERT_NONE         -> PRIM <$> [IF_NONE [] [MAC FAIL]]
  ASSERT_SOME         -> PRIM <$> [IF_NONE [MAC FAIL] []]
  ASSERT_LEFT         -> PRIM <$> [IF_LEFT [] [MAC FAIL]]
  ASSERT_RIGHT        -> PRIM <$> [IF_LEFT [MAC FAIL] []]
  PAPAIR ps t v       -> expandPapair ps t v
  UNPAIR ps           -> expandUnpapair ps
  CADR (A:[]) v f     -> [PRIM $ CAR v f]
  CADR (D:[]) v f     -> [PRIM $ CDR v f]
  CADR (A:cs) v f     -> [PRIM $ CAR Nothing Nothing, MAC $ CADR cs v f]
  CADR (D:cs) v f     -> [PRIM $ CDR Nothing Nothing, MAC $ CADR cs v f]
  SET_CADR c v f      -> undefined
  MAP_CADR c v f ops  -> undefined
  DIIP 2 ops          -> [PRIM $ DIP [PRIM $ DIP (xp ops)]]
  DIIP n ops          -> [PRIM $ DIP [MAC $ DIIP (n - 1) (xp ops)]]
  DUUP 2 v            -> [PRIM $ DIP [PRIM $ DUP v]]
  DUUP n v            -> [PRIM $ DIP [MAC $ DUUP (n - 1) v]]
  where
    xp = fmap expand

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

expandPrim :: I -> I
expandPrim = \case
  (IF_NONE bt bf)    -> IF_NONE (xp bt) (xp bf)
  (IF_LEFT bt bf)    -> IF_LEFT (xp bt) (xp bf)
  (IF_RIGHT bt bf)   -> IF_RIGHT (xp bt) (xp bf)
  (MAP v ops)        -> MAP v (xp ops)
  (ITER v ops)       -> ITER v (xp ops)
  (IF bt bf)         -> IF (xp bt) (xp bf)
  (LOOP ops)         -> LOOP (xp ops)
  (LOOP_LEFT ops)    -> LOOP_LEFT (xp ops)
  (LAMBDA v a b ops) -> LAMBDA v a b (xp ops)
  (DIP ops)          -> DIP (xp ops)
  (CREATE_CONTRACT2 v w (M.Contract p s (M.Code ops)))
                     -> CREATE_CONTRACT2 v w $ M.Contract p s (M.Code (xp ops))
  x                  -> x
  where
    xp = fmap expand

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
