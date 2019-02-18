{-# LANGUAGE GADTs, TypeApplications #-}

-- | Module, containing function to interpret Michelson
-- instructions against given context and input stack.
module Advanced.Interpreter
  ( doInstr
  , ContractEnv (..)
  , Operation (..)
  , run
  ) where

import Control.Monad.Except (Except, MonadError, throwError)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Vinyl (Rec(..), (<+>))


import Advanced.Type (CT(..), T(..))
import Advanced.Value

-- | Data type, representing operation, list of which is returned
-- by Michelson contract (according to calling convention).
--
-- These operations are to be further executed against system state
-- after the contract execution.
data Operation where
  TransferTokens :: Show p => p -> Int64 -> Address -> Operation

deriving instance Show Operation

data ContractEnv = ContractEnv
data MichelsonFailed = MichelsonFailed

-- newtype MichelsonFailed op t = MichelsonFailed
  -- { unMichelsonFailed :: Val op t
  -- } deriving (Show)

newtype EvalOp a = EvalOp
  { runEvalOp :: ExceptT MichelsonFailed (Reader ContractEnv) a
  } deriving (Functor, Applicative, Monad, MonadError MichelsonFailed, MonadReader ContractEnv)

doInstr :: EvalOp a -> ContractEnv -> Either MichelsonFailed a
doInstr = runReader . runExceptT . runEvalOp

-- | Function to interpret Michelson instruction(s) against given stack.
run :: (MonadError MichelsonFailed m, MonadReader ContractEnv m)
    => Instr Operation cp inp out
    -> Rec (Val Operation cp) inp
    -> m (Rec (Val Operation cp) out)
run (Seq i1 i2) r = run i1 r >>= \r' -> run i2 r'
run Nop r = pure $ r
run DROP (_ :& r) = pure $ r
run DUP (a :& r) = pure $ a :& a :& r
run SWAP (a :& b :& r) = pure $ b :& a :& r
run (PUSH v) r = pure $ v :& r
run SOME (a :& r) = pure $ VOption (Just a) :& r
run NONE r = pure $ VOption Nothing :& r
run UNIT r = pure $ VUnit :& r
run (IF_NONE _bNone bJust) (VOption (Just a) :& r) = run bJust (a :& r)
run (IF_NONE bNone _bJust) (VOption Nothing :& r) = run bNone r
run PAIR (a :& b :& r) = pure $ VPair (a, b) :& r
run CAR (VPair (a, _b) :& r) = pure $ a :& r
run CDR (VPair (_a, b) :& r) = pure $ b :& r
run LEFT (a :& r) = pure $ (VOr $ Left a) :& r
run RIGHT (b :& r) = pure $ (VOr $ Right b) :& r
run (IF_LEFT bLeft _) (VOr (Left a) :& r) = run bLeft (a :& r)
run (IF_LEFT _ bRight) (VOr (Right a) :& r) = run bRight (a :& r)
run (IF_RIGHT bRight _) (VOr (Right a) :& r) = run bRight (a :& r)
run (IF_RIGHT _ bLeft) (VOr (Left a) :& r) = run bLeft (a :& r)
-- More here
run NIL r = pure $ VList [] :& r
run CONS (a :& VList l :& r) = pure $ VList (a : l) :& r
run (IF_CONS _ bNil) (VList [] :& r) = run bNil r
run (IF_CONS bCons _) (VList (lh : lr) :& r) = run bCons (lh :& VList lr :& r)
run SIZE (VList a :& r) = pure $ VC (CvNat $ fromIntegral $ length a) :& r
-- run (MAP ops) (VMap a :& r) =  :& r
run (ITER _) (VList [] :& r) = pure $ r
run (ITER e) (VList (lh : lr) :& r) = do
  res <- run e (lh :& r)
  run (ITER e) (VList lr :& res)
run MEM (VC a :& VSet b :& r) = pure $ VC (CvBool (S.member a b)) :& r
run GET (VC a :& VMap b :& r) = pure $ VOption (M.lookup a b) :& r
run UPDATE (VC a :& VC (CvBool True) :& VSet c :& r) = pure $ VSet (S.insert a c) :& r
run UPDATE (VC a :& VC (CvBool False) :& VSet c :& r) = pure $ VSet (S.delete a c) :& r
run (IF bTrue _) (VC (CvBool True) :& r) = run bTrue r
run (IF _ bFalse) (VC (CvBool False) :& r) = run bFalse r
run (LOOP _) (VC (CvBool False) :& r) = pure $ r
run (LOOP ops) (VC (CvBool True) :& r) = do
  res <- run ops r
  run (LOOP ops) res
run (LOOP_LEFT _) (VOr (Right a) :&r) = pure $ a :& r
run (LOOP_LEFT ops) (VOr (Left a) :& r) = do
  res <- run ops (a :& r)
  run (LOOP_LEFT ops) res
run EXEC (a :& VLam lBody :& r) = do
  res <- run lBody (a :& RNil)
  pure $ res <+> r
-- More here
run (DIP i) (a :& r) = do
  res <- run i r
  pure $ a :& res
run FAILWITH _ = throwError $ MichelsonFailed
run CONCAT (VC (CvString a) :& VC (CvString b) :& r) = pure $ VC (CvString $ a <> b) :& r
run SLICE (VC (CvNat o) :& VC (CvNat l) :& VC (CvString s) :& r) = pure $
  if o > fromIntegral (length s) || o + l > fromIntegral (length s)
  then VOption Nothing :& r
  else VOption ((Just . VC . CvString . toText) $ sliceText o l s) :& r
  where
    sliceText :: Natural -> Natural -> Text -> Text
    sliceText o' l' s' = T.take (natToInt l') $ T.takeEnd ((length s') - natToInt o') s'

    natToInt :: Natural -> Int
    natToInt = fromIntegral . toInteger

-- More here
run ADD (VC l :& VC r :& rest) = pure $ VC (evalOp (Proxy @Add) l r) :& rest
run SUB (VC l :& VC r :& rest) = pure $ VC (evalOp (Proxy @Sub) l r) :& rest
run MUL (VC l :& VC r :& rest) = pure $ VC (evalOp (Proxy @Mul) l r) :& rest
run EDIV (VC l :& VC r :& rest) = pure $ evalEDivOp l r :& rest
run ABS (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Abs) a) :& rest
run NEG (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Neg) a) :& rest
run MOD (VC l :& VC r :& rest) = pure $ evalModOp l r :& rest
run LSL (VC x :& VC s :& rest) = pure $ VC (evalOp (Proxy @Lsl) x s) :& rest
run LSR (VC x :& VC s :& rest) = pure $ VC (evalOp (Proxy @Lsr) x s) :& rest
run OR (VC l :& VC r :& rest) = pure $ VC (evalOp (Proxy @Or) l r) :& rest
run AND (VC l :& VC r :& rest) = pure $ VC (evalOp (Proxy @And) l r) :& rest
run XOR (VC l :& VC r :& rest) = pure $ VC (evalOp (Proxy @Xor) l r) :& rest
run NOT (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Not) a) :& rest
run COMPARE (VC l :& VC r :& rest) = pure $ VC (evalOp (Proxy @Compare) l r) :& rest
run Advanced.Value.EQ (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Eq') a) :& rest
run NEQ (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Neq) a) :& rest
run Advanced.Value.LT (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Lt) a) :& rest
run Advanced.Value.GT (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Gt) a) :& rest
run LE (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Le) a) :& rest
run GE (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Ge) a) :& rest
-- More herec
run TRANSFER_TOKENS (p :& VC (CvMutez mutez) :& VContract addr :& r) = pure $ VOp (TransferTokens p mutez addr) :& r
--------------------
-- Examples
--------------------

-- | @myInstr@ is an equivalent to Michelson code:
--
--    PUSH int 223;
--    SOME;
--    IF_NONE { DUP; } { SWAP; };
--    ADD;
--    PUSH nat 12
--    ADD;
myInstr :: Instr op cp ('T_c 'T_int : s) ('T_c 'T_int : s)
myInstr =
  PUSH (VC $ CvInt 223) #
  SOME #
  IF_NONE DUP SWAP #
  ADD #
  PUSH (VC $ CvNat 12) #
  ADD

-- | @myInstr2@ can not be represented in Michelson
-- syntax as Michelson has no way to directly push value
-- of type "option int"
_myInstr2 :: Instr op cp a ('T_option ('T_c 'T_int) : a)
_myInstr2 =
  PUSH (VOption $ Just $ VC $ CvInt 223) #
  Nop

-- _myInstrEvaluated :: Rec (Val Operation) '[ 'T_c 'T_int ]
-- _myInstrEvaluated = run myInstr (VC (CvInt 90) :& RNil)
