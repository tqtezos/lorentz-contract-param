-- | Module, containing function to interpret Michelson
-- instructions against given context and input stack.
module Michelson.Interpret
  ( ContractEnv (..)
  , MichelsonFailed (..)
  , michelsonInterpreter

  , interpret
  ) where

import Prelude hiding (EQ, GT, LT)

import Control.Monad.Except (MonadError, throwError)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Singletons (SingI(..))
import Data.Typeable ((:~:)(..), eqT)
import Data.Vinyl (Rec(..), (<+>))

import Michelson.TypeCheck
  (ContractInp, ContractOut, SomeContract(..), SomeVal(..), TcNopHandler, TypeCheckEnv(..),
  typeCheckVal)
import Michelson.Typed
  (CVal(..), Instr(..), Operation(..), SetDelegate(..), Sing(..), T(..), TransferTokens(..),
  Val(..), fromMType)
import qualified Michelson.Typed as Typed
import Michelson.Typed.Arith
import Michelson.Typed.Polymorphic
import Michelson.Untyped (Op(..), Value(..))
import qualified Michelson.Untyped as M
import Tezos.Address (Address)
import Tezos.Core (Mutez, Timestamp)
import Tezos.Crypto (blake2b, checkSignature, hashKey, sha256, sha512)

-- | Environment for contract execution.
data ContractEnv nop = ContractEnv
  { ceNow :: !Timestamp
  -- ^ Timestamp of the block whose validation triggered this execution.
  , ceMaxSteps :: !Word64
  -- ^ Number of steps after which execution unconditionally terminates.
  , ceBalance :: !Mutez
  -- ^ Current amount of mutez of the current contract.
  , ceStorage :: M.Value (M.Op nop)
  -- ^ Storage value associated with the current contract.
  , ceContracts :: Map Address (M.Contract (M.Op nop))
  -- ^ Mapping from existing contracts' addresses to their executable
  -- representation.
  , ceParameter :: M.Value (M.Op nop)
  -- ^ Parameter passed to the contract.
  , ceSource :: !Address
  -- ^ The contract that initiated the current transaction.
  , ceSender :: !Address
  -- ^ The contract that initiated the current internal transaction.
  , ceAmount :: !Mutez
  -- ^ Amount of the current transaction.
  }

-- | Represents `[FAILED]` state of a Michelson program. Contains
-- value that was on top of the stack when `FAILWITH` was called.
data MichelsonFailedProper nop = MichelsonFailedProper
  -- { unMichelsonFailed :: Value (Op nop) } -- TODO uncomment when conversion to Value will be implemented
  deriving (Show)

data MichelsonFailed where
  RuntimeFailure :: Val cp t -> MichelsonFailed
  ValInvalidType :: Val cp t -> MichelsonFailed
  InstrInvalidType :: Instr op cp int -> MichelsonFailed
  IllTypedContract :: MichelsonFailed
  IllTypedInputParameters :: Text -> MichelsonFailed
  -- InvalidStorageParameter :: MichelsonFailed

deriving instance Show MichelsonFailed

-- | Interpret a contract without performing any side effects.
michelsonInterpreter
  :: forall instr nop.(SingI instr, Typeable instr, Show nop)
  => TcNopHandler nop
  -> ContractEnv nop
  -> SomeContract
  -> Either MichelsonFailed ([Typed.Operation (Typed.Instr instr)], Value (Op nop))
michelsonInterpreter nopHandler cEnv@ContractEnv{..} someContract =
  case someContract of
    (SomeContract (instr :: Instr cp (ContractInp cp st)
                                                     (ContractOut st)) _ _) -> do
      Refl <- maybe (Left $ InstrInvalidType instr) pure $
        eqT @cp @instr
      case interpret @cp @st nopHandler cEnv instr of
        Right (Typed.VPair (Typed.VList ops, newStorage)) ->
          Right (map Typed.unsafeValToOperation ops, Typed.unsafeValToValue newStorage)
        Left e -> Left e

interpret
  :: forall cp st nop.(SingI cp, Typeable cp, SingI st, Typeable st, Show nop)
  => TcNopHandler nop
  -> ContractEnv nop
  -> Instr cp (ContractInp cp st) (ContractOut st)
  -> Either MichelsonFailed (Val (Instr cp) ('T_pair ('T_list 'T_operation) st))
interpret nopHandler env instr =
  case interpretImpl @cp @st @nop nopHandler env instr of
    Left e -> Left e
    Right ((res :: Val (Instr cp1) ('T_pair ('T_list 'T_operation) st1)) :& RNil) -> do
      Refl <- maybe (Left $ ValInvalidType res) pure $
        eqT @cp @cp1
      Refl <- maybe (Left $ ValInvalidType res) pure $
        eqT @st @st1
      pure res

interpretImpl
  :: forall cp st nop.(Typeable cp, SingI cp, SingI st, Typeable st, Show nop)
  => TcNopHandler nop
  -> ContractEnv nop
  -> Instr cp (ContractInp cp st) (ContractOut st)
  -> Either MichelsonFailed (Rec (Val (Instr cp)) (ContractOut st))
interpretImpl nopHandler env@ContractEnv{..} instr = do
  case makeVPairSomeVal (toMType $ fromSingT (sing @cp), toMType $ fromSingT (sing @st))
    (ceParameter, ceStorage) of
    Left e -> Left e
    Right ((pair :: Val (Instr cp) tPair) :::: _) -> do
      Refl <- maybe (Left $ ValInvalidType pair) pure $
        eqT @tPair @('T_pair cp st)
      runEvalOp (runInstr instr (pair :& RNil)) env
  where
    makeVPairSomeVal
      :: (M.Type, M.Type)
      -> (M.Value (M.Op nop), M.Value (M.Op nop))
      -> Either MichelsonFailed (SomeVal cp)
    makeVPairSomeVal (tl, tr) (vl, vr) =
      case usingReader (TypeCheckEnv nopHandler) . runExceptT $
        typeCheckVal (M.ValuePair vl vr) $
        fromMType $ M.Type (M.T_pair M.noAnn M.noAnn tl tr) M.noAnn of
        Left e -> Left $ IllTypedInputParameters $ show e
        Right val -> Right val

newtype EvalOp nop a = EvalOp
  { unEvalOp :: ExceptT MichelsonFailed (Reader (ContractEnv nop)) a
  } deriving ( Functor, Applicative, Monad, MonadError MichelsonFailed
              , MonadReader (ContractEnv nop))

runEvalOp :: EvalOp nop a -> ContractEnv nop -> Either MichelsonFailed a
runEvalOp = runReader . runExceptT . unEvalOp

-- | Function to interpret Michelson instruction(s) against given stack.
runInstr
    :: Instr cp inp out
    -> Rec (Val (Instr cp)) inp
    -> EvalOp nop (Rec (Val (Instr cp)) out)
runInstr (Seq i1 i2) r = runInstr i1 r >>= \r' -> runInstr i2 r'
runInstr Nop r = pure $ r
runInstr DROP (_ :& r) = pure $ r
runInstr DUP (a :& r) = pure $ a :& a :& r
runInstr SWAP (a :& b :& r) = pure $ b :& a :& r
runInstr (PUSH v) r = pure $ v :& r
runInstr SOME (a :& r) = pure $ VOption (Just a) :& r
runInstr NONE r = pure $ VOption Nothing :& r
runInstr UNIT r = pure $ VUnit :& r
runInstr (IF_NONE _bNone bJust) (VOption (Just a) :& r) = runInstr bJust (a :& r)
runInstr (IF_NONE bNone _bJust) (VOption Nothing :& r) = runInstr bNone r
runInstr PAIR (a :& b :& r) = pure $ VPair (a, b) :& r
runInstr CAR (VPair (a, _b) :& r) = pure $ a :& r
runInstr CDR (VPair (_a, b) :& r) = pure $ b :& r
runInstr LEFT (a :& r) = pure $ (VOr $ Left a) :& r
runInstr RIGHT (b :& r) = pure $ (VOr $ Right b) :& r
runInstr (IF_LEFT bLeft _) (VOr (Left a) :& r) = runInstr bLeft (a :& r)
runInstr (IF_LEFT _ bRight) (VOr (Right a) :& r) = runInstr bRight (a :& r)
runInstr (IF_RIGHT bRight _) (VOr (Right a) :& r) = runInstr bRight (a :& r)
runInstr (IF_RIGHT _ bLeft) (VOr (Left a) :& r) = runInstr bLeft (a :& r)
-- More here
runInstr NIL r = pure $ VList [] :& r
runInstr CONS (a :& VList l :& r) = pure $ VList (a : l) :& r
runInstr (IF_CONS _ bNil) (VList [] :& r) = runInstr bNil r
runInstr (IF_CONS bCons _) (VList (lh : lr) :& r) = runInstr bCons (lh :& VList lr :& r)
runInstr SIZE (a :& r) = pure $ VC (CvNat $ (fromInteger . toInteger) $ evalSize a) :& r
runInstr EMPTY_SET r = pure $ VSet Set.empty :& r
runInstr EMPTY_MAP r = pure $ VMap Map.empty :& r
-- TODO: make MAP and ITER polymorphic in orger to get rid of error "unexpected call"
runInstr (MAP ops) (VMap a :& r) = do
  newList <- mapM (\(key, value) -> do
    res <- runInstr ops (VPair (VC key, value) :& r)
    case res of
      ((newValue :: Val (Instr cp) t) :& _) -> pure (key, newValue)) $ Map.toAscList a
  pure $ ((VMap . Map.fromList) newList) :& r
runInstr (MAP ops) (VList a :& r) = do
  newList <- mapM (\x -> do
    res <- runInstr ops (x :& r)
    case res of
      ((newX :: Val (Instr cp) t) :& _) -> pure newX) a
  pure $ VList newList :& r
runInstr (MAP _) (_) = error "unexpected call"
runInstr (ITER _) (VList [] :& r) = pure $ r
runInstr (ITER ops) (VList (lh : lr) :& r) = do
  res <- runInstr ops (lh :& r)
  runInstr (ITER ops) (VList lr :& res)
runInstr (ITER ops) (VSet s :& r) = do
  let ascList = map (\x -> VC x) $ Set.toAscList s
  runInstr (ITER ops) (VList ascList :& r)
runInstr (ITER ops) (VMap m :& r) = do
  let ascList = map (\(key, value) -> VPair (VC key, value)) $ Map.toAscList m
  runInstr (ITER ops) (VList ascList :& r)
runInstr (ITER _) (_) = error "unexpected call"
runInstr MEM (VC a :& b :& r) = pure $ VC (CvBool (evalMem a b)) :& r
runInstr GET (VC a :& b :& r) = pure $ VOption (evalGet a b) :& r
runInstr UPDATE (VC a :& b :& c :& r) = pure $ evalUpd a b c :& r
runInstr (IF bTrue _) (VC (CvBool True) :& r) = runInstr bTrue r
runInstr (IF _ bFalse) (VC (CvBool False) :& r) = runInstr bFalse r
runInstr (LOOP _) (VC (CvBool False) :& r) = pure $ r
runInstr (LOOP ops) (VC (CvBool True) :& r) = do
  res <- runInstr ops r
  runInstr (LOOP ops) res
runInstr (LOOP_LEFT _) (VOr (Right a) :&r) = pure $ a :& r
runInstr (LOOP_LEFT ops) (VOr (Left a) :& r) = do
  res <- runInstr ops (a :& r)
  runInstr (LOOP_LEFT ops) res
runInstr (LAMBDA lam) r = pure $ lam :& r
runInstr EXEC (a :& VLam lBody :& r) = do
  res <- runInstr lBody (a :& RNil)
  pure $ res <+> r
-- More here
runInstr (DIP i) (a :& r) = do
  res <- runInstr i r
  pure $ a :& res
runInstr FAILWITH (a :& _) = throwError $ RuntimeFailure a
runInstr FAILWITH (_) = error "cannot FAILWITH on empty stack"
runInstr CAST (a :& r) = pure $ a :& r
runInstr RENAME (a :& r) = pure $ a :& r
-- TODO
runInstr PACK (_ :& _) = error "PACK not implemented yet"
runInstr UNPACK (_ :& _) = error "UNPACK not implemented yet"
runInstr CONCAT (a :& b :& r) = pure $ evalConcat a b :& r
runInstr CONCAT' (VList a :& r) = pure $ evalConcat' a :& r
runInstr SLICE (VC (CvNat o) :& VC (CvNat l) :& s :& r) =
  pure $ VOption (evalSlice o l s) :& r
runInstr ISNAT (VC (CvInt i) :& r) =
  if i < 0
  then pure $ VOption Nothing :& r
  else pure $ VOption (Just $ VC (CvNat $ fromInteger i)) :& r
runInstr ADD (VC l :& VC r :& rest) = pure $ VC (evalOp (Proxy @Add) l r) :& rest
runInstr SUB (VC l :& VC r :& rest) = pure $ VC (evalOp (Proxy @Sub) l r) :& rest
runInstr MUL (VC l :& VC r :& rest) = pure $ VC (evalOp (Proxy @Mul) l r) :& rest
runInstr EDIV (VC l :& VC r :& rest) = pure $ evalEDivOp l r :& rest
runInstr ABS (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Abs) a) :& rest
runInstr NEG (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Neg) a) :& rest
runInstr LSL (VC x :& VC s :& rest) = pure $ VC (evalOp (Proxy @Lsl) x s) :& rest
runInstr LSR (VC x :& VC s :& rest) = pure $ VC (evalOp (Proxy @Lsr) x s) :& rest
runInstr OR (VC l :& VC r :& rest) = pure $ VC (evalOp (Proxy @Or) l r) :& rest
runInstr AND (VC l :& VC r :& rest) = pure $ VC (evalOp (Proxy @And) l r) :& rest
runInstr XOR (VC l :& VC r :& rest) = pure $ VC (evalOp (Proxy @Xor) l r) :& rest
runInstr NOT (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Not) a) :& rest
runInstr COMPARE (VC l :& VC r :& rest) =
  pure $ VC (evalOp (Proxy @Compare) l r) :& rest
runInstr Typed.EQ (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Eq') a) :& rest
runInstr NEQ (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Neq) a) :& rest
runInstr Typed.LT (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Lt) a) :& rest
runInstr Typed.GT (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Gt) a) :& rest
runInstr LE (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Le) a) :& rest
runInstr GE (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Ge) a) :& rest
runInstr INT (VC (CvNat n) :& r) = pure $ VC (CvInt $ toInteger n) :& r
runInstr SELF r = do
  ContractEnv{..} <- ask
  pure $ VContract ceSource :& r
runInstr CONTRACT (VC (CvAddress addr) :& r) = do
  ContractEnv{..} <- ask
  if Map.member addr ceContracts
  then pure $ VOption (Just $ VContract addr) :& r
  else pure $ VOption Nothing :& r
runInstr TRANSFER_TOKENS (p :& VC (CvMutez mutez) :& contract :& r) =
  pure $ VOp (OpTransferTokens $ TransferTokens p mutez contract) :& r
runInstr SET_DELEGATE (VOption mbKeyHash :& r) =
  case mbKeyHash of
    Just (VC (CvKeyHash k)) -> pure $ VOp (OpSetDelegate $ SetDelegate $ Just k) :& r
    Nothing -> pure $ VOp (OpSetDelegate $ SetDelegate $ Nothing) :& r
-- TODO
runInstr CREATE_ACCOUNT
  (VC (CvKeyHash _k) :& VOption _mbKeyHash :&
    (VC (CvBool _b)) :& (VC (CvMutez _m)) :& _r) =
      error "not implemented yet:("
runInstr CREATE_CONTRACT
  (VC (CvKeyHash _k) :& VOption _mbKeyHash :& (VC (CvBool _b2)) :&
    (VC (CvBool _b1)) :& (VC (CvMutez _m)) :& VLam _ops :& _g :& _r) =
      error "not implemented yet:("
runInstr (CREATE_CONTRACT2 _ops)
  (VC (CvKeyHash _k) :& VOption _mbKeyHash :& (VC (CvBool _b2)) :&
    (VC (CvBool _b1)) :& (VC (CvMutez _m)) :& _g :& _r) =
      error "not implemented yet:("
runInstr IMPLICIT_ACCOUNT (VC (CvKeyHash _k) :& _r) =
  error "not implemented yet:("
runInstr NOW _r = error "not implemented yet:("
  -- ContractEnv{..} <- ask
  -- pure $ VC (CvTimestamp ceNow) :& r
runInstr AMOUNT r = do
  ContractEnv{..} <- ask
  pure $ VC (CvMutez ceAmount) :& r
runInstr BALANCE r = do
  ContractEnv{..} <- ask
  pure $ VC (CvMutez ceBalance) :& r
runInstr CHECK_SIGNATURE (VKey k :& VSignature v :&
  VC (CvBytes b) :& r) = pure $ VC (CvBool $ checkSignature k v b) :& r
runInstr SHA256 (VC (CvBytes b) :& r) = pure $ VC (CvBytes $ sha256 b) :& r
runInstr SHA512 (VC (CvBytes b) :& r) = pure $ VC (CvBytes $ sha512 b) :& r
runInstr BLAKE2B (VC (CvBytes b) :& r) = pure $ VC (CvBytes $ blake2b b) :& r
runInstr HASH_KEY (VKey k :& r) = pure $ VC (CvKeyHash $ hashKey k) :& r
-- TODO
runInstr STEPS_TO_QUOTA _r = error "not implemented yet:("
runInstr SOURCE r = do
  ContractEnv{..} <- ask
  pure $ VC (CvAddress ceSource) :& r
runInstr SENDER r = do
  ContractEnv{..} <- ask
  pure $ VC (CvAddress ceSender) :& r
runInstr ADDRESS (VContract a :& r) = pure $ VC (CvAddress a) :& r
