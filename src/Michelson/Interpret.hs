-- | Module, containing function to interpret Michelson
-- instructions against given context and input stack.
module Michelson.Interpret
  ( ContractEnv (..)
  , MichelsonFailed (..)

  , interpret
  , ContractReturn

  , interpretUntyped
  , InterpretUntypedError (..)
  , InterpretUntypedResult (..)
  ) where

import Prelude hiding (EQ, GT, LT)

import Control.Monad.Except (MonadError, throwError)
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Singletons (SingI(..))
import Data.Typeable ((:~:)(..))
import Data.Vinyl (Rec(..), (<+>))
import Fmt (Buildable(build), genericF)

import Michelson.TypeCheck
  (SomeContract(..), SomeVal(..), TCError, TcNopHandler, eqT', runTypeCheckT, typeCheckContract,
  typeCheckVal)
import Michelson.Typed
  (CVal(..), Contract, CreateAccount(..), CreateContract(..) , Instr(..), Operation(..),
  SetDelegate(..), Sing(..), T(..), TransferTokens(..), Val(..), fromUType, valToOpOrValue)
import qualified Michelson.Typed as Typed
import Michelson.Typed.Arith
import Michelson.Typed.Convert (convertContract, unsafeValToValue)
import Michelson.Typed.Polymorphic
import qualified Michelson.Untyped as U
import Tezos.Address (Address(..))
import Tezos.Core (Mutez, Timestamp(..))
import Tezos.Crypto (KeyHash, blake2b, checkSignature, hashKey, sha256, sha512)

-- | Environment for contract execution.
data ContractEnv nop = ContractEnv
  { ceNow :: !Timestamp
  -- ^ Timestamp of the block whose validation triggered this execution.
  , ceMaxSteps :: !Word64
  -- ^ Number of steps after which execution unconditionally terminates.
  , ceBalance :: !Mutez
  -- ^ Current amount of mutez of the current contract.
  , ceContracts :: Map Address (U.Contract (U.Op nop))
  -- ^ Mapping from existing contracts' addresses to their executable
  -- representation.
  , ceSource :: !Address
  -- ^ The contract that initiated the current transaction.
  , ceSender :: !Address
  -- ^ The contract that initiated the current internal transaction.
  , ceAmount :: !Mutez
  -- ^ Amount of the current transaction.
  }

-- | Represents `[FAILED]` state of a Michelson program. Contains
-- value that was on top of the stack when `FAILWITH` was called.
data MichelsonFailed where
  MichelsonFailedWith :: Val Instr t -> MichelsonFailed
  MichelsonArithError :: ArithError (CVal n) (CVal m) -> MichelsonFailed
  MichelsonGasExhaustion :: MichelsonFailed

deriving instance Show MichelsonFailed

instance Buildable MichelsonFailed where
  build =
    \case
      MichelsonFailedWith v ->
        "Reached FAILWITH instruction with " <> formatValue v
      MichelsonArithError v -> build v
      MichelsonGasExhaustion ->
        "Gas limit exceeded on contract execution"
    where
      formatValue v =
        -- Pass `Bool` as `nop`, because it's not essential and we
        -- need 'Buildable' for 'nop'.
        case valToOpOrValue @_ @Bool v of
          Left op ->
            case op of
              OpTransferTokens {} -> "TransferTokens"
              OpSetDelegate {} -> "SetDelegate"
              OpCreateAccount {} -> "CreateAccount"
              OpCreateContract {} -> "CreateContract"
          Right untypedV -> build untypedV

data InterpretUntypedError nop
  = RuntimeFailure MichelsonFailed
  | IllTypedContract (TCError nop)
  | IllTypedParam (TCError nop)
  | IllTypedStorage (TCError nop)
  | UnexpectedParamType Text
  | UnexpectedStorageType Text
  deriving (Show, Generic)

instance Buildable nop => Buildable (InterpretUntypedError nop) where
  build = genericF

data InterpretUntypedResult where
  InterpretUntypedResult
    :: ( Typeable st
       , SingI st
       )
    => { iurOps :: [ Operation Instr ]
       , iurNewStorage :: Val Instr st
       }
    -> InterpretUntypedResult

-- | Interpret a contract without performing any side effects.
interpretUntyped
  :: (Aeson.ToJSON nop, Show nop, Buildable nop)
  => TcNopHandler nop
  -> U.Contract (U.Op nop)
  -> U.Value (U.Op nop)
  -> U.Value (U.Op nop)
  -> ContractEnv nop
  -> Either (InterpretUntypedError nop) InterpretUntypedResult
interpretUntyped nopHandler' U.Contract{..} paramU initStU env = do
    (SomeContract (instr :: Contract cp st) _ _)
       <- first IllTypedContract $ typeCheckContract nopHandler'
              (U.Contract para stor (U.unOp <$> code))
    paramV :::: ((_ :: Sing cp1), _)
       <- first IllTypedParam $ runTypeCheckT nopHandler' para $
            typeCheckVal paramU (fromUType para)
    initStV :::: ((_ :: Sing st1), _)
       <- first IllTypedStorage $ runTypeCheckT nopHandler' para $
            typeCheckVal initStU (fromUType stor)
    Refl <- first UnexpectedStorageType $ eqT' @st @st1
    Refl <- first UnexpectedParamType   $ eqT' @cp @cp1
    fmap (uncurry InterpretUntypedResult) $
      first RuntimeFailure $
      interpret instr paramV initStV env

interpret
  :: Aeson.ToJSON nop
  => Contract cp st
  -> Val Instr cp
  -> Val Instr st
  -> ContractEnv nop
  -> ContractReturn st
interpret instr param initSt env = fmap toRes $
  runEvalOp (runInstr instr (VPair (param, initSt) :& RNil)) env (RemainingSteps $ ceMaxSteps env)
  where
    toRes
      :: Rec (Val instr) '[ 'T_pair ('T_list 'T_operation) st ]
      -> ([Operation instr], Val instr st)
    toRes (VPair (VList ops_, newSt) :& RNil) =
      (map (\(VOp op) -> op) ops_, newSt)

type ContractReturn st =
  Either MichelsonFailed ([Operation Instr], Val Instr st)

newtype RemainingSteps = RemainingSteps Word64 deriving (Num)

newtype EvalOp nop a = EvalOp
  { unEvalOp :: ExceptT MichelsonFailed (StateT RemainingSteps (Reader (ContractEnv nop))) a
  } deriving ( Functor, Applicative, Monad, MonadError MichelsonFailed
              , MonadState RemainingSteps, MonadReader (ContractEnv nop))

runEvalOp :: EvalOp nop a -> ContractEnv nop -> RemainingSteps -> Either MichelsonFailed a
runEvalOp op env steps = runReader (evalStateT (runExceptT (unEvalOp op)) steps) env

-- | Function to change amount of remaining steps stored in State monad
runInstr
  :: forall inp out nop. Aeson.ToJSON nop
  => Instr inp out
  -> Rec (Val Instr) inp
  -> EvalOp nop (Rec (Val Instr) out)
runInstr i@(Seq _i1 _i2) r = runInstrImpl i r
runInstr i@(Nop) r = runInstrImpl i r
runInstr i r = do
  RemainingSteps rs <- get
  if rs == 0
  then throwError $ MichelsonGasExhaustion
  else do
    modify (subtract 1)
    runInstrImpl i r

-- | Function to interpret Michelson instruction(s) against given stack.
runInstrImpl
    :: forall nop inp out. Aeson.ToJSON nop
    => Instr inp out
    -> Rec (Val Instr) inp
    -> EvalOp nop (Rec (Val Instr) out)
runInstrImpl (Seq i1 i2) r = runInstr i1 r >>= \r' -> runInstr i2 r'
runInstrImpl Nop r = pure $ r
runInstrImpl DROP (_ :& r) = pure $ r
runInstrImpl DUP (a :& r) = pure $ a :& a :& r
runInstrImpl SWAP (a :& b :& r) = pure $ b :& a :& r
runInstrImpl (PUSH v) r = pure $ v :& r
runInstrImpl SOME (a :& r) = pure $ VOption (Just a) :& r
runInstrImpl NONE r = pure $ VOption Nothing :& r
runInstrImpl UNIT r = pure $ VUnit :& r
runInstrImpl (IF_NONE _bNone bJust) (VOption (Just a) :& r) = runInstr bJust (a :& r)
runInstrImpl (IF_NONE bNone _bJust) (VOption Nothing :& r) = runInstr bNone r
runInstrImpl PAIR (a :& b :& r) = pure $ VPair (a, b) :& r
runInstrImpl CAR (VPair (a, _b) :& r) = pure $ a :& r
runInstrImpl CDR (VPair (_a, b) :& r) = pure $ b :& r
runInstrImpl LEFT (a :& r) = pure $ (VOr $ Left a) :& r
runInstrImpl RIGHT (b :& r) = pure $ (VOr $ Right b) :& r
runInstrImpl (IF_LEFT bLeft _) (VOr (Left a) :& r) = runInstr bLeft (a :& r)
runInstrImpl (IF_LEFT _ bRight) (VOr (Right a) :& r) = runInstr bRight (a :& r)
runInstrImpl (IF_RIGHT bRight _) (VOr (Right a) :& r) = runInstr bRight (a :& r)
runInstrImpl (IF_RIGHT _ bLeft) (VOr (Left a) :& r) = runInstr bLeft (a :& r)
-- More here
runInstrImpl NIL r = pure $ VList [] :& r
runInstrImpl CONS (a :& VList l :& r) = pure $ VList (a : l) :& r
runInstrImpl (IF_CONS _ bNil) (VList [] :& r) = runInstr bNil r
runInstrImpl (IF_CONS bCons _) (VList (lh : lr) :& r) = runInstr bCons (lh :& VList lr :& r)
runInstrImpl SIZE (a :& r) = pure $ VC (CvNat $ (fromInteger . toInteger) $ evalSize a) :& r
runInstrImpl EMPTY_SET r = pure $ VSet Set.empty :& r
runInstrImpl EMPTY_MAP r = pure $ VMap Map.empty :& r
runInstrImpl (MAP ops) (a :& r) =
  case ops of
    (code :: Instr (MapOpInp c ': s) (b ': s)) -> do
      newList <- mapM (\(val :: Val Instr (MapOpInp c)) -> do
        res <- runInstr code (val :& r)
        case res of
          ((newVal :: Val Instr b) :& _) -> pure newVal)
        $ mapOpToList @c @b a
      pure $ mapOpFromList a newList :& r
runInstrImpl (ITER ops) (a :& r) =
  case ops of
    (code :: Instr (IterOpEl c ': s) s) ->
      case iterOpDetachOne @c a of
        (Just x, xs) -> do
          res <- runInstr code (x :& r)
          runInstr (ITER code) (xs :& res)
        (Nothing, _) -> pure r
runInstrImpl MEM (VC a :& b :& r) = pure $ VC (CvBool (evalMem a b)) :& r
runInstrImpl GET (VC a :& b :& r) = pure $ VOption (evalGet a b) :& r
runInstrImpl UPDATE (VC a :& b :& c :& r) = pure $ evalUpd a b c :& r
runInstrImpl (IF bTrue _) (VC (CvBool True) :& r) = runInstr bTrue r
runInstrImpl (IF _ bFalse) (VC (CvBool False) :& r) = runInstr bFalse r
runInstrImpl (LOOP _) (VC (CvBool False) :& r) = pure $ r
runInstrImpl (LOOP ops) (VC (CvBool True) :& r) = do
  res <- runInstr ops r
  runInstr (LOOP ops) res
runInstrImpl (LOOP_LEFT _) (VOr (Right a) :&r) = pure $ a :& r
runInstrImpl (LOOP_LEFT ops) (VOr (Left a) :& r) = do
  res <- runInstr ops (a :& r)
  runInstrImpl (LOOP_LEFT ops) res
runInstrImpl (LAMBDA lam) r = pure $ lam :& r
runInstrImpl EXEC (a :& VLam lBody :& r) = do
  res <- runInstr lBody (a :& RNil)
  pure $ res <+> r
runInstrImpl (DIP i) (a :& r) = do
  res <- runInstr i r
  pure $ a :& res
runInstrImpl FAILWITH (a :& _) = throwError $ MichelsonFailedWith a
runInstrImpl CAST (a :& r) = pure $ a :& r
runInstrImpl RENAME (a :& r) = pure $ a :& r
-- TODO
runInstrImpl PACK (_ :& _) = error "PACK not implemented yet"
runInstrImpl UNPACK (_ :& _) = error "UNPACK not implemented yet"
runInstrImpl CONCAT (a :& b :& r) = pure $ evalConcat a b :& r
runInstrImpl CONCAT' (VList a :& r) = pure $ evalConcat' a :& r
runInstrImpl SLICE (VC (CvNat o) :& VC (CvNat l) :& s :& r) =
  pure $ VOption (evalSlice o l s) :& r
runInstrImpl ISNAT (VC (CvInt i) :& r) =
  if i < 0
  then pure $ VOption Nothing :& r
  else pure $ VOption (Just $ VC (CvNat $ fromInteger i)) :& r
runInstrImpl ADD (VC l :& VC r :& rest) =
  (:& rest) <$> runArithOp (Proxy @Add) l r
runInstrImpl SUB (VC l :& VC r :& rest) = (:& rest) <$> runArithOp (Proxy @Sub) l r
runInstrImpl MUL (VC l :& VC r :& rest) = (:& rest) <$> runArithOp (Proxy @Mul) l r
runInstrImpl EDIV (VC l :& VC r :& rest) = pure $ evalEDivOp l r :& rest
runInstrImpl ABS (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Abs) a) :& rest
runInstrImpl NEG (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Neg) a) :& rest
runInstrImpl LSL (VC x :& VC s :& rest) = (:& rest) <$> runArithOp (Proxy @Lsl) x s
runInstrImpl LSR (VC x :& VC s :& rest) = (:& rest) <$> runArithOp (Proxy @Lsr) x s
runInstrImpl OR (VC l :& VC r :& rest) = (:& rest) <$> runArithOp (Proxy @Or) l r
runInstrImpl AND (VC l :& VC r :& rest) = (:& rest) <$> runArithOp (Proxy @And) l r
runInstrImpl XOR (VC l :& VC r :& rest) = (:& rest) <$> runArithOp (Proxy @Xor) l r
runInstrImpl NOT (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Not) a) :& rest
runInstrImpl COMPARE (VC l :& VC r :& rest) = (:& rest) <$> runArithOp (Proxy @Compare) l r
runInstrImpl Typed.EQ (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Eq') a) :& rest
runInstrImpl NEQ (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Neq) a) :& rest
runInstrImpl Typed.LT (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Lt) a) :& rest
runInstrImpl Typed.GT (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Gt) a) :& rest
runInstrImpl LE (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Le) a) :& rest
runInstrImpl GE (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Ge) a) :& rest
runInstrImpl INT (VC (CvNat n) :& r) = pure $ VC (CvInt $ toInteger n) :& r
runInstrImpl SELF r = do
  ContractEnv{..} <- ask
  pure $ VContract ceSource :& r
runInstrImpl CONTRACT (VC (CvAddress addr) :& r) = do
  ContractEnv{..} <- ask
  if Map.member addr ceContracts
  then pure $ VOption (Just $ VContract addr) :& r
  else pure $ VOption Nothing :& r
runInstrImpl TRANSFER_TOKENS (p :& VC (CvMutez mutez) :& contract :& r) =
  pure $ VOp (OpTransferTokens $ TransferTokens p mutez contract) :& r
runInstrImpl SET_DELEGATE (VOption mbKeyHash :& r) =
  case mbKeyHash of
    Just (VC (CvKeyHash k)) -> pure $ VOp (OpSetDelegate $ SetDelegate $ Just k) :& r
    Nothing -> pure $ VOp (OpSetDelegate $ SetDelegate $ Nothing) :& r
runInstrImpl CREATE_ACCOUNT
  (VC (CvKeyHash k) :& VOption mbKeyHash :&
    (VC (CvBool spendable)) :& (VC (CvMutez m)) :& r) =
  pure (VOp (OpCreateAccount $ CreateAccount k (unwrapMbKeyHash mbKeyHash) spendable m)
    :& (VC . CvAddress) (KeyAddress k) :& r)
runInstrImpl CREATE_CONTRACT
  (VC (CvKeyHash k) :& VOption mbKeyHash :& (VC (CvBool spendable)) :&
    (VC (CvBool delegetable)) :& (VC (CvMutez m)) :& VLam ops :& g :& r) =
  pure (VOp (OpCreateContract $
    CreateContract k (unwrapMbKeyHash mbKeyHash) spendable delegetable m g ops)
    :& (VC . CvAddress) (U.mkContractAddress @nop $
      createOrigOp k mbKeyHash spendable delegetable m ops g) :& r)
runInstrImpl (CREATE_CONTRACT2 ops)
  (VC (CvKeyHash k) :& VOption mbKeyHash :& (VC (CvBool spendable)) :&
    (VC (CvBool delegetable)) :& (VC (CvMutez m)) :& g :& r) =
  pure (VOp (OpCreateContract $
    CreateContract k (unwrapMbKeyHash mbKeyHash) spendable delegetable m g ops)
    :& (VC . CvAddress) (U.mkContractAddress @nop $
      createOrigOp k mbKeyHash spendable delegetable m ops g) :& r)
runInstrImpl IMPLICIT_ACCOUNT (VC (CvKeyHash k) :& r) =
  pure $ VContract (KeyAddress k) :& r
runInstrImpl NOW r = do
  ContractEnv{..} <- ask
  pure $ VC (CvTimestamp ceNow) :& r
runInstrImpl AMOUNT r = do
  ContractEnv{..} <- ask
  pure $ VC (CvMutez ceAmount) :& r
runInstrImpl BALANCE r = do
  ContractEnv{..} <- ask
  pure $ VC (CvMutez ceBalance) :& r
runInstrImpl CHECK_SIGNATURE (VKey k :& VSignature v :&
  VC (CvBytes b) :& r) = pure $ VC (CvBool $ checkSignature k v b) :& r
runInstrImpl SHA256 (VC (CvBytes b) :& r) = pure $ VC (CvBytes $ sha256 b) :& r
runInstrImpl SHA512 (VC (CvBytes b) :& r) = pure $ VC (CvBytes $ sha512 b) :& r
runInstrImpl BLAKE2B (VC (CvBytes b) :& r) = pure $ VC (CvBytes $ blake2b b) :& r
runInstrImpl HASH_KEY (VKey k :& r) = pure $ VC (CvKeyHash $ hashKey k) :& r
runInstrImpl STEPS_TO_QUOTA r = do
  (RemainingSteps x) <- get
  pure $ VC (CvNat $ (fromInteger . toInteger) x) :& r
runInstrImpl SOURCE r = do
  ContractEnv{..} <- ask
  pure $ VC (CvAddress ceSource) :& r
runInstrImpl SENDER r = do
  ContractEnv{..} <- ask
  pure $ VC (CvAddress ceSender) :& r
runInstrImpl ADDRESS (VContract a :& r) = pure $ VC (CvAddress a) :& r

-- | Evaluates an arithmetic operation and either fails or proceeds.
runArithOp
  :: ArithOp aop n m
  => proxy aop
  -> CVal n
  -> CVal m
  -> EvalOp nop (Val instr ('T_c (ArithRes aop n m)))
runArithOp op l r = case evalOp op l r of
  Left  err -> throwError (MichelsonArithError err)
  Right res -> pure (VC res)

createOrigOp
  :: (SingI param, SingI store)
  => KeyHash
  -> Maybe (Val Instr ('T_c 'U.T_key_hash))
  -> Bool -> Bool -> Mutez
  -> Contract param store
  -> Val Instr t
  -> U.OriginationOperation nop
createOrigOp k mbKeyHash delegetable spendable m contract g =
  U.OriginationOperation
    { ooManager = k
    , ooDelegate = (unwrapMbKeyHash mbKeyHash)
    , ooSpendable = spendable
    , ooDelegatable = delegetable
    , ooBalance = m
    , ooStorage = unsafeValToValue g
    , ooContract = convertContract contract
    }

unwrapMbKeyHash :: Maybe (Val Instr ('T_c 'U.T_key_hash)) -> Maybe KeyHash
unwrapMbKeyHash (Just (VC (CvKeyHash keyHash))) = Just keyHash
unwrapMbKeyHash Nothing = Nothing
