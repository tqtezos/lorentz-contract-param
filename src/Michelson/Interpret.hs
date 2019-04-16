{-# LANGUAGE DerivingStrategies, Rank2Types #-}

-- | Module, containing function to interpret Michelson
-- instructions against given context and input stack.
module Michelson.Interpret
  ( ContractEnv (..)
  , InterpreterState (..)
  , MichelsonFailed (..)
  , RemainingSteps (..)
  , SomeItStack (..)
  , EvalOp

  , interpret
  , ContractReturn

  , interpretUntyped
  , InterpretUntypedError (..)
  , InterpretUntypedResult (..)
  , runInstr
  , runInstrNoGas
  ) where

import Prelude hiding (EQ, GT, LT)

import Control.Monad.Except (throwError)
import Control.Lens (zoom)
import qualified Data.Aeson as Aeson
import Data.Constraint.Forall (Forall)
import qualified Data.Map as Map
import Data.Default (def)
import qualified Data.Set as Set
import Data.Singletons (SingI(..))
import Data.Typeable ((:~:)(..))
import Data.Vinyl (Rec(..), (<+>))
import Fmt (Buildable(build), Builder, genericF)
import Michelson.EqParam (eqParam1, eqParam2)

import Michelson.Interpret.Pack (packValue')
import Morley.Types (MorleyLogs (..), PrintComment (..), ExtInstr (..), TestAssert (..), StackRef (..))
import Michelson.TypeCheck
  (ExtC, SomeContract(..), SomeValue(..), TCError, TcOriginatedContracts,
  TCTypeError(..), compareTypes, eqType, runTypeCheckT, typeCheckContract, typeCheckValue)
import Michelson.Typed
  (CValue(..), Contract, ConversibleExt, CreateAccount(..), CreateContract(..), HasNoOp, Instr(..),
  OpPresence(..), Operation(..), SetDelegate(..), Sing(..), T(..), TransferTokens(..), Value'(..),
  extractNotes, fromUType, withSomeSingT)
import qualified Michelson.Typed as T
import Michelson.Typed.Arith
import Michelson.Typed.Convert (convertContract, untypeValue)
import Michelson.Typed.Polymorphic
import qualified Michelson.Untyped as U
import Util.Peano (Peano, LongerThan, Sing(SS, SZ))
import Tezos.Address (Address(..))
import Util.Lens (HasLens (..))
import Tezos.Core (Mutez, Timestamp(..))
import Tezos.Crypto (KeyHash, blake2b, checkSignature, hashKey, sha256, sha512)

-- | Environment for contract execution.
data ContractEnv = ContractEnv
  { ceNow :: !Timestamp
  -- ^ Timestamp of the block whose validation triggered this execution.
  , ceMaxSteps :: !RemainingSteps
  -- ^ Number of steps after which execution unconditionally terminates.
  , ceBalance :: !Mutez
  -- ^ Current amount of mutez of the current contract.
  , ceContracts :: TcOriginatedContracts
  -- ^ Mapping from existing contracts' addresses to their executable
  -- representation.
  , ceSelf :: !Address
  -- ^ Address of the interpreted contract.
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
  MichelsonFailedWith :: (Typeable t, SingI t) => T.Value t -> MichelsonFailed
  MichelsonArithError :: (Typeable n, Typeable m) => ArithError (CValue n) (CValue m) -> MichelsonFailed
  MichelsonGasExhaustion :: MichelsonFailed
  MichelsonFailedOther :: Text -> MichelsonFailed

deriving instance Show MichelsonFailed

instance Eq MichelsonFailed where
  MichelsonFailedWith v1 == MichelsonFailedWith v2 = v1 `eqParam1` v2
  MichelsonFailedWith _ == _ = False
  MichelsonArithError ae1 == MichelsonArithError ae2 = ae1 `eqParam2` ae2
  MichelsonArithError _ == _ = False
  MichelsonGasExhaustion == MichelsonGasExhaustion = True
  MichelsonGasExhaustion == _ = False
  MichelsonFailedOther t1 == MichelsonFailedOther t2 = t1 == t2
  MichelsonFailedOther _ == _ = False

instance Forall ConversibleExt => Buildable MichelsonFailed where
  build =
    \case
      MichelsonFailedWith (v :: T.Value t) ->
        "Reached FAILWITH instruction with " <> formatValue v
      MichelsonArithError v -> build v
      MichelsonGasExhaustion ->
        "Gas limit exceeded on contract execution"
      MichelsonFailedOther t -> build t
    where
      formatValue :: forall t . SingI t => Value' Instr t -> Builder
      formatValue v =
        case T.checkOpPresence (sing @t) of
          OpPresent -> "<value with operations>"
          OpAbsent -> build (untypeValue v)

data InterpretUntypedError s
  = RuntimeFailure (MichelsonFailed, s)
  | IllTypedContract TCError
  | IllTypedParam TCError
  | IllTypedStorage TCError
  | UnexpectedParamType TCTypeError
  | UnexpectedStorageType TCTypeError
  deriving (Generic)

deriving instance (Buildable U.ExpandedInstr, Show s) => Show (InterpretUntypedError s)

instance (Forall ConversibleExt, Buildable s) => Buildable (InterpretUntypedError s) where
  build = genericF

data InterpretUntypedResult s where
  InterpretUntypedResult
    :: ( Typeable st
       , SingI st
       , HasNoOp st
       )
    => { iurOps :: [ Operation Instr ]
       , iurNewStorage :: T.Value st
       , iurNewState   :: InterpreterState s
       }
    -> InterpretUntypedResult s

deriving instance Show s => Show (InterpretUntypedResult s)

-- | Interpret a contract without performing any side effects.
interpretUntyped
  :: (ExtC, Aeson.ToJSON U.ExpandedInstrExtU)
  => U.Contract
  -> U.Value
  -> U.Value
  -> ContractEnv
  -> Either (InterpretUntypedError MorleyLogs) (InterpretUntypedResult MorleyLogs)
interpretUntyped U.Contract{..} paramU initStU env = do
  (SomeContract (instr :: Contract cp st) _ _)
      <- first IllTypedContract $ typeCheckContract (ceContracts env)
            (U.Contract para stor code)
  withSomeSingT (fromUType para) $ \sgp ->
    withSomeSingT (fromUType stor) $ \sgs -> do
      ntp <- first (UnexpectedParamType . ExtractionTypeMismatch) $ extractNotes para sgp
      nts <- first (UnexpectedStorageType . ExtractionTypeMismatch) $ extractNotes stor sgs
      paramV :::: ((_ :: Sing cp1), _)
          <- first IllTypedParam $ runTypeCheckT para (ceContracts env) $
               typeCheckValue paramU (sgp, ntp)
      initStV :::: ((_ :: Sing st1), _)
          <- first IllTypedStorage $ runTypeCheckT para (ceContracts env) $
               typeCheckValue initStU (sgs, nts)
      Refl <- first UnexpectedStorageType $ eqType @st @st1
      Refl <- first UnexpectedParamType   $ eqType @cp @cp1
      bimap RuntimeFailure constructIUR $
        toRes $ interpret instr paramV initStV env
  where
    toRes (ei, s) = bimap (,isExtState s) (,s) ei

    constructIUR ::
      (Typeable st, SingI st, HasNoOp st) =>
      (([Operation Instr], Value' Instr st), InterpreterState MorleyLogs) ->
      InterpretUntypedResult MorleyLogs
    constructIUR ((ops, val), st) =
      InterpretUntypedResult
      { iurOps = ops
      , iurNewStorage = val
      , iurNewState = st
      }

type ContractReturn s st =
  (Either MichelsonFailed ([Operation Instr], T.Value st), InterpreterState s)

interpret
  :: (ExtC, Aeson.ToJSON U.ExpandedInstrExtU)
  => Contract cp st
  -> T.Value cp
  -> T.Value st
  -> ContractEnv
  -> ContractReturn MorleyLogs st
interpret instr param initSt env = first (fmap toRes) $
  runEvalOp
    (runInstr instr (T.VPair (param, initSt) :& RNil))
    env
    (InterpreterState def $ ceMaxSteps env)
  where
    toRes
      :: (Rec (T.Value' instr) '[ 'TPair ('TList 'TOperation) st ])
      -> ([Operation instr], T.Value' instr st)
    toRes (T.VPair (T.VList ops_, newSt) :& RNil) =
      (map (\(T.VOp op) -> op) ops_, newSt)

data SomeItStack where
  SomeItStack :: T.InstrExtT inp -> Rec T.Value inp -> SomeItStack

newtype RemainingSteps = RemainingSteps Word64
  deriving stock (Show)
  deriving newtype (Eq, Ord, Buildable, Num)

data InterpreterState s = InterpreterState
  { isExtState       :: s
  , isRemainingSteps :: RemainingSteps
  } deriving (Show)

type EvalOp s a =
  ExceptT MichelsonFailed
    (ReaderT ContractEnv
       (State (InterpreterState s))) a

runEvalOp ::
     EvalOp s a
  -> ContractEnv
  -> InterpreterState s
  -> (Either MichelsonFailed a, InterpreterState s)
runEvalOp act env initSt =
  flip runState initSt $ usingReaderT env $ runExceptT act

-- | Function to change amount of remaining steps stored in State monad
runInstr
  :: ( HasLens (InterpreterState MorleyLogs) (InterpreterState state)
     , ExtC, Aeson.ToJSON U.ExpandedInstrExtU
     )
  => Instr inp out
  -> Rec (T.Value) inp
  -> EvalOp state (Rec (T.Value) out)
runInstr i@(Seq _i1 _i2) r = runInstrImpl runInstr i r
runInstr i@Nop r = runInstrImpl runInstr i r
runInstr i@(Nested _) r = runInstrImpl runInstr i r
runInstr i r = do
  rs <- gets isRemainingSteps
  if rs == 0
  then throwError $ MichelsonGasExhaustion
  else do
    modify (\s -> s {isRemainingSteps = rs - 1})
    runInstrImpl runInstr i r

runInstrNoGas
  :: forall a b state .
  ( HasLens (InterpreterState MorleyLogs) (InterpreterState state)
  , ExtC, Aeson.ToJSON U.ExpandedInstrExtU
  )
  => T.Instr a b -> Rec T.Value a -> EvalOp state (Rec T.Value b)
runInstrNoGas = runInstrImpl runInstrNoGas

-- | Function to interpret Michelson instruction(s) against given stack.
runInstrImpl
    :: ( HasLens (InterpreterState MorleyLogs) (InterpreterState state)
       , ExtC, Aeson.ToJSON U.ExpandedInstrExtU
       )
    => (forall inp1 out1 .
           Instr inp1 out1
        -> Rec (T.Value) inp1
        -> EvalOp state (Rec T.Value out1)
    ) ->
       (forall inp out .
           Instr inp out
        -> Rec (T.Value) inp
        -> EvalOp state (Rec T.Value out)
      )
runInstrImpl runner (Seq i1 i2) r = runner i1 r >>= \r' -> runner i2 r'
runInstrImpl _ Nop r = pure $ r
runInstrImpl _ (Ext nop) r = zoom lensOf $ r <$ interpretExt (SomeItStack nop r)
runInstrImpl runner (Nested sq) r = runInstrImpl runner sq r
runInstrImpl _ DROP (_ :& r) = pure $ r
runInstrImpl _ DUP (a :& r) = pure $ a :& a :& r
runInstrImpl _ SWAP (a :& b :& r) = pure $ b :& a :& r
runInstrImpl _ (PUSH v) r = pure $ v :& r
runInstrImpl _ SOME (a :& r) = pure $ VOption (Just a) :& r
runInstrImpl _ NONE r = pure $ VOption Nothing :& r
runInstrImpl _ UNIT r = pure $ VUnit :& r
runInstrImpl runner (IF_NONE _bNone bJust) (VOption (Just a) :& r) = runner bJust (a :& r)
runInstrImpl runner (IF_NONE bNone _bJust) (VOption Nothing :& r) = runner bNone r
runInstrImpl _ PAIR (a :& b :& r) = pure $ VPair (a, b) :& r
runInstrImpl _ CAR (VPair (a, _b) :& r) = pure $ a :& r
runInstrImpl _ CDR (VPair (_a, b) :& r) = pure $ b :& r
runInstrImpl _ LEFT (a :& r) = pure $ (VOr $ Left a) :& r
runInstrImpl _ RIGHT (b :& r) = pure $ (VOr $ Right b) :& r
runInstrImpl runner (IF_LEFT bLeft _) (VOr (Left a) :& r) = runner bLeft (a :& r)
runInstrImpl runner (IF_LEFT _ bRight) (VOr (Right a) :& r) = runner bRight (a :& r)
-- More here
runInstrImpl _ NIL r = pure $ VList [] :& r
runInstrImpl _ CONS (a :& VList l :& r) = pure $ VList (a : l) :& r
runInstrImpl runner (IF_CONS _ bNil) (VList [] :& r) = runner bNil r
runInstrImpl runner (IF_CONS bCons _) (VList (lh : lr) :& r) = runner bCons (lh :& VList lr :& r)
runInstrImpl _ SIZE (a :& r) = pure $ VC (CvNat $ (fromInteger . toInteger) $ evalSize a) :& r
runInstrImpl _ EMPTY_SET r = pure $ VSet Set.empty :& r
runInstrImpl _ EMPTY_MAP r = pure $ VMap Map.empty :& r
runInstrImpl runner (MAP ops) (a :& r) =
  case ops of
    (code :: Instr (MapOpInp c ': s) (b ': s)) -> do
      newList <- mapM (\(val :: T.Value (MapOpInp c)) -> do
        res <- runner code (val :& r)
        case res of
          ((newVal :: T.Value b) :& _) -> pure newVal)
        $ mapOpToList @c a
      pure $ mapOpFromList a newList :& r
runInstrImpl runner (ITER ops) (a :& r) =
  case ops of
    (code :: Instr (IterOpEl c ': s) s) ->
      case iterOpDetachOne @c a of
        (Just x, xs) -> do
          res <- runner code (x :& r)
          runner (ITER code) (xs :& res)
        (Nothing, _) -> pure r
runInstrImpl _ MEM (VC a :& b :& r) = pure $ VC (CvBool (evalMem a b)) :& r
runInstrImpl _ GET (VC a :& b :& r) = pure $ VOption (evalGet a b) :& r
runInstrImpl _ UPDATE (VC a :& b :& c :& r) = pure $ evalUpd a b c :& r
runInstrImpl runner (IF bTrue _) (VC (CvBool True) :& r) = runner bTrue r
runInstrImpl runner (IF _ bFalse) (VC (CvBool False) :& r) = runner bFalse r
runInstrImpl _ (LOOP _) (VC (CvBool False) :& r) = pure $ r
runInstrImpl runner (LOOP ops) (VC (CvBool True) :& r) = do
  res <- runner ops r
  runner (LOOP ops) res
runInstrImpl _ (LOOP_LEFT _) (VOr (Right a) :&r) = pure $ a :& r
runInstrImpl runner (LOOP_LEFT ops) (VOr (Left a) :& r) = do
  res <- runner ops (a :& r)
  runner  (LOOP_LEFT ops) res
runInstrImpl _ (LAMBDA lam) r = pure $ lam :& r
runInstrImpl runner EXEC (a :& VLam lBody :& r) = do
  res <- runner lBody (a :& RNil)
  pure $ res <+> r
runInstrImpl runner (DIP i) (a :& r) = do
  res <- runner i r
  pure $ a :& res
runInstrImpl _ FAILWITH (a :& _) = throwError $ MichelsonFailedWith a
runInstrImpl _ CAST (a :& r) = pure $ a :& r
runInstrImpl _ RENAME (a :& r) = pure $ a :& r
runInstrImpl _ PACK (a :& r) = pure $ (VC $ CvBytes $ packValue' a) :& r
runInstrImpl _ UNPACK (VC (CvBytes _) :& _) = error "Unimplemented yet :/"
runInstrImpl _ CONCAT (a :& b :& r) = pure $ evalConcat a b :& r
runInstrImpl _ CONCAT' (VList a :& r) = pure $ evalConcat' a :& r
runInstrImpl _ SLICE (VC (CvNat o) :& VC (CvNat l) :& s :& r) =
  pure $ VOption (evalSlice o l s) :& r
runInstrImpl _ ISNAT (VC (CvInt i) :& r) =
  if i < 0
  then pure $ VOption Nothing :& r
  else pure $ VOption (Just $ VC (CvNat $ fromInteger i)) :& r
runInstrImpl _ ADD (VC l :& VC r :& rest) =
  (:& rest) <$> runArithOp (Proxy @Add) l r
runInstrImpl _ SUB (VC l :& VC r :& rest) = (:& rest) <$> runArithOp (Proxy @Sub) l r
runInstrImpl _ MUL (VC l :& VC r :& rest) = (:& rest) <$> runArithOp (Proxy @Mul) l r
runInstrImpl _ EDIV (VC l :& VC r :& rest) = pure $ evalEDivOp l r :& rest
runInstrImpl _ ABS (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Abs) a) :& rest
runInstrImpl _ NEG (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Neg) a) :& rest
runInstrImpl _ LSL (VC x :& VC s :& rest) = (:& rest) <$> runArithOp (Proxy @Lsl) x s
runInstrImpl _ LSR (VC x :& VC s :& rest) = (:& rest) <$> runArithOp (Proxy @Lsr) x s
runInstrImpl _ OR (VC l :& VC r :& rest) = (:& rest) <$> runArithOp (Proxy @Or) l r
runInstrImpl _ AND (VC l :& VC r :& rest) = (:& rest) <$> runArithOp (Proxy @And) l r
runInstrImpl _ XOR (VC l :& VC r :& rest) = (:& rest) <$> runArithOp (Proxy @Xor) l r
runInstrImpl _ NOT (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Not) a) :& rest
runInstrImpl _ COMPARE (VC l :& VC r :& rest) = (:& rest) <$> runArithOp (Proxy @Compare) l r
runInstrImpl _ EQ (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Eq') a) :& rest
runInstrImpl _ NEQ (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Neq) a) :& rest
runInstrImpl _ LT (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Lt) a) :& rest
runInstrImpl _ GT (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Gt) a) :& rest
runInstrImpl _ LE (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Le) a) :& rest
runInstrImpl _ GE (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Ge) a) :& rest
runInstrImpl _ INT (VC (CvNat n) :& r) = pure $ VC (CvInt $ toInteger n) :& r
runInstrImpl _ SELF r = do
  ContractEnv{..} <- ask
  pure $ VContract ceSelf :& r
runInstrImpl _ (CONTRACT (nt :: T.Notes p)) (VC (CvAddress addr) :& r) = do
  ContractEnv{..} <- ask
  case Map.lookup addr ceContracts of
    Just tc -> do
      pure $
        either (const $ VOption Nothing)
               (const $ VOption (Just $ VContract addr))
               (compareTypes (sing @p, nt) tc)
        :& r
    Nothing -> pure $ VOption Nothing :& r
runInstrImpl _ TRANSFER_TOKENS (p :& VC (CvMutez mutez) :& contract :& r) =
  pure $ VOp (OpTransferTokens $ TransferTokens p mutez contract) :& r
runInstrImpl _ SET_DELEGATE (VOption mbKeyHash :& r) =
  case mbKeyHash of
    Just (VC (CvKeyHash k)) -> pure $ VOp (OpSetDelegate $ SetDelegate $ Just k) :& r
    Nothing -> pure $ VOp (OpSetDelegate $ SetDelegate $ Nothing) :& r
runInstrImpl _ CREATE_ACCOUNT
  (VC (CvKeyHash k) :& VOption mbKeyHash :&
    (VC (CvBool spendable)) :& (VC (CvMutez m)) :& r) =
  pure (VOp (OpCreateAccount $ CreateAccount k (unwrapMbKeyHash mbKeyHash) spendable m)
    :& (VC . CvAddress) (KeyAddress k) :& r)
runInstrImpl _ (CREATE_CONTRACT ops)
  (VC (CvKeyHash k) :& VOption mbKeyHash :& (VC (CvBool spendable)) :&
    (VC (CvBool delegetable)) :& (VC (CvMutez m)) :& g :& r) =
  pure (VOp (OpCreateContract $
    CreateContract k (unwrapMbKeyHash mbKeyHash) spendable delegetable m g ops)
    :& (VC . CvAddress) (U.mkContractAddress $
      createOrigOp k mbKeyHash spendable delegetable m ops g) :& r)
runInstrImpl _ IMPLICIT_ACCOUNT (VC (CvKeyHash k) :& r) =
  pure $ VContract (KeyAddress k) :& r
runInstrImpl _ NOW r = do
  ContractEnv{..} <- ask
  pure $ VC (CvTimestamp ceNow) :& r
runInstrImpl _ AMOUNT r = do
  ContractEnv{..} <- ask
  pure $ VC (CvMutez ceAmount) :& r
runInstrImpl _ BALANCE r = do
  ContractEnv{..} <- ask
  pure $ VC (CvMutez ceBalance) :& r
runInstrImpl _ CHECK_SIGNATURE (VKey k :& VSignature v :&
  VC (CvBytes b) :& r) = pure $ VC (CvBool $ checkSignature k v b) :& r
runInstrImpl _ SHA256 (VC (CvBytes b) :& r) = pure $ VC (CvBytes $ sha256 b) :& r
runInstrImpl _ SHA512 (VC (CvBytes b) :& r) = pure $ VC (CvBytes $ sha512 b) :& r
runInstrImpl _ BLAKE2B (VC (CvBytes b) :& r) = pure $ VC (CvBytes $ blake2b b) :& r
runInstrImpl _ HASH_KEY (VKey k :& r) = pure $ VC (CvKeyHash $ hashKey k) :& r
runInstrImpl _ STEPS_TO_QUOTA r = do
  RemainingSteps x <- gets isRemainingSteps
  pure $ VC (CvNat $ (fromInteger . toInteger) x) :& r
runInstrImpl _ SOURCE r = do
  ContractEnv{..} <- ask
  pure $ VC (CvAddress ceSource) :& r
runInstrImpl _ SENDER r = do
  ContractEnv{..} <- ask
  pure $ VC (CvAddress ceSender) :& r
runInstrImpl _ ADDRESS (VContract a :& r) = pure $ VC (CvAddress a) :& r

-- | Evaluates an arithmetic operation and either fails or proceeds.
runArithOp
  :: (ArithOp aop n m, Typeable n, Typeable m)
  => proxy aop
  -> CValue n
  -> CValue m
  -> EvalOp s (T.Value' instr ('Tc (ArithRes aop n m)))
runArithOp op l r = case evalOp op l r of
  Left  err -> throwError (MichelsonArithError err)
  Right res -> pure (T.VC res)

createOrigOp
  :: (SingI param, SingI store, HasNoOp store, Forall ConversibleExt)
  => KeyHash
  -> Maybe (T.Value ('Tc 'U.CKeyHash))
  -> Bool -> Bool -> Mutez
  -> Contract param store
  -> Value' Instr store
  -> U.OriginationOperation
createOrigOp k mbKeyHash delegetable spendable m contract g =
  U.OriginationOperation
    { ooManager = k
    , ooDelegate = (unwrapMbKeyHash mbKeyHash)
    , ooSpendable = spendable
    , ooDelegatable = delegetable
    , ooBalance = m
    , ooStorage = untypeValue g
    , ooContract = convertContract contract
    }

unwrapMbKeyHash :: Maybe (T.Value ('Tc 'U.CKeyHash)) -> Maybe KeyHash
unwrapMbKeyHash (Just (T.VC (CvKeyHash keyHash))) = Just keyHash
unwrapMbKeyHash Nothing = Nothing

interpretExt :: SomeItStack -> EvalOp MorleyLogs ()
interpretExt (SomeItStack (PRINT (PrintComment pc)) st) = do
  let getEl (Left l) = l
      getEl (Right str) = withStackElem str st show
  modify (\s -> s {isExtState = MorleyLogs $ mconcat (map getEl pc) : unMorleyLogs (isExtState s)})

interpretExt (SomeItStack (TEST_ASSERT (TestAssert nm pc (instr :: T.Instr inp1 ('T.Tc 'T.CBool ': out1) )))
            (st :: Rec T.Value inp2)) = do
  runInstrNoGas instr st >>= \case
    (T.VC (T.CvBool False) :& RNil) -> do
      interpretExt (SomeItStack (PRINT pc) st)
      throwError $ MichelsonFailedOther $ "TEST_ASSERT " <> nm <> " failed"
    _  -> pass

-- | Access given stack reference (in CPS style).
withStackElem
  :: forall st a.
     StackRef st
  -> Rec T.Value st
  -> (forall t. T.Value t -> a)
  -> a
withStackElem (StackRef sn) vals cont =
  loop (vals, sn)
  where
    loop
      :: forall s (n :: Peano). (LongerThan s n)
      => (Rec T.Value s, Sing n) -> a
    loop = \case
      (e :& _, SZ) -> cont e
      (_ :& es, SS n) -> loop (es, n)
