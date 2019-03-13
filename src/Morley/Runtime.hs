-- | Interpreter of a contract in Morley language.

module Morley.Runtime
       ( originateContract
       , runContract

       -- * Re-exports
       , Account (..)
       , TxData (..)

       -- * For testing
       , InterpreterOp (..)
       , InterpreterRes (..)
       , InterpreterError (..)
       , interpreterPure
       ) where

import Control.Lens (at, makeLenses, (%=), (.=), (<>=))
import Control.Monad.Except (Except, runExcept, throwError)
import Data.Default (def)
import Fmt (Buildable(build), blockListF, pretty, (+|), (|+))

import Michelson.Interpret (ContractEnv(..), InterpretUntypedError(..), InterpretUntypedResult(..))
import Michelson.Typed (Operation, unsafeValToValue)
import Michelson.Untyped (Contract(..), Op, OriginationOperation(..), Value, mkContractAddress)
import Morley.Ext (interpretMorleyUntyped)
import Morley.Runtime.GState
import Morley.Runtime.TxData
import Morley.Types (MorleyLogs(..))
import Tezos.Address (Address(..))
import Tezos.Core (Timestamp(..), getCurrentTime, unsafeMkMutez)
import Tezos.Crypto (parseKeyHash)

----------------------------------------------------------------------------
-- Auxiliary types
----------------------------------------------------------------------------

-- | Operations executed by interpreter.
-- In our model one network operation (`operation` type in Michelson)
-- corresponds to a list (possibly empty) of interpreter operations.
--
-- Note: 'Address' is not part of 'TxData', because 'TxData' is
-- supposed to be provided by the user, while 'Address' can be
-- computed by our code.
data InterpreterOp
  = OriginateOp !OriginationOperation
  -- ^ Originate a contract.
  | TransferOp Address
               TxData
  -- ^ Send a transaction to given address which is assumed to be the
  -- address of an originated contract.
  deriving (Show)

-- | Result of a single execution of interpreter.
data InterpreterRes = InterpreterRes
  { _irGState :: !GState
  -- ^ New 'GState'.
  , _irOperations :: [InterpreterOp]
  -- ^ List of operations to be added to the operations queue.
  , _irUpdates :: ![GStateUpdate]
  -- ^ Updates applied to 'GState'.
  , _irPrintedLogs :: [MorleyLogs]
  -- ^ During execution a contract can print logs,
  -- all logs are kept until all called contracts are executed
  , _irSourceAddress :: !(Maybe Address)
  -- ^ As soon as transfer operation is encountered, this address is
  -- set to its input.
  } deriving (Show)

makeLenses ''InterpreterRes

-- | Errors that can happen during contract interpreting.
data InterpreterError
  = IEUnknownContract !Address
  -- ^ The interpreted contract hasn't been originated.
  | IEInterpreterFailed !(Contract Op)
                        !(InterpretUntypedError MorleyLogs)
  -- ^ Interpretation of Michelson contract failed.
  | IEAlreadyOriginated !Address
                        !Account
  -- ^ A contract is already originated
  deriving (Show)

instance Buildable InterpreterError where
  build =
    \case
      IEUnknownContract addr -> "The contract is not originated " +| addr |+ ""
      IEInterpreterFailed _ err -> "Michelson interpreter failed: " +| err |+ ""
      IEAlreadyOriginated addr acc ->
        "The following account is already originated: " +| addr |+
        ", " +| acc |+ ""

instance Exception InterpreterError where
  displayException = pretty

----------------------------------------------------------------------------
-- Interface
----------------------------------------------------------------------------

-- | Originate a contract. Returns the address of the originated
-- contract.
originateContract :: Bool -> FilePath -> OriginationOperation -> IO Address
originateContract verbose dbPath origination =
  mkContractAddress origination <$
  interpreter Nothing 100500 verbose dbPath (OriginateOp origination)

-- | Run a contract. The contract is originated first (if it's not
-- already) and then we pretend that we send a transaction to it.
runContract
    :: Maybe Timestamp
    -> Word64
    -> Bool
    -> FilePath
    -> Value Op
    -> Contract Op
    -> TxData
    -> IO ()
runContract maybeNow maxSteps verbose dbPath storageValue contract txData = do
  addr <- originateContract verbose dbPath origination
    `catch` ignoreAlreadyOriginated
  interpreter maybeNow maxSteps verbose dbPath (TransferOp addr txData)
  where
    defaultManager =
      either (error "defaultManager: failed to parse") id $
      parseKeyHash "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"
    defaultBalance = unsafeMkMutez 4000000000
    origination = OriginationOperation
      { ooManager = defaultManager
      , ooDelegate = Nothing
      , ooSpendable = False
      , ooDelegatable = False
      , ooBalance = defaultBalance
      , ooStorage = storageValue
      , ooContract = contract
      }
    ignoreAlreadyOriginated :: InterpreterError -> IO Address
    ignoreAlreadyOriginated =
      \case IEAlreadyOriginated addr _ -> pure addr
            err -> throwM err

----------------------------------------------------------------------------
-- Interpreter
----------------------------------------------------------------------------

-- | Interpret a contract on some global state (read from file) and
-- transaction data (passed explicitly).
interpreter :: Maybe Timestamp -> Word64 -> Bool -> FilePath -> InterpreterOp -> IO ()
interpreter maybeNow maxSteps verbose dbPath operation = do
  now <- maybe getCurrentTime pure maybeNow
  gState <- readGState dbPath
  let eitherRes = interpreterPure now maxSteps gState [operation]
  InterpreterRes {..} <- either throwM pure eitherRes
  when (verbose && not (null _irUpdates)) $
    putTextLn $ "Updates: " +| blockListF _irUpdates
  forM_ _irPrintedLogs $ \(MorleyLogs logs) -> do
    mapM_ putTextLn logs
    putTextLn "" -- extra break line to separate logs from two sequence contracts
  writeGState dbPath _irGState

-- | Implementation of interpreter outside 'IO'.  It reads operations,
-- interprets them one by one and updates state accordingly.
interpreterPure ::
  Timestamp -> Word64 -> GState -> [InterpreterOp] -> Either InterpreterError InterpreterRes
interpreterPure now maxSteps gState ops =
    runExcept (execStateT (statefulInterpreter now maxSteps) initialState)
  where
    initialState = InterpreterRes
      { _irGState = gState
      , _irOperations = ops
      , _irUpdates = mempty
      , _irPrintedLogs = def
      , _irSourceAddress = Nothing
      }

-- TODO: do we want to update anything in case of error?
statefulInterpreter
  :: Timestamp
  -> Word64
  -> StateT InterpreterRes (Except InterpreterError) ()
statefulInterpreter now maxSteps = do
  curGState <- use irGState
  mSourceAddr <- use irSourceAddress
  use irOperations >>= \case
    [] -> pass
    (op:opsTail) ->
      -- TODO: is it correct to pass latest GState?
      either throwError (processIntRes opsTail) $
      interpretOneOp now maxSteps mSourceAddr curGState op
  where
    processIntRes opsTail InterpreterRes {..} = do
      irGState .= _irGState
      irOperations .= opsTail <> _irOperations
      irUpdates <>= _irUpdates
      irPrintedLogs <>= _irPrintedLogs
      irSourceAddress %= (<|> _irSourceAddress)
      statefulInterpreter now maxSteps

-- | Run only one interpreter operation and update 'GState' accordingly.
interpretOneOp
  :: Timestamp
  -> Word64
  -> Maybe Address
  -> GState
  -> InterpreterOp
  -> Either InterpreterError InterpreterRes
interpretOneOp _ _ _ gs (OriginateOp origination) =
  case applyUpdate upd gs of
    Left _ -> Left (IEAlreadyOriginated address account)
    Right newGS -> Right $
      InterpreterRes
      { _irGState = newGS
      , _irOperations = mempty
      , _irUpdates = [upd]
      , _irPrintedLogs = def
      , _irSourceAddress = Nothing
      }
  where
    account = Account
      { accBalance = ooBalance origination
      , accStorage = ooStorage origination
      , accContract = ooContract origination
      }
    address = mkContractAddress origination
    upd = GSAddAccount address account
interpretOneOp now maxSteps mSourceAddr gs (TransferOp addr txData) = do
    acc <- maybe (Left (IEUnknownContract addr)) Right (accounts ^. at addr)
    let
      sourceAddr = fromMaybe (tdSenderAddress txData) mSourceAddr
      contract = accContract acc
      contractEnv = ContractEnv
        { ceNow = now
        , ceMaxSteps = maxSteps
        , ceBalance = accBalance acc
        , ceContracts = accContract <$> accounts
        , ceSource = sourceAddr
        , ceSender = tdSenderAddress txData
        , ceAmount = tdAmount txData
        }
    InterpretUntypedResult networkOps newValue newState
      <- first (IEInterpreterFailed contract) $
            interpretMorleyUntyped contract (tdParameter txData)
                             (accStorage acc) contractEnv
    let
      newValueU = unsafeValToValue newValue
      upd = GSSetStorageValue addr newValueU
      _irGState = setStorageValue addr newValueU gs
      _irOperations = foldMap convertOp networkOps
      _irUpdates = [upd]
      _irPrintedLogs = [newState]
      _irSourceAddress = Just sourceAddr
    pure InterpreterRes {..}
  where
    accounts = gsAccounts gs

----------------------------------------------------------------------------
-- Simple helpers
----------------------------------------------------------------------------

convertOp :: Operation instr -> [InterpreterOp]
convertOp = const []
