-- | Interpreter of a contract in Morley language.

module Morley.Runtime
       ( originateContract
       , runContract
       , contractAddress

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

import Michelson.Interpret (ContractEnv(..), InterpretUntypedError(..), InterpretUntypedResult(..))
import Michelson.Typed (Operation, unsafeValToValue)
import Michelson.Untyped (Contract(..), Op, Value)
import Morley.Nop (interpretMorleyUntyped)
import Morley.Runtime.GState
import Morley.Runtime.TxData
import Morley.Types (NopInstr)
import Tezos.Address (Address(..))
import Tezos.Core (Timestamp(..), getCurrentTime, unsafeMkMutez)

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
    = OriginateOp Account
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
  , _irUpdatedValues :: [(Address, Value (Op NopInstr))]
  -- ^ Addresses of all contracts whose storage value was updated and
  -- corresponding new values themselves.
  -- We log these values.
  , _irSourceAddress :: !(Maybe Address)
  -- ^ As soon as transfer operation is encountered, this address is
  -- set to its input.
  } deriving (Show)

makeLenses ''InterpreterRes

-- TODO: pretty printing
-- | Errors that can happen during contract interpreting.
data InterpreterError
  = IEUnknownContract Address
  -- ^ The interpreted contract hasn't been originated.
  | IEInterpreterFailed (Contract (Op NopInstr)) (InterpretUntypedError NopInstr)
  -- ^ Interpretation of Michelson contract failed.
  | IEAlreadyOriginated Account
  -- ^ A contract is already originated
  deriving (Show)

instance Exception InterpreterError

----------------------------------------------------------------------------
-- Interface
----------------------------------------------------------------------------

-- TODO [TM-17] I guess it's possible to compute address of a contract, but I
-- don't know how do it (yet). Maybe it requires more data. In the
-- worst case we can store such map in GState. Maybe we'll have to
-- move this function to Morley.
contractAddress :: Contract (Op nop) -> Address
contractAddress _ = ContractAddress "dummy-address"

-- | Originate a contract. Returns the address of the originated
-- contract.
originateContract :: Bool -> FilePath -> Account -> IO Address
originateContract verbose dbPath account =
  contractAddress (accContract account) <$
  interpreter Nothing 100500 verbose dbPath (OriginateOp account)

-- | Run a contract. The contract is originated first (if it's not
-- already) and then we pretend that we send a transaction to it.
runContract
    :: Maybe Timestamp
    -> Word64
    -> Bool
    -> FilePath
    -> Value (Op NopInstr)
    -> Contract (Op NopInstr)
    -> TxData
    -> IO ()
runContract maybeNow maxSteps verbose dbPath storageValue contract txData = do
  addr <- originateContract verbose dbPath acc
    `catch` ignoreAlreadyOriginated
  interpreter maybeNow maxSteps verbose dbPath (TransferOp addr txData)
  where
    defaultBalance = unsafeMkMutez 4000000000
    acc = Account
      { accBalance = defaultBalance
      , accStorage = storageValue
      , accContract = contract
      }
    ignoreAlreadyOriginated :: InterpreterError -> IO Address
    ignoreAlreadyOriginated =
      \case IEAlreadyOriginated _ -> pure (contractAddress contract)
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
  let
    eitherRes = interpreterPure now maxSteps gState [operation]
  InterpreterRes {..} <- either throwM pure eitherRes
  -- TODO: pretty print
  when (verbose && not (null _irUpdatedValues)) $
    putTextLn $ "Updates: " <> show _irUpdatedValues
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
      , _irUpdatedValues = mempty
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
      irUpdatedValues <>= _irUpdatedValues
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
interpretOneOp _ _ _ gs (OriginateOp account) =
  case addAccount (contractAddress contract) account gs of
    Nothing -> Left (IEAlreadyOriginated account)
    Just newGS -> Right $
      InterpreterRes
      { _irGState = newGS
      , _irOperations = mempty
      , _irUpdatedValues = mempty
      , _irSourceAddress = Nothing
      }
  where
    contract = accContract account
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
    InterpretUntypedResult networkOps newValue
      <- first (IEInterpreterFailed contract) $
            interpretMorleyUntyped contract (tdParameter txData)
                             (accStorage acc) contractEnv
    let
      newValueU = unsafeValToValue newValue
      _irGState = setStorageValue addr newValueU gs
      _irOperations = foldMap convertOp networkOps
      _irUpdatedValues = [(addr, newValueU)]
      _irSourceAddress = Just sourceAddr
    pure InterpreterRes {..}
  where
    accounts = gsAccounts gs

----------------------------------------------------------------------------
-- Simple helpers
----------------------------------------------------------------------------

convertOp :: Operation instr -> [InterpreterOp]
convertOp = const []
