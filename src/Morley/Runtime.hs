-- | Interpreter of a contract in Morley language.

module Morley.Runtime
       ( originateContract
       , runContract

       -- * Re-exports
       , Account (..)
       , TxData (..)
       ) where

import Control.Lens (at, makeLenses, (%=), (.=), (<>=))
import Control.Monad.Except (Except, runExcept, throwError)

import Michelson.Interpret (ContractEnv(..), MichelsonFailed, michelsonInterpreter)
import Michelson.Types
import Morley.Runtime.GState
import Morley.Runtime.TxData

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

-- | Result of a single execution of interpreter.
data InterpreterRes = InterpreterRes
  { _irGState :: !GState
  -- ^ New 'GState'.
  , _irOperations :: [InterpreterOp]
  -- ^ List of operations to be added to the operations queue.
  , _irUpdatedValues :: [(Address, Value Op)]
  -- ^ Addresses of all contracts whose storage value was updated and
  -- corresponding new values themselves.
  -- We log these values.
  , _irSourceAddress :: !(Maybe Address)
  -- ^ As soon as transfer operation is encountered, this address is
  -- set to its input.
  }

makeLenses ''InterpreterRes

-- TODO: pretty printing
-- | Errors that can happen during contract interpreting.
data InterpreterError
  = IEUnknownContract Address
  -- ^ The interpreted contract hasn't been originated.
  | IEMichelsonFailed (Contract Op) MichelsonFailed
  -- ^ Michelson contract failed (using Michelson's FAILWITH instruction).
  | IEAlreadyOriginated Account
  -- ^ A contract is already originated.
  deriving (Show)

instance Exception InterpreterError

----------------------------------------------------------------------------
-- Interface
----------------------------------------------------------------------------

-- | Originate a contract. Returns the address of the originated
-- contract.
originateContract :: Bool -> FilePath -> Account -> IO Address
originateContract verbose dbPath account =
  contractAddress (accContract account) <$
  interpreter Nothing verbose dbPath (OriginateOp account)

-- | Run a contract. The contract is originated first (if it's not
-- already) and then we pretend that we send a transaction to it.
runContract
    :: Maybe Timestamp
    -> Bool
    -> FilePath
    -> Value Op
    -> Contract Op
    -> TxData
    -> IO ()
runContract maybeNow verbose dbPath storageValue contract txData = do
  addr <- originateContract verbose dbPath acc
    `catch` ignoreAlreadyOriginated
  interpreter maybeNow verbose dbPath (TransferOp addr txData)
  where
    defaultBalance = Mutez 4000000000
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
interpreter :: Maybe Timestamp -> Bool -> FilePath -> InterpreterOp -> IO ()
interpreter maybeNow verbose dbPath operation = do
  now <- maybe getCurrentTime pure maybeNow
  gState <- readGState dbPath
  let initialState =
        InterpreterRes
          { _irGState = gState
          , _irOperations = [operation]
          , _irUpdatedValues = mempty
          , _irSourceAddress = Nothing
          }
      eitherRes =
        runExcept (execStateT (statefulInterpreter now) initialState)
  InterpreterRes {..} <- either throwM pure eitherRes
  -- TODO: pretty print
  when (verbose && not (null _irUpdatedValues)) $
    putTextLn $ "Updates: " <> show _irUpdatedValues
  writeGState dbPath _irGState

-- TODO: do we want to update anything in case of error?
statefulInterpreter ::
     Timestamp -> StateT InterpreterRes (Except InterpreterError) ()
statefulInterpreter now = do
  curGState <- use irGState
  mSourceAddr <- use irSourceAddress
  use irOperations >>= \case
    [] -> pass
    (op:opsTail) ->
      -- TODO: is it correct to pass latest GState?
      either throwError (processIntRes opsTail) $
      interpretOneOp now mSourceAddr curGState op
  where
    processIntRes opsTail InterpreterRes {..} = do
      irGState .= _irGState
      irOperations .= opsTail <> _irOperations
      irUpdatedValues <>= _irUpdatedValues
      irSourceAddress %= (<|> _irSourceAddress)
      statefulInterpreter now

-- | Run only one interpreter operation and update 'GState' accordingly.
interpretOneOp
  :: Timestamp
  -> Maybe Address
  -> GState
  -> InterpreterOp
  -> Either InterpreterError InterpreterRes
interpretOneOp _ _ gs (OriginateOp account) =
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
interpretOneOp now mSourceAddr gs (TransferOp addr txData) = do
    acc <- maybe (Left (IEUnknownContract addr)) Right (accounts ^. at addr)
    let
      sourceAddr = fromMaybe (tdSenderAddress txData) mSourceAddr
      contract = accContract acc
      contractEnv = ContractEnv
        { ceNow = now
        , ceMaxSteps = 100500  -- TODO [TM-18]
        , ceBalance = accBalance acc
        , ceStorage = accStorage acc
        , ceContracts = accContract <$> accounts
        , ceParameter = tdParameter txData
        , ceSource = sourceAddr
        , ceSender = tdSenderAddress txData
        , ceAmount = tdAmount txData
        }
    (networkOps, newValue) <- first (IEMichelsonFailed contract) $
      michelsonInterpreter contractEnv contract
    let
      _irGState = setStorageValue addr newValue gs
      _irOperations = foldMap convertOp networkOps
      _irUpdatedValues = [(addr, newValue)]
      _irSourceAddress = Just sourceAddr
    pure InterpreterRes {..}
  where
    accounts = gsAccounts gs

----------------------------------------------------------------------------
-- Simple helpers
----------------------------------------------------------------------------

convertOp :: NetworkOp -> [InterpreterOp]
convertOp = const []

-- Return current time as 'Timestamp'.
-- TODO [TM-18] implement!
getCurrentTime :: IO Timestamp
getCurrentTime = pure (Timestamp 100500)
