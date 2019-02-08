-- | Interpreter of a contract in Morley language.

module Morley.Runtime
       ( interpreter

       -- * Re-exports
       , TxData (..)
       ) where

import Control.Lens (at, makeLenses, (.=), (<>=))
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
data InterpreterOp = InterpreterOp (Contract Op) TxData

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
  }

makeLenses ''InterpreterRes

-- TODO: pretty printing
-- | Errors that can happen during contract interpreting.
data InterpreterError
  = IEUnknownContract (Contract Op)
  -- ^ The interpreted contract hasn't been originated.
  | IEMichelsonFailed (Contract Op) MichelsonFailed
  -- ^ Michelson contract failed (using Michelson's FAILWITH instruction).
  deriving (Show)

instance Exception InterpreterError

----------------------------------------------------------------------------
-- Interpreter
----------------------------------------------------------------------------

-- | Interpret a contract on some global state (read from file) and
-- transaction data (passed explicitly).
interpreter :: Maybe Timestamp -> Bool -> FilePath -> Contract Op -> TxData -> IO ()
interpreter maybeNow verbose dbPath contract txData = do
  now <- maybe getCurrentTime pure maybeNow
  let sourceAddr = tdSenderAddress txData
  gState <- readGState dbPath
  let initialState =
        InterpreterRes
          { _irGState = gState
          , _irOperations = [InterpreterOp contract txData]
          , _irUpdatedValues = mempty
          }
      eitherRes =
        runExcept (execStateT (statefulInterpreter now sourceAddr) initialState)
  InterpreterRes {..} <- either throwM pure eitherRes
  -- TODO: pretty print
  when verbose $ putTextLn $ "Updates: " <> show _irUpdatedValues
  writeGState dbPath _irGState

-- TODO: do we want to update anything in case of error?
statefulInterpreter ::
     Timestamp -> Address -> StateT InterpreterRes (Except InterpreterError) ()
statefulInterpreter now sourceAddr = do
  curGState <- use irGState
  use irOperations >>= \case
    [] -> pass
    (op:opsTail) ->
      -- TODO: is it correct to pass latest GState?
      either throwError (processIntRes opsTail) $
      interpretOneOp now sourceAddr curGState op
  where
    processIntRes opsTail InterpreterRes {..} = do
      irGState .= _irGState
      irOperations .= opsTail <> _irOperations
      irUpdatedValues <>= _irUpdatedValues
      statefulInterpreter now sourceAddr

-- | Run only one interpreter operation and update 'GState' accordingly.
interpretOneOp
  :: Timestamp
  -> Address
  -> GState
  -> InterpreterOp
  -> Either InterpreterError InterpreterRes
interpretOneOp now sourceAddr gs (InterpreterOp contract txData) = do
    acc <- maybe (Left (IEUnknownContract contract)) Right (accounts ^. at ourAddr)
    let
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
      _irGState = setStorageValue ourAddr newValue gs
      _irOperations = foldMap convertOp networkOps
      _irUpdatedValues = [(ourAddr, newValue)]
    pure InterpreterRes {..}
  where
    accounts = gsAccounts gs
    ourAddr :: Address
    ourAddr = contractAddress contract

----------------------------------------------------------------------------
-- Simple helpers
----------------------------------------------------------------------------

convertOp :: NetworkOp -> [InterpreterOp]
convertOp = const []

-- Return current time as 'Timestamp'.
-- TODO [TM-18] implement!
getCurrentTime :: IO Timestamp
getCurrentTime = pure (Timestamp 100500)
