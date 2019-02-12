-- | Global blockchain state (emulated).

module Morley.Runtime.GState
       ( Account (..)
       , GState (..)
       , initGState
       , readGState
       , writeGState

       -- * Operations on GState
       , addAccount
       , setStorageValue
       ) where

import Control.Lens (at)
import qualified Data.Aeson as Aeson
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import System.IO.Error (IOError, isDoesNotExistError)

import Michelson.Types

data Account = Account
  { accBalance :: !Mutez
  -- ^ Amount of mutez owned by this account.
  , accStorage :: !(Value Op)
  -- ^ Storage value associated with this account.
  , accContract :: !(Contract Op)
  -- ^ Contract of this account.
  } deriving (Show)

deriveJSON defaultOptions ''Account

-- | Persistent data passed to Morley contracts which can be updated
-- as result of contract execution.
data GState = GState
  { gsAccounts :: Map Address Account
  -- ^ All known accounts and their state.
  }

deriveJSON defaultOptions ''GState

-- | Initial 'GState'. It's supposed to be used if no 'GState' is
-- provided. For now it's empty, but we can hardcode some dummy data
-- in the future.
initGState :: GState
initGState =
  GState {gsAccounts = one (contractAddress dummyContract, acc)}
  where
    -- doesn't matter
    dummyContract = Contract
      { para = Type T_unit noAnn
      , stor = Type T_unit noAnn
      , code = []
      }
    acc = Account
      { accBalance = Mutez 1
      , accStorage = ValueUnit
      , accContract = dummyContract
      }

data GStateParseError =
  GStateParseError String
  deriving Show

instance Exception GStateParseError where
  displayException (GStateParseError str) = "Failed to parse GState: " <> str

-- | Read 'GState' from a file.
readGState :: FilePath -> IO GState
readGState fp = (LBS.readFile fp >>= parseFile) `catch` onExc
  where
    parseFile :: LByteString -> IO GState
    parseFile = either (throwM . GStateParseError) pure . Aeson.eitherDecode'
    onExc :: IOError -> IO GState
    onExc exc
      | isDoesNotExistError exc = pure initGState
      | otherwise = throwM exc

-- | Write 'GState' to a file.
writeGState :: FilePath -> GState -> IO ()
writeGState fp gs = LBS.writeFile fp (Aeson.encode gs)

-- | Add an account if it hasn't been added before.
addAccount :: Address -> Account -> GState -> Maybe GState
addAccount addr acc gs
    | addr `Map.member` accounts = Nothing
    | otherwise = Just (gs {gsAccounts = accounts & at addr .~ Just acc})
  where
    accounts = gsAccounts gs

-- | Updare storage value associated with given address. Does nothing
-- if the address is unknown.
setStorageValue :: Address -> Value Op -> GState -> GState
setStorageValue addr newValue gs =
  gs {gsAccounts = gsAccounts gs & at addr %~ fmap setAccountValue}
  where
    setAccountValue account = account {accStorage = newValue}
