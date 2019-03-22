-- | Global blockchain state (emulated).

module Morley.Runtime.GState
       (
         -- * Auxiliary types
         ContractState (..)
       , AddressState (..)
       , asBalance

       -- * GState
       , GState (..)
       , genesisAddress
       , genesisAddressText
       , genesisKeyHash
       , initGState
       , readGState
       , writeGState

       -- * Operations on GState
       , GStateUpdate (..)
       , GStateUpdateError (..)
       , applyUpdate
       , applyUpdates
       , extractAllContracts
       ) where

import Control.Lens (at)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Fmt (genericF, pretty, (+|), (|+))
import Formatting.Buildable (Buildable(build))
import System.IO.Error (IOError, isDoesNotExistError)

import Michelson.Untyped (Contract, Value)
import Morley.Types ()
import Tezos.Address (Address(..))
import Tezos.Core (Mutez)
import Tezos.Crypto (KeyHash, parseKeyHash)
import Michelson.Untyped (Type, para)
import Michelson.TypeCheck (TcOriginatedContracts)

-- | State of a contract with code.
data ContractState = ContractState
  { csBalance :: !Mutez
  -- ^ Amount of mutez owned by this contract.
  , csStorage :: !Value
  -- ^ Storage value associated with this contract.
  , csContract :: !Contract
  -- ^ Contract itself (untyped).
  } deriving (Show, Generic, Eq)

instance Buildable ContractState where
  build = genericF

deriveJSON defaultOptions ''ContractState

-- | State of an arbitrary address.
data AddressState
  = ASSimple !Mutez
  -- ^ For contracts without code we store only its balance.
  | ASContract !ContractState
  -- ^ For contracts with code we store more state represented by
  -- 'ContractState'.
  deriving (Show, Generic, Eq)

instance Buildable AddressState where
  build =
    \case
      ASSimple balance -> "Balance = " +| balance |+ ""
      ASContract cs -> build cs

deriveJSON defaultOptions ''AddressState

-- | Extract balance from 'AddressState'.
asBalance :: AddressState -> Mutez
asBalance =
  \case
    ASSimple b -> b
    ASContract cs -> csBalance cs

-- | Persistent data passed to Morley contracts which can be updated
-- as result of contract execution.
data GState = GState
  { gsAddresses :: Map Address AddressState
  -- ^ All known addresses and their state.
  } deriving Show

deriveJSON defaultOptions ''GState

-- | Initially this address has a lot of money.
genesisAddressText :: Text
genesisAddressText = "tz1Yz3VPaCNB5FjhdEVnSoN8Xv3ZM8g2LYhw"

-- | KeyHash of genesis address.
genesisKeyHash :: KeyHash
genesisKeyHash =
  either (error . mappend "genesisKeyHash: " . pretty) id $
  parseKeyHash genesisAddressText

-- | Initially this address has a lot of money.
genesisAddress :: Address
genesisAddress = KeyAddress genesisKeyHash

-- | Initial 'GState'. It's supposed to be used if no 'GState' is
-- provided. For now it's empty, but we can hardcode some dummy data
-- in the future.
initGState :: GState
initGState =
  GState
  { gsAddresses = Map.fromList
    [ (genesisAddress, ASSimple maxBound)
    ]
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
    parseFile lByteString =
      if length lByteString == 0
      then pure initGState
      else (either (throwM . GStateParseError) pure . Aeson.eitherDecode') lByteString
    onExc :: IOError -> IO GState
    onExc exc
      | isDoesNotExistError exc = pure initGState
      | otherwise = throwM exc

-- | Write 'GState' to a file.
writeGState :: FilePath -> GState -> IO ()
writeGState fp gs = LBS.writeFile fp (Aeson.encodePretty' config gs)
  where
    config =
      Aeson.defConfig
      { Aeson.confTrailingNewline = True
      }

-- | Updates that can be applied to 'GState'.
data GStateUpdate
  = GSAddAddress !Address
                 !AddressState
  | GSSetStorageValue !Address
                      !Value
  | GSSetBalance !Address
                 !Mutez
  deriving (Show, Eq)

instance Buildable GStateUpdate where
  build =
    \case
      GSAddAddress addr st ->
        "Add address " +| addr |+ " with state " +| st |+ ""
      GSSetStorageValue addr val ->
        "Set storage value of address " +| addr |+ " to " +| val |+ ""
      GSSetBalance addr balance ->
        "Set balance of address " +| addr |+ " to " +| balance |+ ""

data GStateUpdateError
  = GStateAddressExists !Address
  | GStateUnknownAddress !Address
  | GStateNotContract !Address
  deriving (Show)

instance Buildable GStateUpdateError where
  build =
    \case
      GStateAddressExists addr -> "Address already exists: " <> build addr
      GStateUnknownAddress addr -> "Unknown address: " <> build addr
      GStateNotContract addr -> "Address doesn't have contract: " <> build addr

-- | Apply 'GStateUpdate' to 'GState'.
applyUpdate :: GStateUpdate -> GState -> Either GStateUpdateError GState
applyUpdate =
  \case
    GSAddAddress addr st ->
      maybeToRight (GStateAddressExists addr) . addAddress addr st
    GSSetStorageValue addr newValue -> setStorageValue addr newValue
    GSSetBalance addr newBalance -> setBalance addr newBalance

-- | Apply a list of 'GStateUpdate's to 'GState'.
applyUpdates :: [GStateUpdate] -> GState -> Either GStateUpdateError GState
applyUpdates = flip (foldM (flip applyUpdate))

-- | Add an address if it hasn't been added before.
addAddress :: Address -> AddressState -> GState -> Maybe GState
addAddress addr st gs
    | addr `Map.member` accounts = Nothing
    | otherwise = Just (gs {gsAddresses = accounts & at addr .~ Just st})
  where
    accounts = gsAddresses gs

-- | Updare storage value associated with given address.
setStorageValue ::
     Address -> Value -> GState -> Either GStateUpdateError GState
setStorageValue addr newValue = updateAddressState addr modifier
  where
    modifier (ASSimple _) = Left (GStateNotContract addr)
    modifier (ASContract cs) = Right $ ASContract $ cs { csStorage = newValue }

-- | Updare storage value associated with given address.
setBalance :: Address -> Mutez -> GState -> Either GStateUpdateError GState
setBalance addr newBalance = updateAddressState addr (Right . modifier)
  where
    modifier (ASSimple _) = ASSimple newBalance
    modifier (ASContract cs) = ASContract (cs {csBalance = newBalance})

updateAddressState ::
     Address
  -> (AddressState -> Either GStateUpdateError AddressState)
  -> GState
  -> Either GStateUpdateError GState
updateAddressState addr f gs =
  case addresses ^. at addr of
    Nothing -> Left (GStateUnknownAddress addr)
    Just as -> do
      newState <- f as
      return $ gs { gsAddresses = addresses & at addr .~ Just newState }
  where
    addresses = gsAddresses gs

-- | Retrive all contracts stored in GState
extractAllContracts :: GState -> TcOriginatedContracts
extractAllContracts = Map.mapMaybe extractContract . gsAddresses
 where
    extractContract :: AddressState -> Maybe Type
    extractContract =
      \case ASSimple {} -> Nothing
            ASContract cs -> Just (para $ csContract cs)
