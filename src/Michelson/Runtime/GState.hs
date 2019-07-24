-- | Global blockchain state (emulated).

module Michelson.Runtime.GState
       (
         -- * Auxiliary types
         ContractState (..)
       , getTypedContract
       , getTypedStorage
       , AddressState (..)
       , asBalance

       -- * GState
       , GState (..)
       , genesisAddresses
       , genesisKeyHashes
       , genesisAddress
       -- * More genesisAddresses which can be used in tests
       , genesisAddress1
       , genesisAddress2
       , genesisAddress3
       , genesisAddress4
       , genesisAddress5
       , genesisAddress6
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
import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, (.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString.Lazy as LBS
import Data.List.NonEmpty ((!!))
import qualified Data.Map.Strict as Map
import Fmt ((+|), (|+), (||+))
import Formatting.Buildable (Buildable(build))
import System.IO.Error (IOError, isDoesNotExistError)

import Michelson.TypeCheck
  (SomeContract(..), StorageOrParameter(..), TCError, TcOriginatedContracts,
  typeCheckContract, typeCheckStorageOrParameter)
import Michelson.Typed (SomeValue)
import Michelson.Untyped (Contract, Type, Value, para)
import Tezos.Address (Address(..))
import Tezos.Core (Mutez, divModMutezInt)
import Tezos.Crypto

-- | State of a contract with code.
data ContractState = ContractState
  { csBalance :: !Mutez
  -- ^ Amount of mutez owned by this contract.
  , csStorage :: !Value
  -- ^ Storage value associated with this contract.
  , csContract :: !Contract
  -- ^ Contract itself (untyped).
  , csTypedContract :: !(Maybe SomeContract)
  , csTypedStorage :: !(Maybe SomeValue)
  -- ^ We keep typed representation of contract code
  -- and storage in form, that hides their actual type
  -- in order to simplify the rest of the code
  -- (e.g. avoid type parameters for `ContractState` and so on).
  -- They are made optional in order to perform safe parsing
  -- from JSON (we simply return `Nothing` in this parser and use
  -- `getTypedStorage` or `getTypedContract` that optionally typecheck
  -- storage or contract code).
  }

deriving instance Show ContractState

instance ToJSON ContractState where
  toJSON ContractState{..} = object
    [ "csBalance" .= csBalance
    , "csStorage" .= csStorage
    , "csContract" .= csContract
    ]

-- These instance is a bit hacky because it is quite painful to
-- write proper JSON instances for typed `Instr` and `Value` so
-- we typecheck untyped representation instead of parsing.
instance FromJSON ContractState where
  parseJSON = withObject "contractstate" $ \o -> do
    csBalance <- o .: "csBalance"
    csStorage <- o .: "csStorage"
    csContract <- o .: "csContract"
    let csTypedContract = Nothing
    let csTypedStorage = Nothing
    return ContractState {..}

instance Buildable ContractState where
  build ContractState{..} =
    "Contractstate:\n csBalance: " +| csBalance |+
    "\n  csStorage: " +| csStorage |+
    "\n  csContract: " +| csContract |+
    "\n  csTypedContract: " +| csTypedContract ||+
    "\n  csTypedStorage: " +| csTypedStorage ||+ ""

-- | State of an arbitrary address.
data AddressState
  = ASSimple !Mutez
  -- ^ For contracts without code we store only its balance.
  | ASContract !ContractState
  -- ^ For contracts with code we store more state represented by
  -- 'ContractState'.
  deriving (Show, Generic)

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

getTypedContract :: GState -> ContractState -> Either TCError SomeContract
getTypedContract gs ContractState{..} =
  typeCheckContract (extractAllContracts gs) csContract

getTypedStorage :: GState -> ContractState -> Either TCError SomeValue
getTypedStorage gs ContractState{..} =
  typeCheckStorageOrParameter Storage csStorage (extractAllContracts gs) csContract

-- | Number of genesis addresses.
genesisAddressesNum :: Word
genesisAddressesNum = 10

-- | Secrets from which genesis addresses are derived from.
genesisSecrets :: NonEmpty SecretKey
genesisSecrets = do
  i <- 1 :| [2 .. genesisAddressesNum]
  let seed = encodeUtf8 (show i :: Text)
  return $ detSecretKey seed

-- | KeyHash of genesis address.
genesisKeyHashes :: NonEmpty KeyHash
genesisKeyHashes = hashKey . toPublic <$> genesisSecrets

-- | Initially these addresses have a lot of money.
genesisAddresses :: NonEmpty Address
genesisAddresses = KeyAddress <$> genesisKeyHashes

-- | One of genesis key hashes.
genesisKeyHash :: KeyHash
genesisKeyHash = head genesisKeyHashes

-- | One of genesis addresses.
genesisAddress :: Address
genesisAddress = head genesisAddresses

-- | More genesis addresses
--
-- We know size of @genesisAddresses@, so it is safe to use @!!@
genesisAddress1, genesisAddress2, genesisAddress3 :: Address
genesisAddress4, genesisAddress5, genesisAddress6 :: Address
genesisAddress1 = genesisAddresses !! 1
genesisAddress2 = genesisAddresses !! 2
genesisAddress3 = genesisAddresses !! 3
genesisAddress4 = genesisAddresses !! 4
genesisAddress5 = genesisAddresses !! 5
genesisAddress6 = genesisAddresses !! 6

-- | Initial 'GState'. It's supposed to be used if no 'GState' is
-- provided. It puts plenty of money on each genesis address.
initGState :: GState
initGState =
  GState
  { gsAddresses = Map.fromList
    [ (genesis, ASSimple money)
    | genesis <- toList genesisAddresses
    , let (money, _) = maxBound @Mutez `divModMutezInt` genesisAddressesNum
                    ?: error "Number of genesis addresses is 0"
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
      if null lByteString
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
                      !SomeValue
  | GSSetBalance !Address
                 !Mutez
  deriving Show

instance Buildable GStateUpdate where
  build =
    \case
      GSAddAddress addr st ->
        "Add address " +| addr |+ " with state " +| st |+ ""
      GSSetStorageValue addr val _ ->
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
    GSSetStorageValue addr newValue newTypedValue ->
      setStorageValue addr newValue newTypedValue
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
     Address -> Value -> SomeValue -> GState -> Either GStateUpdateError GState
setStorageValue addr newValue newTypedValue = updateAddressState addr modifier
  where
    modifier (ASSimple _) = Left (GStateNotContract addr)
    modifier (ASContract cs) = Right $ ASContract $
      cs { csStorage = newValue
         , csTypedStorage = Just newTypedValue
         }

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
