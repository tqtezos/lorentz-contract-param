{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies #-}

{-# OPTIONS -Wno-partial-fields #-}

module Main
  ( main
  ) where

import Control.Applicative
import Data.Char
import Data.List
import Data.String
import GHC.TypeLits (KnownSymbol, symbolVal)
import Prelude hiding (readEither, unlines, unwords, show, null)
import Text.Show
import qualified Prelude as P

import Data.Aeson hiding (Value)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Constraint
import Data.Singletons (SingI(..))
import Data.Version (showVersion)
import Named
import Options.Applicative.Help.Pretty (Doc, linebreak)
import Paths_lorentz_contracts (version)
import qualified Data.Binary as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Types as Opt

import Lorentz hiding (contractName)
import Lorentz.Contracts.Auction
import Lorentz.Contracts.ManagedLedger.Babylon (managedLedgerContract)
import Lorentz.Contracts.ManagedLedger.Proxy (managedLedgerProxyContract)
import Lorentz.Contracts.SomeContractParam
import Lorentz.Contracts.UnsafeLedger
import Lorentz.Contracts.Util ()
import Lorentz.Contracts.VarStorage
import Lorentz.Contracts.Walker
import Michelson.Interpret.Pack
import Michelson.Printer.Util
import Michelson.Typed
import Tezos.Crypto (SecretKey)
import Util.IO
import Util.Named
import qualified Lorentz.Base as L
import qualified Lorentz.Contracts.GenericMultisig as G
import qualified Lorentz.Contracts.GenericMultisig.Wrapper as G
import qualified Lorentz.Contracts.ManagedLedger.Athens as Athens
import qualified Tezos.Crypto as Crypto


deriving instance Show Opt.ParseError

-- | Dummy `Show` instance: returns the fixed string "SomeParser"
instance Show Opt.SomeParser where
  show _ = "SomeParser"

-- | A file with everything needed to sign some multisig contract parameters
data MultisigSignersFile =
  MultisigSignersFile
    { contractName :: String
    , contractAddress :: Address
    , counter :: Natural
    , contractParameter :: Either G.ChangeKeyParams SomeContractParam
    , publicKeys :: [PublicKey]
    , signatures :: Map PublicKey (Maybe Signature)
    }
  deriving (Generic)

instance ToJSON MultisigSignersFile where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON MultisigSignersFile

instance Semigroup MultisigSignersFile where
  fileX <> fileY
    | contractNameX /= contractNameY =
      error $
      "MultisigSignersFile: contract names do not match: " <>
      P.show (contractNameX, contractNameY)
    | contractAddressX /= contractAddressY =
      error $
      "MultisigSignersFile: contract addresses do not match: " <>
      P.show (contractAddressX, contractAddressY)
    | counterX /= counterY =
      error $
      "MultisigSignersFile: counters do not match: " <>
      P.show (counterX, counterY)
    | contractParameterX /= contractParameterY =
      error $
      "MultisigSignersFile: contact parameters do not match: " <>
      P.show (contractParameterX, contractParameterY)
    | Map.keys signaturesX /= Map.keys signaturesY =
      error $
      "MultisigSignersFile: signer keys do not match: " <>
      P.show (Map.keys signaturesX, Map.keys signaturesY)
    | publicKeysX /= publicKeysY =
      error $
      "MultisigSignersFile: signer public keys do not match: " <>
      P.show (publicKeysX, publicKeysY)
    | otherwise =
      fileX
        {signatures = Map.unionWithKey appendSignatures signaturesX signaturesY}
    where
      contractNameX = (contractName :: MultisigSignersFile -> String) fileX
      contractNameY = (contractName :: MultisigSignersFile -> String) fileY
      contractAddressX = (contractAddress :: MultisigSignersFile -> Address) fileX
      contractAddressY = (contractAddress :: MultisigSignersFile -> Address) fileY
      counterX = (counter :: MultisigSignersFile -> Natural) fileX
      counterY = (counter :: MultisigSignersFile -> Natural) fileY
      contractParameterX = contractParameter fileX
      contractParameterY = contractParameter fileY
      publicKeysX = publicKeys fileX
      publicKeysY = publicKeys fileY
      signaturesX = signatures fileX
      signaturesY = signatures fileY
      appendSignatures publicKey mSignatureX mSignatureY =
        case mSignatureX of
          Nothing -> mSignatureY
          Just signatureX ->
            case mSignatureY of
              Nothing -> Just signatureX
              Just signatureY ->
                if signatureX == signatureY
                  then Just signatureX
                  else error $
                       "MultisigSignersFile: key: " <>
                       P.show publicKey <>
                       " has duplicate signatures: " <>
                       P.show (signatureX, signatureY)

-- | Make a `MultisigSignersFile` from a contract name,
-- the current counter, a list of the signers' keys,
-- and a contract parameter
makeMultisigSignersFile ::
     String
  -> Address
  -> Natural
  -> [PublicKey]
  -> Either (Natural, [PublicKey]) SomeContractParam
  -> MultisigSignersFile
makeMultisigSignersFile contractName contractAddress counter signerKeys contractParameter =
  MultisigSignersFile contractName contractAddress counter contractParameter signerKeys signatures
  where
    signatures = Map.fromList . fmap (, Nothing) $ signerKeys

-- | Write a `MultisigSignersFile` to the path generated by the contract name,
-- counter, and base-58-encoded `Crypto.sha256` hash of the parameters
writeMultisigSignersFile ::
     MultisigSignersFile
  -> IO ()
writeMultisigSignersFile multisigSignersFile@MultisigSignersFile{..} = do
  putStrLn $ "Writing parameter to file: " <> show filePath
  BL.writeFile filePath $ encodePretty multisigSignersFile
  where
    filePath :: String
    filePath =
      mconcat [contractName, "_", show counter, "_", paramHash, ".json"]

    paramHash :: String
    paramHash =
      T.unpack . Crypto.encodeBase58Check .
      Crypto.sha256 . BL.toStrict . Binary.runPut . Binary.put . show $
      toJSON contractParameter


-- | Read and parse a `MultisigSignersFile`
readMultisigSignersFile :: FilePath -> IO MultisigSignersFile
readMultisigSignersFile filePath =
  BL.readFile filePath >>= either fail return . eitherDecode

signatureList :: MultisigSignersFile -> [Maybe Signature]
signatureList MultisigSignersFile {..} =
  (\k ->
     Map.findWithDefault
       (error $ "Public key not found: " <> P.show k)
       k
       signatures) <$>
  publicKeys

-- | Sign the parameters in a `MultisigSignersFile`
signMultisigSignersFile ::
     PublicKey -> SecretKey -> MultisigSignersFile -> MultisigSignersFile
signMultisigSignersFile publicKey secretKey multisigSignersFile@MultisigSignersFile {..} =
  case contractParameter of
    Left changeKeyParams' ->
      signMainParams
        (G.ChangeKeys changeKeyParams' :: G.GenericMultisigAction ())
    Right someContractParam' ->
      fromSomeContractParam someContractParam' $ \contractParam ->
        signMainParams $ G.Operation contractParam
  where
    signMainParams ::
         ( Typeable (ToT a)
         , SingI (ToT a)
         , IsoValue a
         , HasNoOp (ToT a)
         , HasNoBigMap (ToT a)
         )
      => G.GenericMultisigAction a
      -> MultisigSignersFile
    signMainParams action' =
      if Crypto.checkSignature publicKey signature packedValue
         then multisigSignersFile {signatures = Map.insert publicKey (Just signature) signatures}
         else error "Unable to verify signature"
      where
        signature =
          Crypto.sign secretKey packedValue

        packedValue =
          packValue' .
          toVal $
          -- G.MainParameter . (, signatureList multisigSignersFile) . (counter, ) $
          (contractAddress, (counter, action'))

-- | Convert to a `Value`, untype, and render
showValue :: (IsoValue t, SingI (ToT t), HasNoOp (ToT t)) => t -> String
showValue = show . renderDoc . untypeValue . toVal

-- | Render a `MultisigSignersFile` as a Michelson parameter
renderMultisigSignersFile :: MultisigSignersFile -> String
renderMultisigSignersFile multisigSignersFile@MultisigSignersFile {..} =
  case contractParameter of
    Left changeKeysParam ->
      makeMainParameter $
      (G.ChangeKeys changeKeysParam :: G.GenericMultisigAction ())
    Right (SomeContractParam xs (_, _) (Dict, Dict)) -> makeMainParameter' xs
  where
    sortedSignatures :: [Maybe Signature]
    sortedSignatures = signatureList multisigSignersFile

    makeMainParameter ::
         (IsoValue a, Typeable (ToT a), SingI (ToT a), HasNoOp (ToT a))
      => G.GenericMultisigAction a
      -> String
    makeMainParameter param =
      showValue $ G.MainParameter ((counter, param), sortedSignatures)

    makeMainParameter' ::
         forall t'. (Typeable t', SingI t', HasNoOp t')
      => Value t'
      -> String
    makeMainParameter' =
      makeMainParameter @(Value t') . G.Operation .
        (fromVal :: Value (ToT (Value t')) -> Value t')


data CmdLnArgs
  = DefaultContractParams
      { renderedParams :: String
      }
  | SomeOperationParam
      { contractName :: String
      , contractParam :: SomeContractParam
      }
  | MultisigDefaultParam
  | MultisigChangeKeysParams
      { contractName :: String
      , contractAddress :: Address
      , counter :: Natural
      , threshold :: Natural
      , signerKeys :: [PublicKey]
      , newSignerKeys :: [PublicKey]
      }
  | MultisigOperationParams
      { contractName :: String
      , contractAddress :: Address
      , counter :: Natural
      , baseContractParam :: SomeContractParam
      , signerKeys :: [PublicKey]
      }
  | MultisigFiles
      { multisigFiles :: NonEmpty FilePath
      }
  | MultisigSignFile
      { secretKey :: SecretKey
      , multisigFile :: FilePath
      }

-- | Option parser to read a `L.Contract`'s parameter type
contractReadParam ::
     Read p
  => String
  -> L.Contract p s
  -> Opt.Parser p
contractReadParam contractName _ =
  Opt.option Opt.auto $
  mconcat
    [ Opt.long "param"
    , Opt.metavar "PARAMETER"
    , Opt.help $ "Parameter for the " ++ contractName ++ " contract."
    ]

-- | Make a command parser, given a command name, parser, and description
mkCommandParser ::
     String -> Opt.Parser a -> String -> Opt.Mod Opt.CommandFields a
mkCommandParser commandName parser desc =
  Opt.command commandName $ Opt.info (Opt.helper <*> parser) $ Opt.progDesc desc

-- | Read and render (or fail with a `String` error message)
-- the parameter type associated with a `L.Contract`
contractReadAndRenderParam ::
     (Read p, IsoValue p, SingI (ToT p), HasNoOp (ToT p))
  => String
  -> L.Contract p s
  -> Opt.Mod Opt.CommandFields CmdLnArgs
-- Either String String
contractReadAndRenderParam contractName contract' =
  mkCommandParser
    contractName
    (DefaultContractParams . showValue <$>
     contractReadParam contractName contract')
    ("Generate parameters for " ++ contractName)

-- | Parse a `Name`d value given its `Name` and a `Opt.Parser`
-- accepting a `String` parameter
parseNamed ::
     forall name a. KnownSymbol name
  => Name name
  -> (String -> Opt.Parser a)
  -> Opt.Parser (name :! a)
parseNamed name' p =
  (name' .!) <$> p (symbolVal (Proxy @name))

-- | Parse a natural number argument, given its field name
parseNatural :: String -> Opt.Parser Natural
parseNatural name =
  Opt.option Opt.auto $
  mconcat
    [ Opt.long name
    , Opt.metavar "NATURAL"
    , Opt.help $ "Natural number representing " ++ name ++ "."
    ]

-- | Parse an `Address` argument, given its field name
parseAddress :: String -> Opt.Parser Address
parseAddress name =
  Opt.option Opt.auto $
  mconcat
    [ Opt.long name
    , Opt.metavar "ADDRESS"
    , Opt.help $ "Address of the " ++ name ++ "."
    ]

-- | Parse a `Bool` (optional) argument, given its field name
parseBool :: String -> Opt.Parser Bool
parseBool name =
  Opt.option Opt.auto $
  mconcat
    [ Opt.long name
    , Opt.metavar "BOOL"
    , Opt.help $
      "Bool representing whether the contract is initially " ++ name ++ "."
    ]

-- | Parse a `View` by parsing its arguments and @"callback-contract"@ address
parseView :: Opt.Parser a -> Opt.Parser (View a r)
parseView parseArg =
  View <$> parseArg <*> fmap ContractAddr (parseAddress "callback-contract")

-- | Parse the signer keys
parseSignerKeys :: String -> Opt.Parser [PublicKey]
parseSignerKeys name =
  Opt.option Opt.auto $
  mconcat
    [ Opt.long name
    , Opt.metavar "List PublicKey"
    , Opt.help $ "Public keys of multisig " ++ name ++ "."
    ]

runReadM :: Opt.ReadM a -> String -> Either String a
runReadM =
  fmap (first show . runIdentity . runExceptT) . runReaderT . Opt.unReadM

-- | Parse a Haskell-style list
parseHaskellList :: forall a. Opt.ReadM a -> Opt.ReadM [a]
parseHaskellList p =
  Opt.eitherReader $ \str ->
    case dropWhile isSpace str of
      '[':begunList -> parseElems begunList
      _ -> Left $ "Expected a String beginning with '[', but got: " ++ str
  where
    parseElems :: String -> Either String [a]
    parseElems str =
      case runReadM p strippedBeforeSeparator of
        Left err -> Left err
        Right parseResult ->
          (parseResult :) <$>
          case withSeparator of
            [] -> return []
            (',':restOfList) -> parseElems restOfList
            (']':leftoverStr) ->
              if null $ dropWhile isSpace leftoverStr
                then return []
                else Left $
                     "Expected the list to end after ']', but got: " ++
                     leftoverStr
            (c:leftoverStr) ->
              Left $
              "Expected ',' or ']', but got: " ++
              [c] ++ " followed by: " ++ leftoverStr
      where
        ~(beforeSeparator, withSeparator) =
          break (liftM2 (||) (== ',') (== ']')) $ dropWhile isSpace str
        strippedBeforeSeparator = dropWhileEnd isSpace beforeSeparator

-- | Parse a Bash-style list
parseBashList :: Opt.ReadM a -> Opt.ReadM [a]
parseBashList p = Opt.eitherReader $ \str ->
  runReadM p `mapM` Data.String.words str

-- | Read a list in a flexible format
parseList :: Opt.ReadM a -> Opt.ReadM [a]
parseList =
  liftM2 (<|>) parseHaskellList parseBashList


-- | Argument parser for `SomeOperationParam`
genericContractParam ::
     ( IsoValue a
     , SingI (ToT a)
     , Typeable (ToT a)
     , HasNoOp (ToT a)
     , HasNoBigMap (ToT a)
     )
  => String
  -> [(String, Opt.Parser a)]
  -> [Opt.Mod Opt.CommandFields CmdLnArgs]
genericContractParam contractName parseBaseContractParams =
  fmap
    (\(operationName, operationParser) ->
       mkCommandParser
         (contractName ++ "-" ++ operationName)
         (parseOperation operationParser)
         (contractName ++ " parameter: " ++ operationName))
    parseBaseContractParams
  where
    parseOperation ::
         ( IsoValue a
         , SingI (ToT a)
         , Typeable (ToT a)
         , HasNoOp (ToT a)
         , HasNoBigMap (ToT a)
         )
      => Opt.Parser a
      -> Opt.Parser CmdLnArgs
    parseOperation parseBaseContractParam =
      SomeOperationParam contractName <$> fmap toSomeContractParam parseBaseContractParam

-- | Given named parsers for a contract's parameters,
-- generate a parser for the associated multisig contract's parameters
genericMultisigParam ::
     ( IsoValue a
     , SingI (ToT a)
     , Typeable (ToT a)
     , HasNoOp (ToT a)
     , HasNoBigMap (ToT a)
     )
  => String
  -> [(String, Opt.Parser a)]
  -> [Opt.Mod Opt.CommandFields CmdLnArgs]
genericMultisigParam contractName parseBaseContractParams =
  [ mkCommandParser
      (contractName ++ "-default")
      (pure MultisigDefaultParam)
      "Default parameter: use to transfer ꜩ (tez) to the contract"
  , mkCommandParser
      (contractName ++ "-change-keys")
      parseChangeKeys
      "Change keys: update the key list and/or the threshold (i.e. quorum)"
  ] ++
  fmap
    (\(operationName, operationParser) ->
       mkCommandParser
         (contractName ++ "-" ++ operationName)
         (parseOperation operationParser)
         (contractName ++ " parameter: " ++ operationName))
    parseBaseContractParams
  where
    parseChangeKeys :: Opt.Parser CmdLnArgs
    parseChangeKeys =
      MultisigChangeKeysParams contractName <$>
      parseAddress "contractAddress" <*>
      parseNatural "counter" <*>
      parseNatural "threshold" <*>
      parseSignerKeys "signerKeys" <*>
      parseSignerKeys "newSignerKeys"

    parseOperation ::
         ( IsoValue a
         , SingI (ToT a)
         , Typeable (ToT a)
         , HasNoOp (ToT a)
         , HasNoBigMap (ToT a)
         )
      => Opt.Parser a
      -> Opt.Parser CmdLnArgs
    parseOperation parseBaseContractParam =
      MultisigOperationParams contractName <$>
      parseAddress "contractAddress" <*>
      parseNatural "counter" <*>
      fmap toSomeContractParam parseBaseContractParam <*>
      parseSignerKeys "signerKeys"


contractNatSubCmds :: [(String, Opt.Parser Natural)]
contractNatSubCmds = [("new-nat", parseNatural "nat")]

contractAthensSubCmds :: [(String, Opt.Parser Athens.Parameter)]
contractAthensSubCmds =
  [ ( "transfer"
    , Athens.Transfer <$>
      liftA3
        (,,)
        (parseNamed #from parseAddress)
        (parseNamed #to parseAddress)
        (parseNamed #value parseNatural))
  , ( "transferViaProxy"
    , Athens.TransferViaProxy <$>
      liftA2
        (,)
        (parseNamed #sender parseAddress)
        (liftA3
           (,,)
           (parseNamed #from parseAddress)
           (parseNamed #to parseAddress)
           (parseNamed #value parseNatural)))
  , ( "approve"
    , Athens.Approve <$>
      liftA2
        (,)
        (parseNamed #spender parseAddress)
        (parseNamed #value parseNatural))
  , ( "approveViaProxy"
    , Athens.ApproveViaProxy <$>
      liftA2
        (,)
        (parseNamed #sender parseAddress)
        (liftA2
           (,)
           (parseNamed #spender parseAddress)
           (parseNamed #value parseNatural)))
  , ( "getAllowance"
    , Athens.GetAllowance <$>
      parseView
        (liftA2
           (,)
           (parseNamed #owner parseAddress)
           (parseNamed #spender parseAddress)))
  , ("getBalance", Athens.GetBalance <$> parseView (parseAddress "account"))
  , ("getTotalSupply", Athens.GetTotalSupply <$> parseView (pure ()))
  , ("setPause", Athens.SetPause <$> parseBool "paused")
  , ( "setAdministrator"
    , Athens.SetAdministrator <$> parseAddress "new-administrator-address")
  , ("getAdministrator", Athens.GetAdministrator <$> parseView (pure ()))
  , ( "mint"
    , Athens.Mint <$>
      liftA2 (,) (parseNamed #to parseAddress) (parseNamed #value parseNatural))
  , ( "burn"
    , Athens.Burn <$>
      liftA2
        (,)
        (parseNamed #from parseAddress)
        (parseNamed #value parseNatural))
  , ("setProxy", Athens.SetProxy <$> parseAddress "new-proxy-address")
  ]


wrappedMultisigContractNatSubCmd :: [Opt.Mod Opt.CommandFields CmdLnArgs]
wrappedMultisigContractNatSubCmd =
  genericMultisigParam
    "WrappedMultisigContractNat"
    contractNatSubCmds

wrappedMultisigContractAthensSubCmd :: [Opt.Mod Opt.CommandFields CmdLnArgs]
wrappedMultisigContractAthensSubCmd =
  genericMultisigParam
    "WrappedMultisigContractAthens"
    contractAthensSubCmds

contractNatSubCmd :: [Opt.Mod Opt.CommandFields CmdLnArgs]
contractNatSubCmd =
  genericContractParam
    "ContractNat"
    contractNatSubCmds

managedLedgerAthensSubCmd :: [Opt.Mod Opt.CommandFields CmdLnArgs]
managedLedgerAthensSubCmd =
  genericContractParam
    "ManagedLedgerAthens"
    contractAthensSubCmds

-- | Command to sign a `MultisigSignersFile`
multisigSignFileSubCmd :: [Opt.Mod Opt.CommandFields CmdLnArgs]
multisigSignFileSubCmd =
  [ mkCommandParser
      "MultisigSignFile"
      parseMultisigSignFile
      "Sign a contract parameter given a multisig signers file in JSON and a private key"
  ]
  where
    parseMultisigSignFile :: Opt.Parser CmdLnArgs
    parseMultisigSignFile =
      MultisigSignFile <$> parseSecretKey <*> parseFilePath

    parseSecretKey :: Opt.Parser SecretKey
    parseSecretKey =
      Opt.option Opt.auto $
        mconcat
          [ Opt.long "secretKey"
          , Opt.metavar "SecretKey"
          , Opt.help "Private key to sign multisig parameter JSON file"
          ]

    parseFilePath :: Opt.Parser FilePath
    parseFilePath =
      Opt.strOption $ -- Opt.option Opt.auto $ -- strOption may be less sensitive to ""'s
      mconcat
        [ Opt.long "signerFile"
        , Opt.metavar "FilePath"
        , Opt.help "File path to multisig parameter JSON file"
        ]


-- | Command to collect, combine, and render a non-empty list
-- of `MultisigSignersFile`'s
multisigSignersFileSubCmd :: [Opt.Mod Opt.CommandFields CmdLnArgs]
multisigSignersFileSubCmd =
  [ mkCommandParser
      "MultisigSignersFile"
      parseMultisigSignersFiles
      "Generate a contract parameter given one or more multisig signers files in JSON"
  ]
  where
    parseMultisigSignersFiles :: Opt.Parser CmdLnArgs
    parseMultisigSignersFiles =
      fmap
        (maybe (error "Expected a non-empty list of JSON file paths") MultisigFiles .
         nonEmpty) .
      Opt.option (parseList Opt.str) $
      mconcat
        [ Opt.long "signerFiles"
        , Opt.metavar "List FilePath"
        , Opt.help "JSON files containing multisig signed parameters"
        ]

argParser :: Opt.Parser CmdLnArgs
argParser =
  Opt.subparser . mconcat $
  -- , contractReadAndRenderParam "ManagedLedgerAthens" managedLedgerAthensContract
  [ contractReadAndRenderParam "Auction" auctionContract
  , contractReadAndRenderParam "ExplicitBigMapManagedLedgerAthens" G.explicitBigMapAthens
  , contractReadAndRenderParam "ManagedLedger" managedLedgerContract
  , contractReadAndRenderParam "ManagedLedgerProxy" managedLedgerProxyContract
  , contractReadAndRenderParam "MultisigManagedLedgerAthens" G.wrappedMultisigContractAthens
  , contractReadAndRenderParam "NatStorageContract" (varStorageContract @Natural)
  , contractReadAndRenderParam "NatStorageWithBigMapContract" G.natStorageWithBigMapContract
  , contractReadAndRenderParam "UnsafeLedger" unsafeLedgerContract
  , contractReadAndRenderParam "Walker" walkerContract
  , contractReadAndRenderParam "WrappedMultisigContractNat" G.wrappedMultisigContractNat
  ] ++
    contractNatSubCmd ++ managedLedgerAthensSubCmd ++
  wrappedMultisigContractNatSubCmd ++
  wrappedMultisigContractAthensSubCmd ++ multisigSignFileSubCmd ++ multisigSignersFileSubCmd

programInfo :: Opt.ParserInfo CmdLnArgs
programInfo =
  Opt.info (Opt.helper <*> versionOption <*> argParser) $
  mconcat
    [ Opt.fullDesc
    , Opt.progDesc "Lorentz contracts parameter generation helper"
    , Opt.header "Lorentz tools"
    , Opt.footerDoc usageDoc
    ]
  where
    versionOption =
      Opt.infoOption
        ("lorentz-contract-param-" <> showVersion version)
        (Opt.long "version" <> Opt.help "Show version.")

usageDoc :: Maybe Doc
usageDoc =
  Just $
  mconcat
    [ "You can use help for specific COMMAND"
    , linebreak
    , "EXAMPLE:"
    , linebreak
    , "  lorentz-contract-param ManagedLedger --help"
    , linebreak
    ]

main :: IO ()
main = do
  hSetTranslit stdout
  hSetTranslit stderr

  -- print . Crypto.toPublic $ either (error . T.pack . show) id $ Crypto.parseSecretKey "edsk3LmXAMnqNZDzT8drMEJTM7AQ3M1zQLP32cjSiR9jQ9mGAV4g1T"
  -- print $ either (error . T.pack . show) id $ Crypto.parsePublicKey "edpktkQJBwKE8kVMgppcMkBtThaRx4uJm37qcuEKAL4hC4Hn579YDW"
  -- undefined

  cmdLnArgs <- Opt.execParser programInfo
  run cmdLnArgs `catchAny` (die . displayException)
  where
    forceSingleLine :: Bool
    forceSingleLine = True

    run :: CmdLnArgs -> IO ()
    run =
      \case
        DefaultContractParams {..} -> putStrLn renderedParams
        SomeOperationParam {..} ->
          case contractParam of
            SomeContractParam xs _ (Dict, _) -> putStrLn $ showValue xs
        MultisigDefaultParam ->
          TL.putStrLn . printLorentzValue forceSingleLine $
          (G.Default :: G.Parameter ())
        MultisigChangeKeysParams {..} ->
          writeMultisigSignersFile $
          makeMultisigSignersFile contractName contractAddress counter signerKeys $
          Left (threshold, newSignerKeys)
        MultisigOperationParams {..} ->
          writeMultisigSignersFile $
          makeMultisigSignersFile contractName contractAddress counter signerKeys $
          Right baseContractParam
        MultisigSignFile {..} -> do
          multisigSignersFile@MultisigSignersFile {..} <-
            readMultisigSignersFile multisigFile
          let publicKey = Crypto.toPublic secretKey
          case signatures Map.!? publicKey of
            Nothing ->
              error "The given public key is not authorized to sign this JSON file"
            Just mSignature ->
              case mSignature of
                Nothing ->
                  writeMultisigSignersFile $
                  signMultisigSignersFile publicKey secretKey multisigSignersFile
                Just _ ->
                  putStrLn
                    ("File has already been signed with the given key" :: String)
        MultisigFiles {..} -> do
          multisigFile <- sconcat <$> mapM readMultisigSignersFile multisigFiles
          putStrLn $ renderMultisigSignersFile multisigFile

-- lorentz-contract-param WrappedMultisigContractAthens-transfer --counter 0 --from "tz1MhGthgRDEK4J5VVzt7r9sBS8FE462DAtr" --to "tz1MhGthgRDEK4J5VVzt7r9sBS8FE462DAtr" --value 3 --signerKeys "[]"
--
-- exampleSignature :: Signature
-- exampleSignature =
--   either (error . fromString . show) id $
--   mkSignature
--     ("\xe5\x56\x43\x00\xc3\x60\xac\x72\x90\x86\xe2\xcc\x80\x6e\x82\x8a\x84\x87\x7f\x1e\xb8\xe5\xd9\x74\xd8\x73\xe0\x65\x22\x49\x01\x55\x5f\xb8\x82\x15\x90\xa3\x3b\xac\xc6\x1e\x39\x70\x1c\xf9\xb4\x6b\xd2\x5b\xf5\xf0\x59\x5b\xbe\x24\x65\x51\x41\x43\x8e\x7a\x10\x0b" :: ByteString)
--
-- readAndRenderTests :: [(String, String)]
-- readAndRenderTests =
--   fmap ((,) "ManagedLedgerAthens") managedLedgerAthensTests ++
--   fmap ((,) "MultisigManagedLedgerAthens") multisigManagedLedgerAthensTests
--   where
--     managedLedgerAthensTests :: [String]
--     managedLedgerAthensTests =
--       [ "Transfer (\"from\" .! \"tz1MhGthgRDEK4J5VVzt7r9sBS8FE462DAtr\", \"to\" .! \"KT1T32YJo9La2czLFpeXPW9J5fVSCt1MHUtx\", \"value\" .! 0)"
--       , "Approve          (\"spender\" .! \"tz1MhGthgRDEK4J5VVzt7r9sBS8FE462DAtr\", \"value\" .! 3)"
--       , "Mint             (\"to\" .! \"tz1MhGthgRDEK4J5VVzt7r9sBS8FE462DAtr\", \"value\" .! 4)"
--       , "Burn             (\"from\" .! \"tz1MhGthgRDEK4J5VVzt7r9sBS8FE462DAtr\", \"value\" .! 4)"
--       , "SetPause         False"
--       , "ApproveViaProxy  (\"sender\" .! \"tz1MhGthgRDEK4J5VVzt7r9sBS8FE462DAtr\", (\"spender\" .! \"tz1MhGthgRDEK4J5VVzt7r9sBS8FE462DAtr\", \"value\" .! 3))"
--       , "SetAdministrator \"tz1MhGthgRDEK4J5VVzt7r9sBS8FE462DAtr\""
--       , "SetProxy \"KT1T32YJo9La2czLFpeXPW9J5fVSCt1MHUtx\""
--       , "SetProxy \"tz1MhGthgRDEK4J5VVzt7r9sBS8FE462DAtr\""
--       , "GetTotalSupply (View () \"KT1T32YJo9La2czLFpeXPW9J5fVSCt1MHUtx\")"
--       , "GetAdministrator (View () \"KT1T32YJo9La2czLFpeXPW9J5fVSCt1MHUtx\")"
--       , "GetAllowance (View (\"owner\" .! \"tz1MhGthgRDEK4J5VVzt7r9sBS8FE462DAtr\", \"spender\" .! \"tz1MhGthgRDEK4J5VVzt7r9sBS8FE462DAtr\") \"KT1T32YJo9La2czLFpeXPW9J5fVSCt1MHUtx\")"
--       , "GetBalance (View \"tz1MhGthgRDEK4J5VVzt7r9sBS8FE462DAtr\" \"KT1T32YJo9La2czLFpeXPW9J5fVSCt1MHUtx\")"
--       ]
--
--     multisigManagedLedgerAthensTests :: [String]
--     multisigManagedLedgerAthensTests =
--       [ "Default"
--       , "MainParameter ((0, ChangeKeys (1, [])), [])"
--       , "MainParameter ((0, ChangeKeys (11, [])), [Nothing])"
--       , "MainParameter ((0, ChangeKeys (111, [])), [Nothing, Nothing])"
--       -- , "MainParameter ((0, ChangeKeys (111, [])), [Just \"" ++ T.unpack (formatSignature exampleSignature) ++ "\"])"
--       ] ++
--       fmap
--         (\athensOperation ->
--            "MainParameter ((0, Operation " ++ athensOperation ++ "), [])")
--         managedLedgerAthensTests

