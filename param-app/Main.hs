{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

{-# OPTIONS -Wno-orphans #-}
{-# OPTIONS -Wno-partial-fields #-}

module Main
  ( main
  ) where

import Control.Applicative
import Data.Char
import Data.List
import Data.Typeable
import Data.String
import GHC.TypeLits (KnownSymbol, symbolVal)
import Prelude hiding (readEither, unlines, unwords, show, null)
import Text.Show
import qualified Prelude as P

import qualified Data.ByteString.Lazy as BL
import Data.Aeson (eitherDecode)
import Data.Constraint
import Data.Singletons (SingI(..))
import Data.Version (showVersion)
import Named
import Options.Applicative.Help.Pretty (Doc, linebreak)
import Paths_lorentz_contract_param (version)
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
import Michelson.Macro
import Michelson.Parser
import Michelson.Runtime
import Michelson.TypeCheck
import Michelson.Typed
import Tezos.Crypto (SecretKey)
import Util.IO
import Util.Named
import qualified Lorentz.Base as L
import qualified Lorentz.Contracts.GenericMultisig as G
import qualified Lorentz.Contracts.GenericMultisig.Wrapper as G
import qualified Lorentz.Contracts.ManagedLedger.Athens as Athens
import qualified Lorentz.Contracts.ManagedLedger.Babylon as Babylon

import Multisig

deriving instance Show Opt.ParseError

-- | Dummy `Show` instance: returns the fixed string "SomeParser"
instance Show Opt.SomeParser where
  show _ = "SomeParser"


data CmdLnArgs where
  DefaultContractParams ::
      { renderedParams :: String
      } -> CmdLnArgs
  SomeOperationParam ::
      { contractName :: String
      , contractParam :: SomeContractParam
      } -> CmdLnArgs
  MultisigDefaultParam :: CmdLnArgs
  MultisigChangeKeysParams :: forall key. G.IsKey key =>
      { keyProxy :: Proxy key
      , contractName :: String
      , contractAddress :: Address
      , counter :: Natural
      , threshold :: Natural
      , signerKeys :: [G.Public key]
      , newSignerKeys :: [G.Public key]
      } -> CmdLnArgs
  GenericMultisigOperationParams :: forall key. G.IsKey key =>
      { keyProxy :: Proxy key
      , contractAddress :: Address
      , counter :: Natural
      , genericOperation :: Lambda () [Operation]
      , signerKeys :: [G.Public key]
      } -> CmdLnArgs
  MultisigOperationParams :: forall key. G.IsKey key =>
      { keyProxy :: Proxy key
      , contractName :: String
      , contractAddress :: Address
      , counter :: Natural
      , baseContractParam :: SomeContractParam
      , signerKeys :: [G.Public key]
      } -> CmdLnArgs
  MultisigSomeOperationParams :: forall key. G.IsKey key =>
      { keyProxy :: Proxy key
      , contractName :: String
      , contractFilePath :: Maybe String
      , contractAddress :: Address
      , counter :: Natural
      , untypedBaseContractParam :: String
      , signerKeys :: [G.Public key]
      } -> CmdLnArgs
  MultisigFiles ::
      { multisigFiles :: [FilePath]
      } -> CmdLnArgs
  MultisigSignFile ::
      { secretKey :: SecretKey
      , somePublicKey :: G.SomePublicKey
      , multisigFile :: FilePath
      } -> CmdLnArgs

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

parseSignerKeyPairs :: String -> Opt.Parser [(PublicKey, PublicKey)]
parseSignerKeyPairs name =
  -- Opt.option parser' $
  Opt.option (Opt.eitherReader parser' <|> Opt.auto) $
  -- Opt.option (Opt.eitherReader (error . T.pack) <|> Opt.auto) $
  mconcat
    [ Opt.long name
    , Opt.metavar "[(PublicKey, PublicKey)]"
    , Opt.help $ "Public keys of multisig " ++ name ++ "."
    ]
  where
    parser' :: String -> Either String [(PublicKey, PublicKey)]
    parser' = eitherDecode . fromString

parseFilePath :: String -> String -> Opt.Parser FilePath
parseFilePath name description =
  Opt.strOption $
  mconcat
    [ Opt.long name
    , Opt.metavar "FilePath"
    , Opt.help description
    ]

-- | Parse the signer keys
parseContractName :: Opt.Parser String
parseContractName =
  Opt.strOption $
  mconcat
    [ Opt.long "contractName"
    , Opt.metavar "STRING"
    , Opt.help "Contract name"
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

parseLambda :: String -> String -> Opt.Parser (Lambda () [Operation])
parseLambda name description =
  fmap (\x -> fromVal $
    either (error . T.pack . show) id $
    parseNoEnv
      (G.parseTypeCheckValue @(ToT (Lambda () [Operation])))
      "GenericMultisigContract223" $
    T.pack x) .
  Opt.strOption $
  mconcat
    [ Opt.long name
    , Opt.metavar "Lambda () [Operation]"
    , Opt.help description
    ]

parseSecretKey :: Opt.Parser SecretKey
parseSecretKey =
  Opt.option Opt.auto $
    mconcat
      [ Opt.long "secretKey"
      , Opt.metavar "SecretKey"
      , Opt.help "Private key to sign multisig parameter JSON file"
      ]

parseSomePublicKey :: Opt.Parser G.SomePublicKey
parseSomePublicKey =
  Opt.option parser' $
    mconcat
      [ Opt.long "publicKey"
      , Opt.metavar "publicKey"
      , Opt.help "Public key(s) to sign multisig parameter JSON file"
      ]
  where
    parser' =
      (G.SomePublicKey (Proxy @PublicKey) <$>
      (Opt.auto :: Opt.ReadM PublicKey)) <|>
      (G.SomePublicKey (Proxy @(PublicKey, PublicKey)) <$>
      (Opt.eitherReader (eitherDecode . fromString) <|> Opt.auto :: Opt.ReadM (PublicKey, PublicKey)))

parseChangeKeys :: String -> Opt.Parser CmdLnArgs
parseChangeKeys contractName =
  (MultisigChangeKeysParams (Proxy @PublicKey) contractName <$>
  parseAddress "contractAddress" <*>
  parseNatural "counter" <*>
  parseNatural "threshold" <*>
  parseSignerKeys "signerKeys" <*>
  parseSignerKeys "newSignerKeys") <|>
  (MultisigChangeKeysParams (Proxy @(PublicKey, PublicKey)) contractName <$>
  parseAddress "contractAddress" <*>
  parseNatural "counter" <*>
  parseNatural "threshold" <*>
  parseSignerKeyPairs "signerKeyPairs" <*>
  parseSignerKeyPairs "newSignerKeyPairs")

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
genericMultisigOperationParamsSubCmd ::
  [Opt.Mod Opt.CommandFields CmdLnArgs]
genericMultisigOperationParamsSubCmd =
  [ mkCommandParser
      "GenericMultisigContract223-default"
      (pure MultisigDefaultParam)
      "Default parameter: use to transfer ꜩ (tez) to the contract"
  , mkCommandParser
      "GenericMultisigContract223-change-keys"
      (parseChangeKeys "GenericMultisigContract223")
      "Change keys: update the key list and/or the threshold (i.e. quorum)"
  , mkCommandParser
      "GenericMultisigContract223-operation"
      (GenericMultisigOperationParams (Proxy @(PublicKey, PublicKey)) <$>
      parseAddress "contractAddress" <*>
      parseNatural "counter" <*>
      parseLambda "operation" "A generic operation to sign and execute" <*>
      parseSignerKeyPairs "signerKeyPairs")
      "Change keys: update the key list and/or the threshold (i.e. quorum)"
  ]

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
      (parseChangeKeys contractName)
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
      (MultisigOperationParams (Proxy @PublicKey) contractName <$>
      parseAddress "contractAddress" <*>
      parseNatural "counter" <*>
      fmap toSomeContractParam parseBaseContractParam <*>
      parseSignerKeys "signerKeys") <|>
      (MultisigOperationParams (Proxy @(PublicKey, PublicKey)) contractName <$>
      parseAddress "contractAddress" <*>
      parseNatural "counter" <*>
      fmap toSomeContractParam parseBaseContractParam <*>
      parseSignerKeyPairs "signerKeyPairs")

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

contractBabylonSubCmds :: [(String, Opt.Parser Babylon.Parameter)]
contractBabylonSubCmds =
  [ ( "transfer"
    , Babylon.Transfer <$>
      liftA3
        (,,)
        (parseNamed #from parseAddress)
        (parseNamed #to parseAddress)
        (parseNamed #value parseNatural))
  , ( "approve"
    , Babylon.Approve <$>
      liftA2
        (,)
        (parseNamed #spender parseAddress)
        (parseNamed #value parseNatural))
  , ( "getAllowance"
    , Babylon.GetAllowance <$>
      parseView
        (liftA2
           (,)
           (parseNamed #owner parseAddress)
           (parseNamed #spender parseAddress)))
  , ("getBalance", Babylon.GetBalance <$> parseView (parseAddress "account"))
  , ("getTotalSupply", Babylon.GetTotalSupply <$> parseView (pure ()))
  , ("setPause", Babylon.SetPause <$> parseBool "paused")
  , ( "setAdministrator"
    , Babylon.SetAdministrator <$> parseAddress "new-administrator-address")
  , ("getAdministrator", Babylon.GetAdministrator <$> parseView (pure ()))
  , ( "mint"
    , Babylon.Mint <$>
      liftA2 (,) (parseNamed #to parseAddress) (parseNamed #value parseNatural))
  , ( "burn"
    , Babylon.Burn <$>
      liftA2
        (,)
        (parseNamed #from parseAddress)
        (parseNamed #value parseNatural))
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

wrappedMultisigContractBabylonSubCmd :: [Opt.Mod Opt.CommandFields CmdLnArgs]
wrappedMultisigContractBabylonSubCmd =
  genericMultisigParam
    "WrappedMultisigContractBabylon"
    contractBabylonSubCmds

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

managedLedgerBabylonSubCmd :: [Opt.Mod Opt.CommandFields CmdLnArgs]
managedLedgerBabylonSubCmd =
  genericContractParam
    "ManagedLedgerBabylon"
    contractBabylonSubCmds


wrappedMultisigDefaultSubCmd :: [Opt.Mod Opt.CommandFields CmdLnArgs]
wrappedMultisigDefaultSubCmd =
  [ mkCommandParser
      "WrappedMultisig-default"
      (pure MultisigDefaultParam)
      "Default parameter: use to transfer ꜩ (tez) to the contract"
  ]

wrappedMultisigChangeKeysSubCmd :: [Opt.Mod Opt.CommandFields CmdLnArgs]
wrappedMultisigChangeKeysSubCmd =
  [ mkCommandParser
      "WrappedMultisig-change-keys"
      (parseChangeKeys "WrappedMultisig")
      "Change keys: update the key list and/or the threshold (i.e. quorum)"
  ]

-- | Command to generate a parameter file for a multisig-wrapped contract
-- given the source of an arbitrary contract
multisigSomeOperationParamsSubCmd :: [Opt.Mod Opt.CommandFields CmdLnArgs]
multisigSomeOperationParamsSubCmd =
  [ mkCommandParser
      "MultisigSomeOperationParams"
      parseSomeOperation
      ("Generate a multisig parameter file for an arbitrary multisig-wrapped contract.\n" ++
       "Omit the 'contractFilePath' option to pass the contract through STDIN.")
  ]
  where
    parseSomeOperation :: Opt.Parser CmdLnArgs
    parseSomeOperation =
      (MultisigSomeOperationParams <$>
        pure (Proxy @PublicKey) <*>
        parseContractName <*>
        Opt.optional (parseFilePath "contractFilePath" "File path to the base contract source") <*>
        parseAddress "contractAddress" <*>
        parseNatural "counter" <*>
        parseContractParam <*>
        parseSignerKeys "signerKeys") <|>
      (MultisigSomeOperationParams <$>
        pure (Proxy @(PublicKey, PublicKey)) <*>
        parseContractName <*>
        Opt.optional (parseFilePath "contractFilePath" "File path to the base contract source") <*>
        parseAddress "contractAddress" <*>
        parseNatural "counter" <*>
        parseContractParam <*>
        parseSignerKeyPairs "signerKeyPairs")

    parseContractParam :: Opt.Parser String
    parseContractParam =
      Opt.strOption $
      mconcat
        [ Opt.long "contractParam"
        , Opt.metavar "MICHELSON_VALUE"
        , Opt.help "Contract parameter"
        ]

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
      MultisigSignFile <$> parseSecretKey <*> parseSomePublicKey <*> parseFilePath "signerFile" "File path to multisig parameter JSON file"


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
      -- fmap
      --   (maybe (error "Expected a non-empty list of JSON file paths") MultisigFiles .
      --    nonEmpty) .
      Opt.option (MultisigFiles <$> parseList Opt.str) $
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
    contractNatSubCmd ++
    managedLedgerAthensSubCmd ++
    managedLedgerBabylonSubCmd ++
    wrappedMultisigContractNatSubCmd ++
    wrappedMultisigContractAthensSubCmd ++
    wrappedMultisigContractBabylonSubCmd ++
    wrappedMultisigDefaultSubCmd ++
    wrappedMultisigChangeKeysSubCmd ++
    multisigSomeOperationParamsSubCmd ++
    multisigSignFileSubCmd ++
    multisigSignersFileSubCmd ++
    genericMultisigOperationParamsSubCmd

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
          (G.Default :: G.Parameter PublicKey ())
        MultisigChangeKeysParams {..} ->
          case keyProxy of
            (_ :: Proxy key) ->
              writeMultisigSignersFile $
              makeMultisigSignersFile @key contractName contractAddress counter signerKeys $
              Left (threshold, newSignerKeys)
        GenericMultisigOperationParams {..} ->
          case keyProxy of
            (_ :: Proxy key) ->
              writeMultisigSignersFile $
              makeMultisigSignersFile @key "GenericMultisigContract223" contractAddress counter signerKeys $
              Right $
              toSomeContractParam genericOperation
        MultisigOperationParams {..} ->
          case keyProxy of
            (_ :: Proxy key) ->
              writeMultisigSignersFile $
              makeMultisigSignersFile @key contractName contractAddress counter signerKeys $
              Right baseContractParam
        MultisigSomeOperationParams {..} ->
          case keyProxy of
            (_ :: Proxy key) -> do
              uContract <- expandContract <$> readAndParseContract contractFilePath
              case typeCheckContract mempty uContract of
                Left err -> die $ show err
                Right typeCheckedContract -> do
                  let paramParser =
                        fst
                          $ G.someBigMapContractStorageParams typeCheckedContract
                  someBaseContractParam <-
                    either (die . show) return . parseNoEnv paramParser contractName $
                    T.pack untypedBaseContractParam
                  writeMultisigSignersFile .
                    makeMultisigSignersFile @key
                      contractName
                      contractAddress
                      counter
                      signerKeys .
                    Right $
                    someBaseContractParam
        MultisigSignFile {..} -> do
          signingResult <- runExceptT $
            multisigSignFile (Proxy @PublicKey) secretKey somePublicKey multisigFile <|>
            multisigSignFile (Proxy @(PublicKey, PublicKey)) secretKey somePublicKey multisigFile
          case signingResult of
            Left err -> P.fail err
            Right () -> return ()
        MultisigFiles {..} ->
          case multisigFiles of
            [] -> do
              stdin' <- BL.hGetContents stdin
              let file1 = renderMultisigSignersFile @PublicKey <$> eitherDecode stdin'
              let file2 = renderMultisigSignersFile @(PublicKey, PublicKey) <$> eitherDecode stdin'
              case file1 <|> file2 of
                Left err -> P.fail err
                Right result -> putStrLn result
            (x:xs) -> do
              let multisigFiles' = x :| xs
              concatResult <- runExceptT $
                readConcatMultisigSignersFiles (Proxy @PublicKey) multisigFiles' <|>
                readConcatMultisigSignersFiles (Proxy @(PublicKey, PublicKey)) multisigFiles'
              case concatResult of
                Left err -> P.fail err
                Right () -> return ()


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

