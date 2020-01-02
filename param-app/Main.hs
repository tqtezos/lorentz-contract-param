{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

{-# OPTIONS -Wno-orphans #-}
{-# OPTIONS -Wno-partial-fields #-}

module Main
  ( main
  ) where

import Control.Applicative
import Data.List
import Data.Coerce
import Data.Typeable
import Data.String
import Prelude hiding (readEither, unlines, unwords, show, null)
import qualified Prelude as P

import qualified Data.ByteString.Lazy as BL
import Named
import Data.Aeson (eitherDecode)
import Data.Constraint
import Data.Singletons (SingI(..))
import Data.Version (showVersion)
import Options.Applicative.Help.Pretty (Doc, linebreak)
import Paths_lorentz_contract_param (version)
-- import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import qualified Options.Applicative as Opt

import Lorentz hiding (contractName)
import Lorentz.Contracts.ManagedLedger (managedLedgerContract)
import Lorentz.Contracts.SomeContractParam
import Lorentz.Contracts.Util ()
import Lorentz.Contracts.VarStorage
-- import Michelson.Macro
-- import Michelson.Parser
-- import Michelson.Runtime
-- import Michelson.TypeCheck
import Michelson.Typed
import Util.IO
import qualified Lorentz.Base as L
import qualified Lorentz.Contracts.GenericMultisig as G
import qualified Lorentz.Contracts.GenericMultisig.Wrapper as G
import qualified Lorentz.Contracts.ManagedLedger as ManagedLedger
import Lorentz.Contracts.Parse

import Multisig

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
  -- MultisigSomeOperationParams :: forall key. G.IsKey key =>
  --     { keyProxy :: Proxy key
  --     , contractName :: String
  --     , contractFilePath :: Maybe String
  --     , contractAddress :: Address
  --     , counter :: Natural
  --     , untypedBaseContractParam :: String
  --     , signerKeys :: [G.Public key]
  --     } -> CmdLnArgs
  MultisigFiles ::
      { multisigFiles :: [FilePath]
      } -> CmdLnArgs
  -- MultisigSignFile ::
  --     { secretKey :: SecretKey
  --     , somePublicKey :: G.SomePublicKey
  --     , multisigFile :: FilePath
  --     } -> CmdLnArgs
  MultisigVerifyFile ::
      { multisigFile :: FilePath
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

-- contractManagedLedgerSubCmds :: [(String, Opt.Parser ManagedLedger.Parameter)]
-- contractManagedLedgerSubCmds =
--   [ ( "transfer"
--     , ManagedLedger.Transfer <$>
--       liftA3
--         (,,)
--         (parseNamed #from parseAddress)
--         (parseNamed #to parseAddress)
--         (parseNamed #value parseNatural))
--   -- , ( "transferViaProxy"
--   --   , ManagedLedger.TransferViaProxy <$>
--   --     liftA2
--   --       (,)
--   --       (parseNamed #sender parseAddress)
--   --       (liftA3
--   --          (,,)
--   --          (parseNamed #from parseAddress)
--   --          (parseNamed #to parseAddress)
--   --          (parseNamed #value parseNatural)))
--   , ( "approve"
--     , ManagedLedger.Approve <$>
--       liftA2
--         (,)
--         (parseNamed #spender parseAddress)
--         (parseNamed #value parseNatural))
--   -- , ( "approveViaProxy"
--   --   , ManagedLedger.ApproveViaProxy <$>
--   --     liftA2
--   --       (,)
--   --       (parseNamed #sender parseAddress)
--   --       (liftA2
--   --          (,)
--   --          (parseNamed #spender parseAddress)
--   --          (parseNamed #value parseNatural)))
--   , ( "getAllowance"
--     , ManagedLedger.GetAllowance <$>
--       parseView
--         (liftA2
--            (,)
--            (parseNamed #owner parseAddress)
--            (parseNamed #spender parseAddress)))
--   , ("getBalance", ManagedLedger.GetBalance <$> parseView (parseAddress "account"))
--   , ("getTotalSupply", ManagedLedger.GetTotalSupply <$> parseView (pure ()))
--   , ("setPause", ManagedLedger.SetPause <$> parseBool "paused")
--   , ( "setAdministrator"
--     , ManagedLedger.SetAdministrator <$> parseAddress "new-administrator-address")
--   , ("getAdministrator", ManagedLedger.GetAdministrator <$> parseView (pure ()))
--   , ( "mint"
--     , ManagedLedger.Mint <$>
--       liftA2 (,) (parseNamed #to parseAddress) (parseNamed #value parseNatural))
--   , ( "burn"
--     , ManagedLedger.Burn <$>
--       liftA2
--         (,)
--         (parseNamed #from parseAddress)
--         (parseNamed #value parseNatural))
--   -- , ("setProxy", ManagedLedger.SetProxy <$> parseAddress "new-proxy-address")
--   ]

contractManagedLedgerSubCmds :: [(String, Opt.Parser ManagedLedger.Parameter)]
contractManagedLedgerSubCmds =
  [ ( "transfer"
    , ManagedLedger.Transfer <$>
      liftA3
        (,,)
        (parseNamed #from parseAddress)
        (parseNamed #to parseAddress)
        (parseNamed #value parseNatural))
  , ( "approve"
    , ManagedLedger.Approve <$>
      liftA2
        (,)
        (parseNamed #spender parseAddress)
        (parseNamed #value parseNatural))
  , ( "getAllowance"
    , ManagedLedger.GetAllowance <$>
      parseView
        (liftA2
           (,)
           (parseNamed #owner parseAddress)
           (parseNamed #spender parseAddress)))
  , ("getBalance", ManagedLedger.GetBalance <$> parseView (coerce <$> parseAddress "account"))
  , ("getTotalSupply", ManagedLedger.GetTotalSupply <$> parseView (pure ()))
  , ("setPause", ManagedLedger.SetPause <$> parseBool "paused")
  , ( "setAdministrator"
    , ManagedLedger.SetAdministrator <$> parseAddress "new-administrator-address")
  , ("getAdministrator", ManagedLedger.GetAdministrator <$> parseView (pure ()))
  , ( "mint"
    , ManagedLedger.Mint <$>
      liftA2 (,) (parseNamed #to parseAddress) (parseNamed #value parseNatural))
  , ( "burn"
    , ManagedLedger.Burn <$>
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

wrappedMultisigContractManagedLedgerSubCmd :: [Opt.Mod Opt.CommandFields CmdLnArgs]
wrappedMultisigContractManagedLedgerSubCmd =
  genericMultisigParam
    "WrappedMultisigContractManagedLedger"
    contractManagedLedgerSubCmds

-- wrappedMultisigContractManagedLedgerSubCmd :: [Opt.Mod Opt.CommandFields CmdLnArgs]
-- wrappedMultisigContractManagedLedgerSubCmd =
--   genericMultisigParam
--     "WrappedMultisigContractManagedLedger"
--     contractManagedLedgerSubCmds

contractNatSubCmd :: [Opt.Mod Opt.CommandFields CmdLnArgs]
contractNatSubCmd =
  genericContractParam
    "ContractNat"
    contractNatSubCmds

managedLedgerSubCmd :: [Opt.Mod Opt.CommandFields CmdLnArgs]
managedLedgerSubCmd =
  genericContractParam
    "ManagedLedgerManagedLedger"
    contractManagedLedgerSubCmds

-- managedLedgerManagedLedgerSubCmd :: [Opt.Mod Opt.CommandFields CmdLnArgs]
-- managedLedgerManagedLedgerSubCmd =
--   genericContractParam
--     "ManagedLedgerManagedLedger"
--     contractManagedLedgerSubCmds


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

-- -- | Command to generate a parameter file for a multisig-wrapped contract
-- -- given the source of an arbitrary contract
-- multisigSomeOperationParamsSubCmd :: [Opt.Mod Opt.CommandFields CmdLnArgs]
-- multisigSomeOperationParamsSubCmd =
--   [ mkCommandParser
--       "MultisigSomeOperationParams"
--       parseSomeOperation
--       ("Generate a multisig parameter file for an arbitrary multisig-wrapped contract.\n" ++
--        "Omit the 'contractFilePath' option to pass the contract through STDIN.")
--   ]
--   where
--     parseSomeOperation :: Opt.Parser CmdLnArgs
--     parseSomeOperation =
--       (MultisigSomeOperationParams <$>
--         pure (Proxy @PublicKey) <*>
--         parseContractName <*>
--         Opt.optional (parseFilePath "contractFilePath" "File path to the base contract source") <*>
--         parseAddress "contractAddress" <*>
--         parseNatural "counter" <*>
--         parseContractParam <*>
--         parseSignerKeys "signerKeys") <|>
--       (MultisigSomeOperationParams <$>
--         pure (Proxy @(PublicKey, PublicKey)) <*>
--         parseContractName <*>
--         Opt.optional (parseFilePath "contractFilePath" "File path to the base contract source") <*>
--         parseAddress "contractAddress" <*>
--         parseNatural "counter" <*>
--         parseContractParam <*>
--         parseSignerKeyPairs "signerKeyPairs")
--
--     parseContractParam :: Opt.Parser String
--     parseContractParam =
--       Opt.strOption $
--       mconcat
--         [ Opt.long "contractParam"
--         , Opt.metavar "MICHELSON_VALUE"
--         , Opt.help "Contract parameter"
--         ]

-- -- | Command to sign a `MultisigSignersFile`
-- multisigSignFileSubCmd :: [Opt.Mod Opt.CommandFields CmdLnArgs]
-- multisigSignFileSubCmd =
--   [ mkCommandParser
--       "MultisigSignFile"
--       parseMultisigSignFile
--       "Sign a contract parameter given a multisig signers file in JSON and a private key"
--   ]
--   where
--     parseMultisigSignFile :: Opt.Parser CmdLnArgs
--     parseMultisigSignFile =
--       MultisigSignFile <$> parseSecretKey <*> parseSomePublicKey <*> parseFilePath "signerFile" "File path to multisig parameter JSON file"

-- | Command to verify a `MultisigSignersFile`
multisigVerifyFileSubCmd :: [Opt.Mod Opt.CommandFields CmdLnArgs]
multisigVerifyFileSubCmd =
  [ mkCommandParser
      "MultisigVerifyFile"
      parseMultisigVerifyFile
      "Verify the contract parameter signatures given a multisig signers file in JSON"
  ]
  where
    parseMultisigVerifyFile :: Opt.Parser CmdLnArgs
    parseMultisigVerifyFile =
      MultisigVerifyFile <$> parseFilePath "signerFile" "File path to multisig parameter JSON file"

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
  -- , contractReadAndRenderParam "ManagedLedger" managedLedgerContract
  -- [ contractReadAndRenderParam "Auction" auctionContract
  -- , contractReadAndRenderParam "ExplicitBigMapManagedLedger" G.explicitBigMapManagedLedger
  [ contractReadAndRenderParam "ManagedLedger" managedLedgerContract
  -- , contractReadAndRenderParam "MultisigManagedLedger" G.wrappedMultisigContractManagedLedger
  , contractReadAndRenderParam "NatStorageContract" (varStorageContract @Natural)
  -- , contractReadAndRenderParam "NatStorageWithBigMapContract" G.natStorageWithBigMapContract
  , contractReadAndRenderParam "WrappedMultisigContractNat" G.wrappedMultisigContractNat
  ] ++
    contractNatSubCmd ++
    managedLedgerSubCmd ++
    -- managedLedgerManagedLedgerSubCmd ++
    wrappedMultisigContractNatSubCmd ++
    wrappedMultisigContractManagedLedgerSubCmd ++
    -- wrappedMultisigContractManagedLedgerSubCmd ++
    wrappedMultisigDefaultSubCmd ++
    wrappedMultisigChangeKeysSubCmd ++
    -- multisigSomeOperationParamsSubCmd ++
    -- multisigSignFileSubCmd ++
    multisigVerifyFileSubCmd ++
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
        -- MultisigSomeOperationParams {..} ->
        --   case keyProxy of
        --     (_ :: Proxy key) -> do
        --       uContract <- expandContract <$> readAndParseContract contractFilePath
        --       case typeCheckContract mempty uContract of
        --         Left err -> die $ show err
        --         Right typeCheckedContract -> do
        --           let paramParser =
        --                 fst
        --                   $ G.someBigMapContractStorageParams typeCheckedContract
        --           someBaseContractParam <-
        --             either (die . show) return . parseNoEnv paramParser contractName $
        --             T.pack untypedBaseContractParam
        --           writeMultisigSignersFile .
        --             makeMultisigSignersFile @key
        --               contractName
        --               contractAddress
        --               counter
        --               signerKeys .
        --             Right $
        --             someBaseContractParam
        -- MultisigSignFile {..} -> do
        --   signingResult <- runExceptT $
        --     multisigSignFile (Proxy @PublicKey) secretKey somePublicKey multisigFile <|>
        --     multisigSignFile (Proxy @(PublicKey, PublicKey)) secretKey somePublicKey multisigFile
        --   case signingResult of
        --     Left err -> P.fail err
        --     Right () -> return ()
        MultisigVerifyFile {..} -> do
          verificationResult <- runExceptT $
            multisigVerifyFile (Proxy @PublicKey) multisigFile <|>
            multisigVerifyFile (Proxy @(PublicKey, PublicKey)) multisigFile
          case verificationResult of
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

