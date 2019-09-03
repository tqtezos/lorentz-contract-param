{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Main
  ( main
  ) where

import Options.Applicative.Help.Pretty (Doc, linebreak)
import qualified Data.Text.Lazy.IO as TL
import qualified Options.Applicative as Opt

import Lorentz
import Lorentz.Contracts.GenericMultisig.Wrapper
import qualified Lorentz.Contracts.ManagedLedger.Athens as Athens
import qualified Lorentz.Contracts.ManagedLedger.Types as Athens
import Lorentz.Contracts.Util ()
import Util.IO

import Data.Version (showVersion)
import Paths_lorentz_contracts (version)

data CmdLnArgs
  = ManagedLedgerAthens
      { admin       :: Address
      , paused      :: Bool
      , totalSupply :: Natural
      , proxyAdmin  :: Address
      }
  | WrappedMultisigContractNat
      { initialNat :: Natural
      , threshold  :: Natural
      , signerKeys :: [PublicKey]
      }
  | WrappedMultisigContractAthens
      { admin       :: Address
      , paused      :: Bool
      , totalSupply :: Natural
      , proxyAdmin  :: Address
      , threshold   :: Natural
      , signerKeys  :: [PublicKey]
      }
  | WrappedMultisigContractGeneric
      { contractName :: String
      , contractFilePath :: Maybe FilePath
      , contractInitialStorage :: String
      , threshold   :: Natural
      , signerKeys  :: [PublicKey]
      }

argParser :: Opt.Parser CmdLnArgs
argParser = Opt.subparser $ mconcat
  [ managedLedgerAthensSubCmd
  , wrappedMultisigContractNatSubCmd
  , wrappedMultisigContractAthensSubCmd
  , wrappedMultisigContractGenericSubCmd
  ]
  where
    mkCommandParser commandName parser desc =
      Opt.command commandName $
      Opt.info (Opt.helper <*> parser) $
      Opt.progDesc desc

    managedLedgerAthensSubCmd =
      mkCommandParser
        "ManagedLedgerAthens"
        (ManagedLedgerAthens <$> parseAddress "admin" <*>
         parseBool "paused" <*>
         parseNatural "totalSupply" <*>
         parseAddress "proxyAdmin")
        "Make initial storage for ManagedLedgerAthens"

    wrappedMultisigContractNatSubCmd =
      mkCommandParser
        "WrappedMultisigContractNat"
        (WrappedMultisigContractNat <$> parseNatural "initialNat" <*>
         parseNatural "threshold" <*>
         parseSignerKeys)
        "Make initial storage for WrappedMultisigContractNat"

    wrappedMultisigContractAthensSubCmd =
      mkCommandParser
        "WrappedMultisigContractAthens"
        (WrappedMultisigContractAthens <$> parseAddress "admin" <*>
         parseBool "paused" <*>
         parseNatural "totalSupply" <*>
         parseAddress "proxyAdmin" <*>
         parseNatural "threshold" <*>
         parseSignerKeys)
        "Make initial storage for WrappedMultisigContractAthens"

    wrappedMultisigContractGenericSubCmd =
      mkCommandParser
        "WrappedMultisigContractGeneric"
        (WrappedMultisigContractGeneric <$>
         parseString "contractName" <*>
         Opt.optional (parseString "contractFilePath") <*>
         parseString "contractInitialStorage" <*>
         parseNatural "threshold" <*>
         parseSignerKeys)
        "Make initial storage for some wrapped Michelson contract"

    parseNatural name = Opt.option Opt.auto $ mconcat
      [ Opt.long name
      , Opt.metavar "NATURAL"
      , Opt.help $ "Natural number representing " ++ name ++ "."
      ]

    parseAddress name = Opt.option Opt.auto $ mconcat
      [ Opt.long name
      , Opt.metavar "ADDRESS"
      , Opt.help $ "Address of the " ++ name ++ "."
      ]

    parseBool name = Opt.option Opt.auto $ mconcat
      [ Opt.long name
      , Opt.metavar "BOOL"
      , Opt.help $ "Bool representing whether the contract is initially " ++ name ++ "."
      ]

    parseString name = Opt.option Opt.auto $ mconcat
      [ Opt.long name
      , Opt.metavar "STRING"
      , Opt.help $ "String representing the contract's initial " ++ name ++ "."
      ]

    parseSignerKeys = Opt.option Opt.auto $ mconcat
      [ Opt.long "signerKeys"
      , Opt.metavar "List PublicKey"
      , Opt.help $ "Public keys of multisig signers."
      ]

programInfo :: Opt.ParserInfo CmdLnArgs
programInfo = Opt.info (Opt.helper <*> versionOption <*> argParser) $
  mconcat
  [ Opt.fullDesc
  , Opt.progDesc "Lorentz contracts intial storage helper"
  , Opt.header "Lorentz tools"
  , Opt.footerDoc usageDoc
  ]
  where
    versionOption = Opt.infoOption ("lorentz-contracts-" <> showVersion version)
      (Opt.long "version" <> Opt.help "Show version.")

usageDoc :: Maybe Doc
usageDoc = Just $ mconcat
   [ "You can use help for specific COMMAND", linebreak
   , "EXAMPLE:", linebreak
   , "  lorentz-contract-storage print --help", linebreak
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
        ManagedLedgerAthens {..} ->
          TL.putStrLn . printLorentzValue forceSingleLine .
          Athens.Storage' mempty $ Athens.StorageFields admin paused totalSupply (Left proxyAdmin)
        WrappedMultisigContractNat {..} ->
          TL.putStrLn . printLorentzValue forceSingleLine $
          initStorageWrappedMultisigContractNat initialNat threshold signerKeys
        WrappedMultisigContractAthens {..} ->
          TL.putStrLn . printLorentzValue forceSingleLine $
          initStorageWrappedMultisigContractAthens
            admin
            paused
            totalSupply
            (Left proxyAdmin)
            threshold
            signerKeys
        WrappedMultisigContractGeneric {..} -> do
          -- uContract <- expandContract <$> readAndParseContract contractFilePath
          -- case typeCheckContract mempty uContract of
          --   Left err -> die $ show err
          --   Right typeCheckedContract ->
          --     _
          putStrLn
            ("WrappedMultisigContractGeneric not currently supported" :: String)

