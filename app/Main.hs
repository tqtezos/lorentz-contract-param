module Main
  ( main
  ) where

import Fmt (Builder, blockListF, fmt, nameF, (+|), (|+))
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Singletons (SingI)
import qualified Data.Text.Lazy.IO as TL
import qualified Options.Applicative as Opt

import qualified Lorentz as L
import Lorentz.Contracts.Auction
import Lorentz.Contracts.ManagedLedger.Athens (managedLedgerAthensContract)
import Lorentz.Contracts.ManagedLedger.Babylon (managedLedgerContract)
import Lorentz.Contracts.ManagedLedger.Proxy (managedLedgerProxyContract)
import Lorentz.Contracts.UnsafeLedger
import Lorentz.Contracts.VarStorage
import Lorentz.Contracts.Walker
import qualified Lorentz.Contracts.GenericMultisig.Wrapper as G

import Lorentz.Contracts.Util.Strip
import LorentzContractsOptions
import Michelson.Macro
import Michelson.Parser (program)
import Michelson.Runtime
import Michelson.TypeCheck
import Util.IO
import qualified Michelson.Typed as T


data ContractInfo =
  forall cp st.
    (Each '[SingI] [T.ToT cp, T.ToT st]) =>
  ContractInfo
  { ciContract :: L.Contract cp st
  , ciPrinterOpts :: L.LorentzCompilationWay cp st
  , ciIsDocumented :: Bool
  }

(?::) :: Text -> a -> (Text, a)
(?::) = (,)

contracts :: Map Text ContractInfo
contracts = Map.fromList
  [ "ManagedLedger" ?:: ContractInfo
    { ciContract = managedLedgerContract
    , ciPrinterOpts = L.lcwEntryPoints
    , ciIsDocumented = False
    }
  , "ManagedLedgerAthens" ?:: ContractInfo
    { ciContract = managedLedgerAthensContract
    , ciPrinterOpts = L.lcwEntryPoints
    , ciIsDocumented = False
    }
  , "ManagedLedgerProxy" ?:: ContractInfo
    { ciContract = managedLedgerProxyContract
    , ciPrinterOpts = L.lcwEntryPointsRecursive
    , ciIsDocumented = False
    }
  , "UnsafeLedger" ?:: ContractInfo
    { ciContract = unsafeLedgerContract
    , ciPrinterOpts = L.lcwEntryPoints
    , ciIsDocumented = False
    }
  , "Walker" ?:: ContractInfo
    { ciContract = walkerContract
    , ciPrinterOpts = L.lcwEntryPoints
    , ciIsDocumented = False
    }
  , "Auction" ?:: ContractInfo
    { ciContract = auctionContract
    , ciPrinterOpts = L.lcwDumb
    , ciIsDocumented = False
    }
  , "AddressStorageContract" ?:: ContractInfo
    { ciContract = (varStorageContract @L.Address)
    , ciPrinterOpts = L.lcwDumb
    , ciIsDocumented = False
    }
  , "ExplicitBigMapManagedLedgerAthens" ?:: ContractInfo
    { ciContract = G.explicitBigMapAthens
    , ciPrinterOpts = L.lcwEntryPoints
    , ciIsDocumented = False
    }
  , "MultisigManagedLedgerAthens" ?:: ContractInfo
    { ciContract = G.wrappedMultisigContractAthens
    , ciPrinterOpts = L.lcwEntryPoints
    , ciIsDocumented = False
    }
  , "NatStorageContract" ?:: ContractInfo
    { ciContract = (varStorageContract @Natural)
    , ciPrinterOpts = L.lcwDumb
    , ciIsDocumented = False
    }
  , "NatStorageWithBigMapContract" ?:: ContractInfo
    { ciContract = G.natStorageWithBigMapContract
    , ciPrinterOpts = L.lcwDumb
    , ciIsDocumented = False
    }
  , "WrappedMultisigContractNat" ?:: ContractInfo
    { ciContract = G.wrappedMultisigContractNat
    , ciPrinterOpts = L.lcwDumb
    , ciIsDocumented = False
    }
  ]

wrappedMultisigContractNames :: [Text]
wrappedMultisigContractNames = ["WrappedMultisigBase", "WrappedMultisig"]

getContract :: Text -> IO ContractInfo
getContract name =
  case Map.lookup name contracts of
    Nothing ->
      die $ "No contract with name '" +| name |+ "' found\n" +|
            availableContracts
    Just c -> pure c

availableContracts :: Builder
availableContracts =
  nameF
    "Available contracts"
    (blockListF $ keys contracts ++ wrappedMultisigContractNames)

main :: IO ()
main = do
  hSetTranslit stdout
  hSetTranslit stderr
  cmdLnArgs <- Opt.execParser programInfo
  run cmdLnArgs `catchAny` (die . displayException)
  where
    run :: CmdLnArgs -> IO ()
    run =
      \case
        List -> fmt availableContracts
        Parse inputPathX inputPathY -> do
          inputFileX <- T.readFile $ T.unpack inputPathX
          case parse program (T.unpack inputPathX) inputFileX of
            Left err -> putStrLn $ errorBundlePretty err
            Right parsedX -> do
              inputFileY <- T.readFile $ T.unpack inputPathY
              case parse program (T.unpack inputPathY) inputFileY of
                Left err -> putStrLn $ errorBundlePretty err
                Right parsedY -> compareContracts parsedX parsedY
        Print name mOutput mInput forceOneLine ->
          if name `elem` wrappedMultisigContractNames
            then do
              uContract <- expandContract <$> readAndParseContract mInput
              case typeCheckContract mempty uContract of
                Left err -> die $ show err
                Right typeCheckedContract ->
                  case bool fst snd (name == "WrappedMultisig") $
                       G.wrapSomeTypeCheckedContract typeCheckedContract of
                    L.SomeContract wrappedContract ->
                      maybe TL.putStrLn writeFileUtf8 mOutput $
                      L.printLorentzContract forceOneLine L.lcwDumb wrappedContract
            else case Map.lookup name contracts of
                   Nothing ->
                     die $
                     "No contract with name '" +| name |+ "' found\n" +|
                     availableContracts
                   Just ContractInfo{..} ->
                     maybe TL.putStrLn writeFileUtf8 mOutput $
                     L.printLorentzContract forceOneLine ciPrinterOpts ciContract
        Document name mOutput -> do
          ContractInfo{..} <- getContract name
          if ciIsDocumented
          then maybe TL.putStrLn writeFileUtf8 mOutput $
               T.contractDocToMarkdown $ L.buildLorentzDoc ciContract
          else die "This contract is not documented"

