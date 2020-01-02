{-# OPTIONS -Wno-orphans #-}

module Main
  ( main
  ) where

import Fmt (Builder, blockListF, fmt, nameF, (+|), (|+))
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- import Data.Singletons (SingI)
import qualified Data.Text.Lazy.IO as TL
import qualified Options.Applicative as Opt

import qualified Lorentz as L
import Lorentz.Contracts.ManagedLedger (managedLedgerContract)
import Lorentz.Contracts.VarStorage
import qualified Lorentz.Contracts.GenericMultisig.Wrapper as GW
import qualified Lorentz.Contracts.GenericMultisig as G

import Lorentz.Contracts.Util.Strip
import LorentzContractsOptions
-- import Michelson.Macro
import Michelson.Parser (program)
-- import Michelson.Runtime
-- import Michelson.TypeCheck
import Util.IO
-- import qualified Michelson.Typed as T

instance L.ParameterEntryPoints L.Address where
  parameterEntryPoints = L.pepNone

instance L.ParameterEntryPoints Natural where
  parameterEntryPoints = L.pepNone

instance (L.NiceParameter a, G.IsKey key) => L.ParameterEntryPoints (G.Parameter key a) where
  parameterEntryPoints = L.pepNone



data ContractInfo =
  forall cp st. (L.NiceParameter cp, L.NiceStorage st, L.ParameterEntryPoints cp) =>
  ContractInfo
  { ciContract :: L.Contract cp st
  , ciIsDocumented :: Bool
  }

(?::) :: Text -> a -> (Text, a)
(?::) = (,)

contracts :: Map Text ContractInfo
contracts = Map.fromList
  [ "ManagedLedger" ?:: ContractInfo
    { ciContract = managedLedgerContract
    , ciIsDocumented = False
    }
  , "AddressStorageContract" ?:: ContractInfo
    { ciContract = (varStorageContract @L.Address)
    , ciIsDocumented = False
    }
  -- , "ExplicitBigMapManagedLedgerAthens" ?:: ContractInfo
  --   { ciContract = GW.explicitBigMapAthens
  --   , ciIsDocumented = False
  --   }
  -- , "MultisigManagedLedgerAthens" ?:: ContractInfo
  --   { ciContract = GW.wrappedMultisigContractAthens
  --   , ciIsDocumented = False
  --   }
  , "NatStorageContract" ?:: ContractInfo
    { ciContract = (varStorageContract @Natural)
    , ciIsDocumented = False
    }
  -- , "NatStorageWithBigMapContract" ?:: ContractInfo
  --   { ciContract = GW.natStorageWithBigMapContract
  --   , ciIsDocumented = False
  --   }
  , "WrappedMultisigContractNat" ?:: ContractInfo
    { ciContract = GW.wrappedMultisigContractNat
    , ciIsDocumented = False
    }
  , "GenericMultisigContract223" ?:: ContractInfo
    { ciContract = G.generigMultisigContract223
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
        Print name mOutput _mInput forceOneLine ->
          if name `elem` wrappedMultisigContractNames
            then error "wrappedMultisigContracts disabled"
            -- do
            --   uContract <- expandContract <$> readAndParseContract mInput
            --   case typeCheckContract mempty uContract of
            --     Left err -> die $ show err
            --     Right typeCheckedContract ->
            --       case bool fst snd (name == "WrappedMultisig") $
            --            GW.wrapSomeTypeCheckedContract typeCheckedContract of
            --         L.SomeContract wrappedContract ->
            --           maybe TL.putStrLn writeFileUtf8 mOutput $
            --           L.printLorentzContract forceOneLine wrappedContract
            else case Map.lookup name contracts of
                   Nothing ->
                     die $
                     "No contract with name '" +| name |+ "' found\n" +|
                     availableContracts
                   Just ContractInfo{..} ->
                     maybe TL.putStrLn writeFileUtf8 mOutput $
                     L.printLorentzContract forceOneLine ciContract
        Document name mOutput -> do
          ContractInfo{..} <- getContract name
          if ciIsDocumented
          then maybe TL.putStrLn writeFileUtf8 mOutput $
               L.contractDocToMarkdown $ L.buildLorentzDoc ciContract
          else die "This contract is not documented"

