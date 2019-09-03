module Main
  ( main
  ) where

import Fmt (Builder, blockListF, fmt, nameF, (+|), (|+))
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import qualified Options.Applicative as Opt

import Lorentz
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
import qualified Lorentz.Base as L
import qualified Lorentz.Doc as L
import qualified Michelson.Typed as T


contracts :: Map Text L.SomeContract
contracts = Map.fromList
  [ ("AddressStorageContract", L.SomeContract (varStorageContract @Address))
  , ("Auction", L.SomeContract auctionContract)
  , ("ExplicitBigMapManagedLedgerAthens", L.SomeContract G.explicitBigMapAthens)
  , ("ManagedLedger", L.SomeContract managedLedgerContract)
  , ("ManagedLedgerAthens", L.SomeContract managedLedgerAthensContract)
  , ("ManagedLedgerProxy", L.SomeContract managedLedgerProxyContract)
  , ("MultisigManagedLedgerAthens", L.SomeContract G.wrappedMultisigContractAthens)
  , ("NatStorageContract", L.SomeContract (varStorageContract @Natural))
  , ("NatStorageWithBigMapContract", L.SomeContract G.natStorageWithBigMapContract)
  , ("UnsafeLedger", L.SomeContract unsafeLedgerContract)
  , ("Walker", L.SomeContract walkerContract)
  , ("WrappedMultisigContractNat", L.SomeContract G.wrappedMultisigContractNat)
  ]

wrappedMultisigContractNames :: [Text]
wrappedMultisigContractNames = ["WrappedMultisigBase", "WrappedMultisig"]

getContract :: Text -> IO L.SomeContract
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
                      L.printLorentzContract forceOneLine wrappedContract
            else case Map.lookup name contracts of
                   Nothing ->
                     die $
                     "No contract with name '" +| name |+ "' found\n" +|
                     availableContracts
                   Just (L.SomeContract c) ->
                     maybe TL.putStrLn writeFileUtf8 mOutput $
                     L.printLorentzContract forceOneLine c
        Document name mOutput -> do
          L.SomeContract c <- getContract name
          maybe TL.putStrLn writeFileUtf8 mOutput $
            T.contractDocToMarkdown $ L.buildLorentzDoc c

