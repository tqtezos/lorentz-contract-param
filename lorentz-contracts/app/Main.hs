module Main
  ( main
  ) where

import qualified Data.Map as Map
import qualified Data.Text.Lazy.IO as TL
import Fmt (Builder, blockListF, fmt, nameF, (+|), (|+))
import qualified Options.Applicative as Opt

import qualified Lorentz.Base as L
import Lorentz.Contracts.Auction
import Lorentz.Contracts.ManagedLedger
import Lorentz.Contracts.UnsafeLedger
import Lorentz.Contracts.Walker
import qualified Lorentz.Doc as L
import LorentzContractsOptions
import qualified Michelson.Typed as T
import Util.IO

contracts :: Map Text L.SomeContract
contracts = Map.fromList
  [ ("ManagedLedger", L.SomeContract managedLedgerContract)
  , ("UnsafeLedger", L.SomeContract unsafeLedgerContract)
  , ("Walker", L.SomeContract walkerContract)
  , ("Auction", L.SomeContract auctionContract)
  ]

getContract :: Text -> IO L.SomeContract
getContract name =
  case Map.lookup name contracts of
    Nothing ->
      die $ "No contract with name '" +| name |+ "' found\n" +|
            availableContracts
    Just c -> pure c

availableContracts :: Builder
availableContracts =
  nameF "Available contracts" (blockListF $ keys contracts)

main :: IO ()
main = do
  hSetTranslit stdout
  hSetTranslit stderr
  cmdLnArgs <- Opt.execParser programInfo
  run cmdLnArgs `catchAny` (die . displayException)
  where
    run :: CmdLnArgs -> IO ()
    run = \case
      List ->
        fmt availableContracts
      Print name mOutput forceOneLine ->
        case Map.lookup name contracts of
          Nothing ->
            die $ "No contract with name '" +| name |+ "' found\n" +|
                  availableContracts
          Just (L.SomeContract c) ->
            maybe TL.putStrLn writeFileUtf8 mOutput $
              L.printLorentzContract forceOneLine c
      Document name mOutput -> do
        L.SomeContract c <- getContract name
        maybe TL.putStrLn writeFileUtf8 mOutput $
          T.contractDocToMarkdown $ L.buildLorentzDoc c
