module Main
  ( main
  ) where

import qualified Data.Map as Map
import qualified Data.Text.Lazy.IO as TL
import Fmt (blockListF, fmt, nameF, (+|), (|+))
import qualified Options.Applicative as Opt

import qualified Lorentz.Base as L
import Lorentz.Contracts.Auction
import Lorentz.Contracts.ManagedLedger
import Lorentz.Contracts.UnsafeLedger
import Lorentz.Contracts.Walker
import LorentzContractsOptions

contracts :: Map Text L.SomeContract
contracts = Map.fromList
  [ ("ManagedLedger", L.SomeContract managedLedgerContract)
  , ("UnsafeLedger", L.SomeContract unsafeLedgerContract)
  , ("Walker", L.SomeContract walkerContract)
  , ("Auction", L.SomeContract auctionContract)
  ]

main :: IO ()
main = do
  cmdLnArgs <- Opt.execParser programInfo
  run cmdLnArgs `catchAny` (die . displayException)
  where
    run :: CmdLnArgs -> IO ()
    run = \case
      List ->
        fmt availableContracts
      Print name ->
        case Map.lookup name contracts of
          Nothing ->
            die $ "No contract with name '" +| name |+ "' found\n" +|
                  availableContracts
          Just (L.SomeContract c) ->
            TL.putStrLn $ L.printLorentzContract c

    availableContracts =
      nameF "Available contracts" (blockListF $ keys contracts)
