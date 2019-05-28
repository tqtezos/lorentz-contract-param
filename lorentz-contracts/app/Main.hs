module Main
  ( main
  ) where

import qualified Data.Map as Map
import qualified Data.Text.Lazy.IO as TL
import Fmt (blockListF, fmt, nameF, (+|), (|+))
import qualified Options.Applicative as Opt

import Michelson.Printer
import qualified Michelson.Untyped as U

import qualified LorentzContracts
import LorentzContractsOptions

contracts :: Map Text U.Contract
contracts = LorentzContracts.contracts

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
          Just c ->
            TL.putStrLn $ printUntypedContract c

    availableContracts =
      nameF "Available contracts" (blockListF $ keys contracts)
