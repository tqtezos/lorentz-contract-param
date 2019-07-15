module LorentzContractsOptions
  ( CmdLnArgs (..)
  , programInfo
  ) where

import Data.Version (showVersion)
import qualified Options.Applicative as Opt
import Options.Applicative.Help.Pretty (Doc, linebreak)
import Paths_lorentz_contracts (version)

data CmdLnArgs
  = List
  | Print Text Bool

argParser :: Opt.Parser CmdLnArgs
argParser = Opt.subparser $ mconcat
  [ listSubCmd
  , printSubCmd
  ]
  where
    mkCommandParser commandName parser desc =
      Opt.command commandName $
      Opt.info (Opt.helper <*> parser) $
      Opt.progDesc desc

    listSubCmd =
      mkCommandParser "list"
      (pure List)
      "Show all available contracts"

    printSubCmd =
      mkCommandParser "print"
      (Print <$> printOptions <*> onelineOption)
      "Dump a contract in form of Michelson code"

    printOptions = Opt.strOption $ mconcat
      [ Opt.short 'n'
      , Opt.long "name"
      , Opt.metavar "IDENTIFIER"
      , Opt.help "Name of a contract returned by `list` command."
      ]

    onelineOption :: Opt.Parser Bool
    onelineOption = Opt.switch (
      Opt.long "oneline" <>
      Opt.help "Force single line output")

programInfo :: Opt.ParserInfo CmdLnArgs
programInfo = Opt.info (Opt.helper <*> versionOption <*> argParser) $
  mconcat
  [ Opt.fullDesc
  , Opt.progDesc "Lorentz contracts registry"
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
   , "  lorentz-contracts print --help", linebreak
   ]
