
module Lorentz.Contracts.Parse where

import Data.Char
import Control.Applicative
import Control.Monad
import Data.String
import Data.Either
import GHC.TypeLits (KnownSymbol, symbolVal)
-- import Prelude (FilePath)
import Prelude hiding (readEither, unlines, unwords, show, null)
import Data.Function (id)
import Text.Show
import Data.List

import Data.Aeson (eitherDecode)
import Named
import qualified Options.Applicative as Opt
import qualified Data.Text as T
import qualified Options.Applicative.Types as Opt

import Lorentz hiding (contractName)
import Tezos.Crypto (PublicKey, SecretKey)
import Util.Named
import Lorentz.Contracts.Util ()
import Michelson.Parser

import qualified Lorentz.Contracts.GenericMultisig as G
import qualified Lorentz.Contracts.GenericMultisig.Wrapper as G

-- | Dummy `Show` instance: returns the fixed string "SomeParser"
instance Show Opt.SomeParser where
  show _ = "SomeParser"

deriving instance Show Opt.ParseError

-- | Parse whether to output on one line
onelineOption :: Opt.Parser Bool
onelineOption = Opt.switch (
  Opt.long "oneline" <>
  Opt.help "Force single line output")

-- | Parse a `String`
parseString :: String -> Opt.Parser String
parseString name = Opt.strOption $ mconcat
  [ Opt.long name
  , Opt.metavar "STRING"
  , Opt.help $ "String representing the contract's initial " ++ name ++ "."
  ]

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

-- | Parse pairs of signer keys
parseSignerKeyPairs :: String -> Opt.Parser [(PublicKey, PublicKey)]
parseSignerKeyPairs name =
  Opt.option (Opt.eitherReader parser' <|> Opt.auto) $
  mconcat
    [ Opt.long name
    , Opt.metavar "[(PublicKey, PublicKey)]"
    , Opt.help $ "Public keys of multisig " ++ name ++ "."
    ]
  where
    parser' :: String -> Either String [(PublicKey, PublicKey)]
    parser' = eitherDecode . fromString

-- | Parse a `FilePath`, given a name and description
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

-- | Run `Opt.ReadM` on a `String`
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
              bool
                (Left $
                     "Expected the list to end after ']', but got: " ++
                     leftoverStr)
                (return []) $
                null $ dropWhile isSpace leftoverStr
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

-- | Parse an effectful `Lambda`
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

-- | Parse a `SecretKey`
parseSecretKey :: Opt.Parser SecretKey
parseSecretKey =
  Opt.option Opt.auto $
    mconcat
      [ Opt.long "secretKey"
      , Opt.metavar "SecretKey"
      , Opt.help "Private key to sign multisig parameter JSON file"
      ]

-- | Parser `G.SomePublicKey`
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

-- | Parse which contract to print
printOptions :: Opt.Parser Text
printOptions = Opt.strOption $ mconcat
  [ Opt.short 'n'
  , Opt.long "name"
  , Opt.metavar "IDENTIFIER"
  , Opt.help "Name of a contract returned by `list` command."
  ]

-- | Parse the output `FilePath`
outputOptions :: Opt.Parser (Maybe FilePath)
outputOptions = optional . Opt.strOption $ mconcat
  [ Opt.short 'o'
  , Opt.long "output"
  , Opt.metavar "FILEPATH"
  , Opt.help "File to use as output. If not specified, stdout is used."
  ]

