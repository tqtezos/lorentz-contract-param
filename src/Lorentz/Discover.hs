-- | Utilities used for contracts discovery.
--
-- All the discovery logic resides in 'lorentz-discover' executable.
module Lorentz.Discover
  ( IsContract (..)

  , ExportedContractInfo (..)
  , ExportedContractDecl (..)

  , isHaskellModule
  , haskellExportsParser
  ) where

import Data.Char (isAlphaNum)
import Data.Singletons (SingI)
import qualified Data.Text as T
import System.FilePath.Posix (takeExtension, takeFileName)
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as PL

import qualified Lorentz.Base as L
import Lorentz.Constraints
import qualified Michelson.Typed as T
import qualified Michelson.Untyped as U

-- | Defined for values representing a contract.
class IsContract c where
  toUntypedContract :: c -> U.Contract

instance IsContract U.Contract where
  toUntypedContract = id

instance (SingI cp, SingI st) => IsContract (T.Contract cp st) where
  toUntypedContract = T.convertContract

instance ( SingI (T.ToT cp), SingI (T.ToT st)
         , NoOperation cp, NoOperation st, NoBigMap cp, CanHaveBigMap st
         ) =>
         IsContract (L.Contract cp st) where
  toUntypedContract = toUntypedContract . L.compileLorentzContract

-- | Information about a contract required for contracts registry.
data ExportedContractInfo = ExportedContractInfo
  { eciModuleName :: Text
  , eciContractDecl :: ExportedContractDecl
  } deriving (Show, Eq)

-- | Contract names, for Haskell and for humans.
data ExportedContractDecl = ExportedContractDecl
  { ecdName :: Text
    -- ^ Identifier of a contract, e.g. "auction".
  , ecdVar :: Text
    -- ^ Name of a contract as is appears in Haskell code.
  } deriving (Show, Eq)

isHaskellModule :: FilePath -> Bool
isHaskellModule path =
  let file = takeFileName path
  in and
     [ takeExtension file == ".hs"
     , all (\c -> isAlphaNum c || c == '_' || c == '.') file
     ]

haskellExportsParser :: Parsec Void Text [ExportedContractInfo]
haskellExportsParser = do
  space
  symbol "module"
  moduleName <-
    lexeme $ P.takeWhile1P (Just "module name")
                           (\c -> isAlphaNum c || c `elem` ['.', '_'])
  symbol "("
  exports <- exportItems
  symbol ")"
  symbol "where"
  return
    [ ExportedContractInfo
      { eciModuleName = moduleName
      , eciContractDecl = decl
      }
    | exportRaw <- exports
    , let export = T.strip exportRaw
    , Just decl <- pure $ toContractDecl export
    ]
  where
    exportItems :: Parsec Void Text [Text]
    exportItems = P.sepBy exportItem (symbol ",")

    -- We do not follow the syntax precisely, just trying to parse
    -- all valid Haskell programs
    exportItem = fmap mconcat . many . lexeme $
      P.choice
        [ P.takeWhile1P Nothing isExportEntryChar
        , do symbol "("
             _ <- exportItems
             symbol ")"
             return "(..)"  -- we are not interested in content
        , symbol ".." $> ".."
        ]

    space = PL.space P.space1 (PL.skipLineComment "--")
                              (PL.skipBlockComment "{-" "-}")
    lexeme = PL.lexeme space
    symbol = void . PL.symbol space

isExportEntryChar :: Char -> Bool
isExportEntryChar c = isAlphaNum c || c `elem` ['.', '_']

toContractDecl :: Text -> Maybe ExportedContractDecl
toContractDecl varName = do
  rawName <- T.stripPrefix "contract_" varName
  guard $ all isExportEntryChar $ toString rawName
  let name = T.replace "_" " " rawName
  return ExportedContractDecl
    { ecdName = name
    , ecdVar = varName
    }
