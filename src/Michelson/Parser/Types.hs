-- | Core parser types

module Michelson.Parser.Types
  ( Parser
  , LetEnv (..)
  , noLetEnv
  ) where

import Data.Default (Default(..))
import qualified Data.Map as Map
import Text.Megaparsec (Parsec)

import Michelson.Parser.Error
import Michelson.Types (LetMacro, LetType, LetValue)

type Parser = ReaderT LetEnv (Parsec CustomParserException Text)

instance Default a => Default (Parser a) where
  def = pure def

-- | The environment containing lets from the let-block
data LetEnv = LetEnv
  { letMacros :: Map Text LetMacro
  , letValues :: Map Text LetValue
  , letTypes  :: Map Text LetType
  } deriving (Show, Eq)

noLetEnv :: LetEnv
noLetEnv = LetEnv Map.empty Map.empty Map.empty
