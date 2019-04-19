module Test.Util.Parser
  ( shouldParse
  ) where

import Test.HUnit.Base (assertFailure)
import Text.Megaparsec (errorBundlePretty)

import Michelson.Parser (Parser)
import qualified Michelson.Parser as Parser

-- | Expect the given text to be successfully parsed.
shouldParse :: Parser a -> Text -> IO a
shouldParse parser text =
  case Parser.parseNoEnv parser (toString text) text of
    Left err -> assertFailure (errorBundlePretty err)
    Right res -> return res
infix 2 `shouldParse`
