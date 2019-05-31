-- | Parsing of untyped Michelson values.

module Michelson.Parser.Value
  ( value'
  , mkLetVal

  -- * For tests
  , stringLiteral
  , bytesLiteral
  , intLiteral
  ) where

import Prelude hiding (many, note, some, try)

import qualified Data.ByteString.Base16 as B16
import qualified Data.Char as Char
import qualified Data.Map as Map

import Text.Megaparsec (anySingle, choice, customFailure, manyTill, satisfy, takeWhileP, try)
import Text.Megaparsec.Char (char, string)
import qualified Text.Megaparsec.Char.Lexer as L

import Michelson.Let (LetValue(..))
import Michelson.Macro (ParsedOp, ParsedValue)
import Michelson.Parser.Error
import Michelson.Parser.Helpers
import Michelson.Parser.Lexer
import Michelson.Parser.Types (Parser, letValues)
import Michelson.Text (isMChar, mkMTextUnsafe)
import qualified Michelson.Untyped as U

-- | Parse untyped 'ParsedValue'. Take instruction parser as argument
-- to avoid cyclic dependencies between modules, hence ' in its name.
value' :: Parser ParsedOp -> Parser ParsedValue
value' opParser = lexeme $ valueInner' <|> parens valueInner'
  where
    valueInner' = valueInner opParser

valueInner :: Parser ParsedOp -> Parser ParsedValue
valueInner opParser = choice $
  [ stringLiteral, bytesLiteral, intLiteral, unitValue
  , trueValue, falseValue, pairValue opParser, leftValue opParser
  , rightValue opParser, someValue opParser, noneValue, nilValue
  , seqValue opParser, mapValue opParser, lambdaValue opParser, dataLetValue
  ]

stringLiteral :: Parser ParsedValue
stringLiteral = U.ValueString . mkMTextUnsafe . toText <$> do
  _ <- try $ string "\""
  manyTill validChar (string "\"")
  where
      validChar :: Parser Char
      validChar = choice
        [ strEscape
        , satisfy (\x -> x /= '"' && isMChar x)
        , anySingle >>= stringLiteralFailure . InvalidChar
        ]

      strEscape :: Parser Char
      strEscape = try (char '\\') >> esc
        where
          esc = choice
            [ char '\\'
            , char '"'
            , char 'n' $> '\n'
            , anySingle >>= stringLiteralFailure . InvalidEscapeSequence
            ]
      stringLiteralFailure = customFailure . StringLiteralException

-- It is safe not to use `try` here because bytesLiteral is the only
-- thing that starts from 0x (at least for now)
bytesLiteral :: Parser (U.Value' op)
bytesLiteral = do
  symbol "0x"
  hexdigits <- takeWhileP Nothing Char.isHexDigit
  let (bytes, remain) = B16.decode $ encodeUtf8 hexdigits
  if remain == ""
  then return . U.ValueBytes . U.InternalByteString $ bytes
  else customFailure OddNumberBytesException

intLiteral :: Parser (U.Value' op)
intLiteral = try $ U.ValueInt <$> L.signed pass L.decimal

unitValue :: Parser ParsedValue
unitValue = do symbol "Unit"; return U.ValueUnit

trueValue :: Parser ParsedValue
trueValue = do symbol "True"; return U.ValueTrue

falseValue :: Parser ParsedValue
falseValue = do symbol "False"; return U.ValueFalse

pairValue :: Parser ParsedOp -> Parser ParsedValue
pairValue opParser = core <|> tuple
  where
    core = do
      void $ symbol "Pair"
      a <- value' opParser
      U.ValuePair a <$> value' opParser
    tuple = try $ do
      symbol "("
      ty <- tupleInner
      symbol ")"
      return ty
    tupleInner = try $ do
      a <- value' opParser
      comma
      b <- tupleInner <|> value' opParser
      return $ U.ValuePair a b

leftValue :: Parser ParsedOp -> Parser ParsedValue
leftValue opParser = do void $ symbol "Left"; U.ValueLeft <$> value' opParser

rightValue :: Parser ParsedOp -> Parser ParsedValue
rightValue opParser = do void $ symbol "Right"; U.ValueRight <$> value' opParser

someValue :: Parser ParsedOp -> Parser ParsedValue
someValue opParser = do void $ symbol "Some"; U.ValueSome <$> value' opParser

noneValue :: Parser ParsedValue
noneValue = U.ValueNone <$ symbol "None"

nilValue :: Parser ParsedValue
nilValue = U.ValueNil <$ (try $ braces pass)

lambdaValue :: Parser ParsedOp -> Parser ParsedValue
lambdaValue opParser = U.ValueLambda <$> ops1
  where
    ops1 :: Parser (NonEmpty ParsedOp)
    ops1 = braces $ sepEndBy1 opParser semicolon

seqValue :: Parser ParsedOp -> Parser ParsedValue
seqValue opParser =
  U.ValueSeq <$> (try $ braces $ sepEndBy1 (value' opParser) semicolon)

eltValue :: Parser ParsedOp -> Parser (U.Elt ParsedOp)
eltValue opParser = do
  void $ symbol "Elt"
  U.Elt <$> value' opParser <*> value' opParser

mapValue :: Parser ParsedOp -> Parser ParsedValue
mapValue opParser = U.ValueMap <$> (try $ braces $ sepEndBy1 (eltValue opParser) semicolon)

dataLetValue :: Parser ParsedValue
dataLetValue = do
  lvs <- asks letValues
  lvVal <$> (mkLetVal lvs)

mkLetVal :: Map Text LetValue -> Parser LetValue
mkLetVal lvs = choice $ mkParser lvName <$> Map.elems lvs
