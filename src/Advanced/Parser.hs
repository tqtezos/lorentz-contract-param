{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Advanced.Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative.Permutations (runPermutation, toPermutation)

import Advanced.Interpreter (Operation)
import Advanced.Type (CT(..), T(..), withSomeProxy, tProxy)
import Advanced.CValue (CVal (..), Address)
import Advanced.Value (Val (..), Instr (..), (#), SomeInstr(..), ContractT)

type Parser = Parsec Void Text

-- Lexing
lexeme :: Parser a -> Parser a
lexeme = L.lexeme mSpace
mSpace = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "/*" "*/")

symbol = L.symbol mSpace

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

semicolon = symbol ";"

type_ :: Parser T
type_ = lexeme (typeInner <|> parens typeInner)

parameter = do _ <- symbol "parameter"; type_ <* semicolon
storage   = do _ <- symbol "storage"; type_ <* semicolon

typeInner :: Parser T
typeInner = lexeme $
      do T_c <$> ct
  <|> do symbol "key"; pure T_key;
  <|> do symbol "unit"; pure T_unit;

ct :: Parser CT
ct = (symbol "int" >> return T_int)
  <|> (symbol "nat" >> return T_nat)
  <|> (symbol "string" >> return T_string)
  <|> (symbol "bytes" >> return T_bytes)
  <|> (symbol "mutez" >> return T_mutez)
  <|> (symbol "bool" >> return T_bool)
  <|> (symbol "key_hash" >> return T_key_hash)
  <|> (symbol "timestamp" >> return T_timestamp)
  <|> (symbol "address" >> return T_address)

contract :: forall op . Parser (SomeInstr op)
contract = do
  mSpace
  (p,s) <- runPermutation $
              (,) <$> toPermutation parameter
                  <*> toPermutation storage
  withSomeProxy (tProxy s) $ \(_ :: Proxy s) ->
    withSomeProxy (tProxy p) $ \(_ :: Proxy p) ->
      SomeInstr <$>
        (symbol "code" *>
        -- (parseInstr :: Parser (ContractT op s p))
        parseInstr @op @'[ 'T_pair p s ] @'[ s ]
          <* optional semicolon)

swapInstr = symbol "SWAP" $> SWAP;
cdrInstr  = symbol "CDR" $> CDR

class (Typeable inp, Typeable out) => ParseInstr op inp out where
  parseInstr :: Parser (Instr op inp out)

instance (Typeable p, Typeable s) => ParseInstr op '[ 'T_pair p s ] '[ s ] where
  parseInstr = symbol "CDR" $> CDR
