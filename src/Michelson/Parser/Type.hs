-- | Parsing of Michelson types.

module Michelson.Parser.Type
  ( type_
  , comparable
  ) where

import Prelude hiding (many, note, some, try)

import Data.Default (Default)
import qualified Data.Map as Map
import Text.Megaparsec (choice, customFailure, try)

import Michelson.Parser.Annotations
import Michelson.Parser.Error
import Michelson.Parser.Helpers
import Michelson.Parser.Lexer
import Michelson.Parser.Types (Parser, letTypes)
import Michelson.Types (LetType(..))
import Michelson.Untyped

-- | Parse untyped Michelson 'Type` (i. e. one with annotations).
type_ :: Parser Type
type_ = (ti <|> parens ti) <|> (customFailure UnknownTypeException)
  where
    ti = snd <$> (lexeme $ typeInner (pure noAnn))

typeInner :: Parser FieldAnn -> Parser (FieldAnn, Type)
typeInner fp = choice $ (\x -> x fp) <$>
  [ t_ct, t_key, t_unit, t_signature, t_option, t_list, t_set, t_operation
  , t_contract, t_pair, t_or, t_lambda, t_map, t_big_map, t_view, t_void
  , t_letType
  ]

----------------------------------------------------------------------------
-- Comparable types
----------------------------------------------------------------------------

comparable :: Parser Comparable
comparable = let c = do ct' <- ct; Comparable ct' <$> noteTDef in parens c <|> c

t_ct :: (Default a) => Parser a -> Parser (a, Type)
t_ct fp = do ct' <- ct; (f,t) <- fieldType fp; return (f, Type (Tc ct') t)

ct :: Parser CT
ct =  (symbol' "Int" >> return CInt)
  <|> (symbol' "Nat" >> return CNat)
  <|> (symbol' "String" >> return CString)
  <|> (symbol' "Bytes" >> return CBytes)
  <|> (symbol' "Mutez" >> return CMutez)
  <|> (symbol' "Bool" >> return CBool)
  <|> ((symbol' "KeyHash" <|> symbol "key_hash") >> return CKeyHash)
  <|> (symbol' "Timestamp" >> return CTimestamp)
  <|> (symbol' "Address" >> return CAddress)

----------------------------------------------------------------------------
-- Non-comparable types
----------------------------------------------------------------------------

field :: Parser (FieldAnn, Type)
field = lexeme (fi <|> parens fi)
  where
    fi = typeInner noteF

t_key :: (Default a) => Parser a -> Parser (a, Type)
t_key fp = do symbol' "Key"; (f,t) <- fieldType fp; return (f, Type TKey t)

t_signature :: (Default a) => Parser a -> Parser (a, Type)
t_signature fp = do symbol' "Signature"; (f, t) <- fieldType fp; return (f, Type TSignature t)

t_operation :: (Default a) => Parser a -> Parser (a, Type)
t_operation fp = do symbol' "Operation"; (f, t) <- fieldType fp; return (f, Type TOperation t)

t_contract :: (Default a) => Parser a -> Parser (a, Type)
t_contract  fp = do
  symbol' "Contract"
  (f, t) <- fieldType fp
  a <- type_
  return (f, Type (TContract a) t)

t_unit :: (Default a) => Parser a -> Parser (a, Type)
t_unit fp = do
  symbol' "Unit" <|> symbol "()"
  (f,t) <- fieldType fp
  return (f, Type TUnit t)

t_pair :: (Default a) => Parser a -> Parser (a, Type)
t_pair fp = core <|> tuple
  where
    core = do
      symbol' "Pair"
      (f, t) <- fieldType fp
      (l, a) <- field
      (r, b) <- field
      return (f, Type (TPair l r a b) t)
    tuple = try $ do
      symbol "("
      (l, a) <- field
      comma
      (r, b) <- tupleInner <|> field
      symbol ")"
      (f, t) <- fieldType fp
      return (f, Type (TPair l r a b) t)
    tupleInner = try $ do
      (l, a) <- field
      comma
      (r, b) <- tupleInner <|> field
      return (noAnn, Type (TPair l r a b) noAnn)

t_or :: (Default a) => Parser a -> Parser (a, Type)
t_or fp = core <|> bar
  where
    core = do
      symbol' "Or"
      (f, t) <- fieldType fp
      (l, a) <- field
      (r, b) <- field
      return (f, Type (TOr l r a b) t)
    bar = try $ do
      symbol "("
      (l, a) <- field
      symbol "|"
      (r, b) <- barInner <|> field
      symbol ")"
      (f, t) <- fieldType fp
      return (f, Type (TOr l r a b) t)
    barInner = try $ do
      (l, a) <- field
      symbol "|"
      (r, b) <- barInner <|> field
      return (noAnn, Type (TOr l r a b) noAnn)

t_option :: (Default a) => Parser a -> Parser (a, Type)
t_option fp = do
  symbol' "Option"
  (f, t) <- fieldType fp
  (fa, a) <- field
  return (f, Type (TOption fa a) t)

t_lambda :: (Default a) => Parser a -> Parser (a, Type)
t_lambda fp = core <|> slashLambda
  where
    core = do
      symbol' "Lambda"
      (f, t) <- fieldType fp
      a <- type_
      b <- type_
      return (f, Type (TLambda a b) t)
    slashLambda = do
      symbol "\\"
      (f, t) <- fieldType fp
      a <- type_
      symbol "->"
      b <- type_
      return (f, Type (TLambda a b) t)

-- Container types
t_list :: (Default a) => Parser a -> Parser (a, Type)
t_list fp = core <|> bracketList
  where
    core = do
      symbol' "List"
      (f, t) <- fieldType fp
      a <- type_
      return (f, Type (TList a) t)
    bracketList = do
      a <- brackets type_
      (f, t) <- fieldType fp
      return (f, Type (TList a) t)

t_set :: (Default a) => Parser a -> Parser (a, Type)
t_set fp = core <|> braceSet
  where
    core = do
      symbol' "Set"
      (f, t) <- fieldType fp
      a <- comparable
      return (f, Type (TSet a) t)
    braceSet = do
      a <- braces comparable
      (f, t) <- fieldType fp
      return (f, Type (TSet a) t)

t_map :: (Default a) => Parser a -> Parser (a, Type)
t_map fp = do
  symbol' "Map"
  (f, t) <- fieldType fp
  a <- comparable
  b <- type_
  return (f, Type (TMap a b) t)

t_big_map :: (Default a) => Parser a -> Parser (a, Type)
t_big_map fp = do
  symbol' "BigMap" <|> symbol "big_map"
  (f, t) <- fieldType fp
  a <- comparable
  b <- type_
  return (f, Type (TBigMap a b) t)

----------------------------------------------------------------------------
-- Non-standard types (Morley extensions)
----------------------------------------------------------------------------

t_view :: Default a => Parser a -> Parser (a, Type)
t_view fp = do
  symbol' "View"
  a <- type_
  r <- type_
  (f, t) <- fieldType fp
  let r' = Type (TOption noAnn r) noAnn
  let c = Type (TPair noAnn noAnn a r') noAnn
  let c' = Type (TContract c) noAnn
  return (f, Type (TPair noAnn noAnn a c') t)

t_void :: Default a => Parser a -> Parser (a, Type)
t_void fp = do
  symbol' "Void"
  a <- type_
  b <- type_
  (f, t) <- fieldType fp
  let c = Type (TLambda b b) noAnn
  return (f, Type (TPair noAnn noAnn a c) t)

t_letType :: Default fp => Parser fp -> Parser (fp, Type)
t_letType fp = do
  lts <- asks letTypes
  lt <- ltSig <$> (mkLetType lts)
  f <- parseDef fp
  return (f, lt)

mkLetType :: Map Text LetType -> Parser LetType
mkLetType lts = choice $ mkParser ltName <$> (Map.elems lts)
