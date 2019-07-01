-- | Parsing of Michelson types.

module Michelson.Parser.Type
  ( type_
  , explicitType
  , comparable
  ) where

import Prelude hiding (many, note, some, try)

import Data.Default (Default)
import qualified Data.Map as Map
import Text.Megaparsec (choice, customFailure, try)

import Michelson.Let (LetType(..))
import Michelson.Parser.Annotations
import Michelson.Parser.Error
import Michelson.Parser.Helpers
import Michelson.Parser.Lexer
import Michelson.Parser.Types (Parser, letTypes)
import Michelson.Untyped
import Util.Generic

-- | Parse untyped Michelson 'Type` (i. e. one with annotations).
type_ :: Parser Type
type_ = typeHelper implicitTypes

-- | Parse only explicit `Type`, `Parameter` and `Storage` are prohibited
explicitType :: Parser Type
explicitType = typeHelper empty

typeHelper :: Parser Type -> Parser Type
typeHelper implicitParser = ti <|> parens ti <|> customFailure UnknownTypeException
  where
    ti = snd <$> (lexeme $ typeInner implicitParser (pure noAnn)) <|> implicitParser

typeInner
  :: Parser Type
  -> Parser FieldAnn -> Parser (FieldAnn, Type)
typeInner implicit fp = choice $ (\x -> x fp) <$>
  [ t_ct, t_key, t_unit, t_signature, t_option implicit, t_list implicit, t_set
  , t_operation, t_contract implicit, t_pair implicit, t_or implicit
  , t_lambda implicit, t_map implicit, t_big_map implicit, t_view implicit
  , t_void implicit, t_letType
  ]

implicitTypes :: Parser Type
implicitTypes = choice [t_parameter, t_storage]

----------------------------------------------------------------------------
-- Comparable types
----------------------------------------------------------------------------

comparable :: Parser Comparable
comparable = let c = do ct' <- ct; Comparable ct' <$> noteTDef in parens c <|> c

t_parameter :: Parser Type
t_parameter = do void $ symbol' "Parameter"; return TypeParameter

t_storage :: Parser Type
t_storage = do void $ symbol' "Storage"; return TypeStorage

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

field :: Parser Type -> Parser (FieldAnn, Type)
field implicit = lexeme (fi <|> parens fi)
  where
    fi = typeInner implicit noteF

t_key :: (Default a) => Parser a -> Parser (a, Type)
t_key fp = do symbol' "Key"; (f,t) <- fieldType fp; return (f, Type TKey t)

t_signature :: (Default a) => Parser a -> Parser (a, Type)
t_signature fp = do symbol' "Signature"; (f, t) <- fieldType fp; return (f, Type TSignature t)

t_operation :: (Default a) => Parser a -> Parser (a, Type)
t_operation fp = do symbol' "Operation"; (f, t) <- fieldType fp; return (f, Type TOperation t)

t_contract :: (Default a) => Parser Type -> Parser a -> Parser (a, Type)
t_contract implicit fp = do
  symbol' "Contract"
  (f, t) <- fieldType fp
  a <- typeHelper implicit
  return (f, Type (TContract a) t)

t_unit :: (Default a) => Parser a -> Parser (a, Type)
t_unit fp = do
  symbol' "Unit" <|> symbol "()"
  (f,t) <- fieldType fp
  return (f, Type TUnit t)

t_pair_like
  :: (Default a)
  => (FieldAnn -> FieldAnn -> Type -> Type -> T)
  -> Parser Type
  -> Parser a
  -> Parser (a, Type)
t_pair_like mkPair implicit fp = do
  (f, t) <- fieldType fp
  (l, a) <- implicitF
  (r, b) <- implicitF
  return (f, Type (mkPair l r a b) t)
  where
    implicitF = field implicit <|> (,) <$> noteFDef <*> implicit

t_pair :: (Default a) => Parser Type -> Parser a -> Parser (a, Type)
t_pair implicit fp = core <|> tuple
  where
    core = do
      symbol' "Pair"
      t_pair_like TPair implicit fp
    tuple = try $ do
      symbol "("
      (l, r, a, b) <- typePair
      symbol ")"
      (f, t) <- fieldType fp
      return (f, Type (TPair l r a b) t)
    tupleInner = try $ do
      (l, r, a, b) <- typePair
      return (noAnn, Type (TPair l r a b) noAnn)
    implicitF = field implicit <|> (,) <$> noteFDef <*> implicit
    typePair = do
      (l, a) <- implicitF
      comma
      (r, b) <- tupleInner <|> implicitF
      return (l, r, a, b)

t_or :: (Default a) => Parser Type -> Parser a -> Parser (a, Type)
t_or implicit fp = core <|> bar
  where
    core = do
      symbol' "Or"
      t_pair_like TOr implicit fp
    bar = try $ do
      symbol "("
      (_, Type ty _) <- barInner
      symbol ")"
      (f, t) <- fieldType fp
      return (f, Type ty t)
    barInner = do
      fs <- sepBy2 implicitF (symbol "|")
      let mergeTwo _ (l, a) (r, b) = (noAnn, Type (TOr l r a b) noAnn)
      return $ mkGenericTree mergeTwo fs
    implicitF = field implicit <|> (,) <$> noteFDef <*> implicit

t_option :: (Default a) => Parser Type -> Parser a -> Parser (a, Type)
t_option implicit fp = do
  symbol' "Option"
  (f, t) <- fieldType fp
  (fa, a) <- field implicit <|> (,) <$> noteFDef <*> implicit
  return (f, Type (TOption fa a) t)

t_lambda :: (Default a) => Parser Type -> Parser a -> Parser (a, Type)
t_lambda implicit fp = core <|> slashLambda
  where
    core = do
      symbol' "Lambda"
      (f, t) <- fieldType fp
      a <- implicitType
      b <- implicitType
      return (f, Type (TLambda a b) t)
    slashLambda = do
      symbol "\\"
      (f, t) <- fieldType fp
      a <- implicitType
      symbol "->"
      b <- implicitType
      return (f, Type (TLambda a b) t)
    implicitType = typeHelper implicit

-- Container types
t_list :: (Default a) => Parser Type -> Parser a -> Parser (a, Type)
t_list implicit fp = core <|> bracketList
  where
    core = do
      symbol' "List"
      (f, t) <- fieldType fp
      a <- typeHelper implicit
      return (f, Type (TList a) t)
    bracketList = do
      a <- brackets (typeHelper implicit)
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

t_map_like
  :: Default a
  => Parser Type -> Parser a -> Parser (Comparable, Type, a, TypeAnn)
t_map_like implicit fp = do
  (f, t) <- fieldType fp
  a <- comparable
  b <- typeHelper implicit
  return (a, b, f, t)

t_map :: (Default a) => Parser Type -> Parser a -> Parser (a, Type)
t_map implicit fp = do
  symbol' "Map"
  (a, b, f, t) <- t_map_like implicit fp
  return (f, Type (TMap a b) t)

t_big_map :: (Default a) => Parser Type -> Parser a -> Parser (a, Type)
t_big_map implicit fp = do
  symbol' "BigMap" <|> symbol "big_map"
  (a, b, f, t) <- t_map_like implicit fp
  return (f, Type (TBigMap a b) t)

----------------------------------------------------------------------------
-- Non-standard types (Morley extensions)
----------------------------------------------------------------------------

t_view :: Default a => Parser Type -> Parser a -> Parser (a, Type)
t_view implicit fp = do
  symbol' "View"
  a <- typeHelper implicit
  r <- typeHelper implicit
  (f, t) <- fieldType fp
  let c' = Type (TContract r) noAnn
  return (f, Type (TPair noAnn noAnn a c') t)

t_void :: Default a => Parser Type -> Parser a -> Parser (a, Type)
t_void implicit fp = do
  symbol' "Void"
  a <- typeHelper implicit
  b <- typeHelper implicit
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
