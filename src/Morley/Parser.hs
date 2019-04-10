module Morley.Parser
  ( program
  , parseNoEnv
  , codeEntry
  , ParserException (..)
  , stringLiteral
  , type_
  , value
  , stackType
  , printComment
  , bytesLiteral
  , pushOp
  , intLiteral
  ) where

import Prelude hiding (many, note, some, try)

import Control.Applicative.Permutations (intercalateEffect, toPermutation)
import qualified Data.ByteString.Base16 as B16
import qualified Data.Char as Char
import Data.Default (Default)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import Text.Megaparsec
  (choice, customFailure, eitherP, many, manyTill, notFollowedBy, parse, satisfy, sepEndBy, some,
  takeWhileP, try)
import Text.Megaparsec.Char (alphaNumChar, char, lowerChar, string, upperChar)
import qualified Text.Megaparsec.Char.Lexer as L

import Morley.Lexer
import qualified Morley.Macro as Macro
import Morley.Parser.Annotations
import Morley.Parser.Helpers
import Morley.Types (CustomParserException(..), ParsedOp(..), Parser, ParserException(..))
import qualified Morley.Types as Mo

-------------------------------------------------------------------------------
-- Top-Level Parsers
-------------------------------------------------------------------------------


-- Contracts
------------------

-- | Michelson contract with let definitions
program :: Mo.Parsec CustomParserException T.Text (Mo.Contract' ParsedOp)
program = runReaderT programInner Mo.noLetEnv

programInner :: Parser (Mo.Contract' ParsedOp)
programInner = do
  mSpace
  env <- fromMaybe Mo.noLetEnv <$> (optional letBlock)
  local (const env) contract

-- | Parse with empty environment
parseNoEnv :: Parser a -> String -> T.Text
       -> Either (Mo.ParseErrorBundle T.Text CustomParserException) a
parseNoEnv p = parse (runReaderT p Mo.noLetEnv)

-- | Michelson contract
contract :: Parser (Mo.Contract' ParsedOp)
contract = do
  mSpace
  (p,s,c) <- intercalateEffect semicolon $
              (,,) <$> toPermutation parameter
                   <*> toPermutation storage
                   <*> toPermutation code
  return $ Mo.Contract p s c

-- Contract Blocks
------------------

-- | let block parser
letBlock :: Parser Mo.LetEnv
letBlock = do
  symbol "let"
  symbol "{"
  ls <- local (const Mo.noLetEnv) letInner
  symbol "}"
  semicolon
  return ls

parameter :: Parser Mo.Type
parameter = do void $ symbol "parameter"; type_

storage :: Parser Mo.Type
storage = do void $ symbol "storage"; type_

code :: Parser [ParsedOp]
code = do void $ symbol "code"; codeEntry

-- Michelson expressions
------------------------
value :: Parser Mo.ParsedValue
value = lexeme $ valueInner <|> parens valueInner

type_ :: Parser Mo.Type
type_ = (ti <|> parens ti) <|> (customFailure UnknownTypeException)
  where
    ti = snd <$> (lexeme $ typeInner (pure Mo.noAnn))

-- | Parses code block after "code" keyword of a contract.
--
-- This function is part of the module API, its semantics should not change.
codeEntry :: Parser [ParsedOp]
codeEntry = ops

op' :: Parser Mo.ParsedOp
op' = do
  lms <- asks Mo.letMacros
  choice
    [ (Mo.Prim . Mo.EXT) <$> nopInstr
    , Mo.LMac <$> mkLetMac lms
    , Mo.Prim <$> prim
    , Mo.Mac <$> macro
    , primOrMac
    , Mo.Seq <$> ops
    ]

ops :: Parser [Mo.ParsedOp]
ops = braces $ sepEndBy op' semicolon

ops1 :: Parser (NonEmpty Mo.ParsedOp)
ops1 = braces $ sepEndBy1 op' semicolon

-------------------------------------------------------------------------------
-- Let block
-------------------------------------------------------------------------------

-- | Element of a let block
data Let = LetM Mo.LetMacro | LetV Mo.LetValue | LetT Mo.LetType

-- | Incrementally build the let environment
letInner :: Parser Mo.LetEnv
letInner = do
  env <- ask
  l <- lets
  semicolon
  (local (addLet l) letInner) <|> return (addLet l env)

-- | add a Let to the environment in the correct place
addLet :: Let -> Mo.LetEnv -> Mo.LetEnv
addLet l (Mo.LetEnv lms lvs lts) = case l of
  LetM lm -> Mo.LetEnv (Map.insert (Mo.lmName lm) lm lms) lvs lts
  LetV lv -> Mo.LetEnv lms (Map.insert (Mo.lvName lv) lv lvs) lts
  LetT lt -> Mo.LetEnv lms lvs (Map.insert (Mo.ltName lt) lt lts)

lets :: Parser Let
lets = choice [ (LetM <$> (try letMacro))
              , (LetV <$> (try letValue))
              , (LetT <$> (try letType))
              ]

-- | build a let name parser from a leading character parser
letName :: Parser Char -> Parser T.Text
letName p = lexeme $ do
  v <- p
  let validChar x = Char.isAscii x && (Char.isAlphaNum x || x == '\'' || x == '_')
  vs <- many (satisfy validChar)
  return $ T.pack (v:vs)

letMacro :: Parser Mo.LetMacro
letMacro = lexeme $ do
  n <- letName lowerChar
  symbol "::"
  s <- stackFn
  symbol "="
  o <- ops
  return $ Mo.LetMacro n s o

letType :: Parser Mo.LetType
letType = lexeme $ do
  symbol "type"
  n <- letName lowerChar
  symbol "="
  t <- type_
  case t of
    (Mo.Type t' a) ->
      if a == Mo.noAnn
      then return $ Mo.LetType n (Mo.Type t' (Mo.ann n))
      else return $ Mo.LetType n t

letValue :: Parser Mo.LetValue
letValue = lexeme $ do
  n <- letName upperChar
  symbol "::"
  t <- type_
  symbol "="
  v <- value
  return $ Mo.LetValue n t v

-- | make a parser from a string
mkParser :: (a -> T.Text) -> a -> Parser a
mkParser f a = (try $ symbol (f a)) >> return a

mkLetMac :: Map Text Mo.LetMacro -> Parser Mo.LetMacro
mkLetMac lms = choice $ mkParser Mo.lmName <$> (Map.elems lms)

mkLetVal :: Map Text Mo.LetValue -> Parser Mo.LetValue
mkLetVal lvs = choice $ mkParser Mo.lvName <$> (Map.elems lvs)

mkLetType :: Map Text Mo.LetType -> Parser Mo.LetType
mkLetType lts = choice $ mkParser Mo.ltName <$> (Map.elems lts)

stackFn :: Parser Mo.StackFn
stackFn = do
  vs <- (optional (symbol "forall" >> some varID <* symbol "."))
  a <- stackType
  symbol "->"
  b <- stackType
  return $ Mo.StackFn (Set.fromList <$> vs) a b

tyVar :: Parser Mo.TyVar
tyVar = (Mo.TyCon <$> type_) <|> (Mo.VarID <$> varID)

lowerAlphaNumChar :: Parser Char
lowerAlphaNumChar = satisfy (\x -> Char.isLower x || Char.isDigit x)

varID :: Parser Mo.Var
varID = lexeme $ do
  v <- lowerChar
  vs <- many lowerAlphaNumChar
  return $ Mo.Var (T.pack (v:vs))

-------------------------------------------------------------------------------
-- Value Parsers
-------------------------------------------------------------------------------

valueInner :: Parser Mo.ParsedValue
valueInner = choice $
  [ stringLiteral, bytesLiteral, intLiteral, unitValue
  , trueValue, falseValue, pairValue, leftValue, rightValue
  , someValue, noneValue, nilValue, seqValue, mapValue, lambdaValue
  , dataLetValue
  ]

dataLetValue :: Parser Mo.ParsedValue
dataLetValue = do
  lvs <- asks Mo.letValues
  Mo.lvVal <$> (mkLetVal lvs)

-- Literals
intLiteral :: Parser (Mo.Value' a)
intLiteral = try $ Mo.ValueInt <$> (L.signed (return ()) L.decimal)


-- It is safe not to use `try` here because bytesLiteral is the only
-- thing that starts from 0x (at least for now)
bytesLiteral :: Parser (Mo.Value' a)
bytesLiteral = do
  symbol "0x"
  hexdigits <- takeWhileP Nothing Char.isHexDigit
  let (bytes, remain) = B16.decode $ encodeUtf8 hexdigits
  if remain == ""
  then return . Mo.ValueBytes . Mo.InternalByteString $ bytes
  else customFailure OddNumberBytesException

stringLiteral :: Parser Mo.ParsedValue
stringLiteral = try $ Mo.ValueString <$>
  (T.pack <$>
    ( (++) <$>
        (concat <$> (string "\"" >> many validChar)) <*>
        (manyTill (lineBreakChar <|> (customFailure $ UnexpectedLineBreak)) (string "\""))
    )
  )
  where
      validChar :: Parser String
      validChar =
        try strEscape <|>
          try ((:[]) <$> satisfy (\x -> x /= '"' && x /= '\n' && x /= '\r'))
      lineBreakChar :: Parser Char
      lineBreakChar = char '\n' <|> char '\r'

strEscape :: Parser String
strEscape = char '\\' >> esc
  where
    esc = (char 't' >> return "\t")
      <|> (char 'b' >> return "\b")
      <|> (char '\\' >> return "\\")
      <|> (char '"' >> return "\"")
      <|> (char 'n' >> return "\n")
      <|> (char 'r' >> return "\r")


unitValue :: Parser Mo.ParsedValue
unitValue = do symbol "Unit"; return Mo.ValueUnit

trueValue :: Parser Mo.ParsedValue
trueValue = do symbol "True"; return Mo.ValueTrue

falseValue :: Parser Mo.ParsedValue
falseValue = do symbol "False"; return Mo.ValueFalse

pairValue :: Parser Mo.ParsedValue
pairValue = core <|> tuple
  where
    core = do symbol "Pair"; a <- value; Mo.ValuePair a <$> value
    tuple = try $ do
      symbol "("
      a <- value
      comma
      b <- tupleInner <|> value
      symbol ")"
      return $ Mo.ValuePair a b
    tupleInner = try $ do
      a <- value
      comma
      b <- tupleInner <|> value
      return $ Mo.ValuePair a b

leftValue :: Parser Mo.ParsedValue
leftValue = do void $ symbol "Left"; Mo.ValueLeft <$> value

rightValue :: Parser Mo.ParsedValue
rightValue = do void $ symbol "Right"; Mo.ValueRight <$> value

someValue :: Parser Mo.ParsedValue
someValue = do void $ symbol "Some"; Mo.ValueSome <$> value

noneValue :: Parser Mo.ParsedValue
noneValue = do symbol "None"; return Mo.ValueNone

nilValue :: Parser Mo.ParsedValue
nilValue = Mo.ValueNil <$ (try $ braces pass)

lambdaValue :: Parser Mo.ParsedValue
lambdaValue = Mo.ValueLambda <$> ops1

seqValue :: Parser Mo.ParsedValue
seqValue = Mo.ValueSeq <$> (try $ braces $ sepEndBy1 value semicolon)

eltValue :: Parser (Mo.Elt ParsedOp)
eltValue = do void $ symbol "Elt"; Mo.Elt <$> value <*> value

mapValue :: Parser Mo.ParsedValue
mapValue = Mo.ValueMap <$> (try $ braces $ sepEndBy1 eltValue semicolon)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
field :: Parser (Mo.FieldAnn, Mo.Type)
field = lexeme (fi <|> parens fi)
  where
    fi = typeInner noteF

typeInner :: Parser Mo.FieldAnn -> Parser (Mo.FieldAnn, Mo.Type)
typeInner fp = choice $ (\x -> x fp) <$>
  [ t_ct, t_key, t_unit, t_signature, t_option, t_list, t_set, t_operation
  , t_contract, t_pair, t_or, t_lambda, t_map, t_big_map, t_letType
  ]

t_letType :: Parser fp -> Parser (fp, Mo.Type)
t_letType fp = do
  lts <- asks Mo.letTypes
  lt <- Mo.ltSig <$> (mkLetType lts)
  f <- fp
  return (f, lt)

-- Comparable Types
comparable :: Parser Mo.Comparable
comparable = let c = do ct' <- ct; Mo.Comparable ct' <$> noteTDef in parens c <|> c

t_ct :: (Default a) => Parser a -> Parser (a, Mo.Type)
t_ct fp = do ct' <- ct; (f,t) <- fieldType fp; return (f, Mo.Type (Mo.Tc ct') t)

ct :: Parser Mo.CT
ct = (symbol "int" >> return Mo.CInt)
  <|> (symbol "nat" >> return Mo.CNat)
  <|> (symbol "string" >> return Mo.CString)
  <|> (symbol "bytes" >> return Mo.CBytes)
  <|> (symbol "mutez" >> return Mo.CMutez)
  <|> (symbol "bool" >> return Mo.CBool)
  <|> (symbol "key_hash" >> return Mo.CKeyHash)
  <|> (symbol "timestamp" >> return Mo.CTimestamp)
  <|> (symbol "address" >> return Mo.CAddress)

-- Protocol Types
t_key :: (Default a) => Parser a -> Parser (a, Mo.Type)
t_key       fp = do symbol "key"; (f,t) <- fieldType fp; return (f, Mo.Type Mo.TKey t)

t_signature :: (Default a) => Parser a -> Parser (a, Mo.Type)
t_signature fp = do symbol "signature"; (f, t) <- fieldType fp; return (f, Mo.Type Mo.TSignature t)

t_operation :: (Default a) => Parser a -> Parser (a, Mo.Type)
t_operation fp = do symbol "operation"; (f, t) <- fieldType fp; return (f, Mo.Type Mo.TOperation t)

t_contract :: (Default a) => Parser a -> Parser (a, Mo.Type)
t_contract  fp = do symbol "contract"; (f, t) <- fieldType fp; a <- type_; return (f, Mo.Type (Mo.TContract a) t)
--(do symbol "address"; (f, t) <- ft; return (f, Mo.Type Mo.CAddress t)

-- Abstraction Types
t_unit :: (Default a) => Parser a -> Parser (a, Mo.Type)
t_unit fp = do
  symbol "unit" <|> symbol "()"
  (f,t) <- fieldType fp
  return (f, Mo.Type Mo.TUnit t)

t_pair :: (Default a) => Parser a -> Parser (a, Mo.Type)
t_pair fp = core <|> tuple
  where
    core = do
      symbol "pair"
      (f, t) <- fieldType fp
      (l, a) <- field
      (r, b) <- field
      return (f, Mo.Type (Mo.TPair l r a b) t)
    tuple = try $ do
      symbol "("
      (l, a) <- field
      comma
      (r, b) <- tupleInner <|> field
      symbol ")"
      (f, t) <- fieldType fp
      return (f, Mo.Type (Mo.TPair l r a b) t)
    tupleInner = try $ do
      (l, a) <- field
      comma
      (r, b) <- tupleInner <|> field
      return (Mo.noAnn, Mo.Type (Mo.TPair l r a b) Mo.noAnn)

t_or :: (Default a) => Parser a -> Parser (a, Mo.Type)
t_or fp = core <|> bar
  where
    core = do
      symbol "or"
      (f, t) <- fieldType fp
      (l, a) <- field
      (r, b) <- field
      return (f, Mo.Type (Mo.TOr l r a b) t)
    bar = try $ do
      symbol "("
      (l, a) <- field
      symbol "|"
      (r, b) <- barInner <|> field
      symbol ")"
      (f, t) <- fieldType fp
      return (f, Mo.Type (Mo.TOr l r a b) t)
    barInner = try $ do
      (l, a) <- field
      symbol "|"
      (r, b) <- barInner <|> field
      return (Mo.noAnn, Mo.Type (Mo.TOr l r a b) Mo.noAnn)

t_option :: (Default a) => Parser a -> Parser (a, Mo.Type)
t_option fp = do
  symbol "option"
  (f, t) <- fieldType fp
  (fa, a) <- field
  return (f, Mo.Type (Mo.TOption fa a) t)

t_lambda :: (Default a) => Parser a -> Parser (a, Mo.Type)
t_lambda fp = core <|> slashLambda
  where
    core = do
      symbol "lambda"
      (f, t) <- fieldType fp
      a <- type_
      b <- type_
      return (f, Mo.Type (Mo.TLambda a b) t)
    slashLambda = do
      symbol "\\"
      (f, t) <- fieldType fp
      a <- type_
      symbol "->"
      b <- type_
      return (f, Mo.Type (Mo.TLambda a b) t)

-- Container types
t_list :: (Default a) => Parser a -> Parser (a, Mo.Type)
t_list fp = core <|> bracketList
  where
    core = do
      symbol "list"
      (f, t) <- fieldType fp
      a <- type_
      return (f, Mo.Type (Mo.TList a) t)
    bracketList = do
      a <- brackets type_
      (f, t) <- fieldType fp
      return (f, Mo.Type (Mo.TList a) t)

t_set :: (Default a) => Parser a -> Parser (a, Mo.Type)
t_set fp = core <|> braceSet
  where
    core = do
      symbol "set"
      (f, t) <- fieldType fp
      a <- comparable
      return (f, Mo.Type (Mo.TSet a) t)
    braceSet = do
      a <- braces comparable
      (f, t) <- fieldType fp
      return (f, Mo.Type (Mo.TSet a) t)

t_map :: (Default a) => Parser a -> Parser (a, Mo.Type)
t_map fp = (do symbol "map"; (f, t) <- fieldType fp; a <- comparable; b <- type_; return (f, Mo.Type (Mo.TMap a b) t))

t_big_map :: (Default a) => Parser a -> Parser (a, Mo.Type)
t_big_map fp = (do symbol "big_map"; (f, t) <- fieldType fp; a <- comparable; b <- type_; return (f, Mo.Type (Mo.TBigMap a b) t))

-------------------------------------------------------------------------------
-- Primitive Instruction Parsers
-------------------------------------------------------------------------------
prim :: Parser Mo.ParsedInstr
prim = choice
  [ dropOp, dupOp, swapOp, pushOp, someOp, noneOp, unitOp, ifNoneOp
  , carOp, cdrOp, leftOp, rightOp, ifLeftOp, nilOp, consOp, ifConsOp
  , sizeOp, emptySetOp, emptyMapOp, iterOp, memOp, getOp, updateOp
  , loopLOp, loopOp, lambdaOp, execOp, dipOp, failWithOp, castOp, renameOp
  , concatOp, packOp, unpackOp, sliceOp, isNatOp, addressOp, addOp, subOp
  , mulOp, edivOp, absOp, negOp, lslOp, lsrOp, orOp, andOp, xorOp, notOp
  , compareOp, eqOp, neqOp, ltOp, leOp, gtOp, geOp, intOp, selfOp, contractOp
  , transferTokensOp, setDelegateOp, createAccountOp, createContractOp
  , implicitAccountOp, nowOp, amountOp, balanceOp, checkSigOp
  , sha256Op, sha512Op, blake2BOp, hashKeyOp, stepsToQuotaOp, sourceOp, senderOp
  ]

-- Control Structures

failWithOp :: Parser Mo.ParsedInstr
failWithOp = do symbol' "FAILWITH"; return Mo.FAILWITH

loopOp :: Parser Mo.ParsedInstr
loopOp  = do void $ symbol' "LOOP"; Mo.LOOP <$> ops

loopLOp :: Parser Mo.ParsedInstr
loopLOp = do void $ symbol' "LOOP_LEFT"; Mo.LOOP_LEFT <$> ops

execOp :: Parser Mo.ParsedInstr
execOp = do void $ symbol' "EXEC"; Mo.EXEC <$> noteVDef

dipOp :: Parser Mo.ParsedInstr
dipOp = do void $ symbol' "DIP"; Mo.DIP <$> ops

-- Stack Operations

dropOp :: Parser Mo.ParsedInstr
dropOp = do symbol' "DROP"; return Mo.DROP;

dupOp :: Parser Mo.ParsedInstr
dupOp = do void $ symbol' "DUP"; Mo.DUP <$> noteVDef

swapOp :: Parser Mo.ParsedInstr
swapOp = do symbol' "SWAP"; return Mo.SWAP;

pushOp :: Parser Mo.ParsedInstr
pushOp = do
  symbol' "PUSH"
  v <- noteVDef
  (try $ pushLet v) <|> (push' v)
  where
    pushLet v = do
      lvs <- asks Mo.letValues
      lv <- mkLetVal lvs
      return $ Mo.PUSH v (Mo.lvSig lv) (Mo.lvVal lv)
    push' v = do a <- type_; Mo.PUSH v a <$> value

unitOp :: Parser Mo.ParsedInstr
unitOp = do symbol' "UNIT"; (t, v) <- notesTV; return $ Mo.UNIT t v

lambdaOp :: Parser Mo.ParsedInstr
lambdaOp = do symbol' "LAMBDA"; v <- noteVDef; a <- type_; b <- type_;
              Mo.LAMBDA v a b <$> ops

-- Generic comparison

eqOp :: Parser Mo.ParsedInstr
eqOp = do void $ symbol' "EQ"; Mo.EQ <$> noteVDef

neqOp :: Parser Mo.ParsedInstr
neqOp = do void $ symbol' "NEQ"; Mo.NEQ <$> noteVDef

ltOp :: Parser Mo.ParsedInstr
ltOp = do void $ symbol' "LT"; Mo.LT <$> noteVDef

gtOp :: Parser Mo.ParsedInstr
gtOp = do void $ symbol' "GT"; Mo.GT <$> noteVDef

leOp :: Parser Mo.ParsedInstr
leOp = do void $ symbol' "LE"; Mo.LE <$> noteVDef

geOp :: Parser Mo.ParsedInstr
geOp = do void $ symbol' "GE"; Mo.GE <$> noteVDef

-- ad-hoc comparison

compareOp :: Parser Mo.ParsedInstr
compareOp = do void $ symbol' "COMPARE"; Mo.COMPARE <$> noteVDef

-- Operations on booleans

orOp :: Parser Mo.ParsedInstr
orOp = do void $ symbol' "OR";  Mo.OR <$> noteVDef

andOp :: Parser Mo.ParsedInstr
andOp = do void $ symbol' "AND"; Mo.AND <$> noteVDef

xorOp :: Parser Mo.ParsedInstr
xorOp = do void $ symbol' "XOR"; Mo.XOR <$> noteVDef

notOp :: Parser Mo.ParsedInstr
notOp = do void $ symbol' "NOT"; Mo.NOT <$> noteVDef

-- Operations on integers and natural numbers

addOp :: Parser Mo.ParsedInstr
addOp = do void $ symbol' "ADD"; Mo.ADD <$> noteVDef

subOp :: Parser Mo.ParsedInstr
subOp = do void $ symbol' "SUB"; Mo.SUB <$> noteVDef

mulOp :: Parser Mo.ParsedInstr
mulOp = do void $ symbol' "MUL"; Mo.MUL <$> noteVDef

edivOp :: Parser Mo.ParsedInstr
edivOp = do void $ symbol' "EDIV";Mo.EDIV <$> noteVDef

absOp :: Parser Mo.ParsedInstr
absOp = do void $ symbol' "ABS"; Mo.ABS <$> noteVDef

negOp :: Parser Mo.ParsedInstr
negOp = do symbol' "NEG"; return Mo.NEG;

-- Bitwise logical operators

lslOp :: Parser Mo.ParsedInstr
lslOp = do void $ symbol' "LSL"; Mo.LSL <$> noteVDef

lsrOp :: Parser Mo.ParsedInstr
lsrOp = do void $ symbol' "LSR"; Mo.LSR <$> noteVDef

-- Operations on string's

concatOp :: Parser Mo.ParsedInstr
concatOp = do void $ symbol' "CONCAT"; Mo.CONCAT <$> noteVDef

sliceOp :: Parser Mo.ParsedInstr
sliceOp = do void $ symbol' "SLICE"; Mo.SLICE <$> noteVDef

-- Operations on pairs
pairOp :: Parser Mo.ParsedInstr
pairOp = do symbol' "PAIR"; (t, v, (p, q)) <- notesTVF2; return $ Mo.PAIR t v p q

carOp :: Parser Mo.ParsedInstr
carOp = do symbol' "CAR"; (v, f) <- notesVF; return $ Mo.CAR v f

cdrOp :: Parser Mo.ParsedInstr
cdrOp = do symbol' "CDR"; (v, f) <- notesVF; return $ Mo.CDR v f

-- Operations on collections (sets, maps, lists)

emptySetOp :: Parser Mo.ParsedInstr
emptySetOp = do symbol' "EMPTY_SET"; (t, v) <- notesTV;
                Mo.EMPTY_SET t v <$> comparable

emptyMapOp :: Parser Mo.ParsedInstr
emptyMapOp = do symbol' "EMPTY_MAP"; (t, v) <- notesTV; a <- comparable;
                Mo.EMPTY_MAP t v a <$> type_

memOp :: Parser Mo.ParsedInstr
memOp = do void $ symbol' "MEM"; Mo.MEM <$> noteVDef

updateOp :: Parser Mo.ParsedInstr
updateOp = do symbol' "UPDATE"; return Mo.UPDATE

iterOp :: Parser Mo.ParsedInstr
iterOp = do void $ symbol' "ITER"; Mo.ITER <$> ops

sizeOp :: Parser Mo.ParsedInstr
sizeOp = do void $ symbol' "SIZE"; Mo.SIZE <$> noteVDef

mapOp :: Parser Mo.ParsedInstr
mapOp = do symbol' "MAP"; v <- noteVDef; Mo.MAP v <$> ops

getOp :: Parser Mo.ParsedInstr
getOp = do void $ symbol' "GET"; Mo.GET <$> noteVDef

nilOp :: Parser Mo.ParsedInstr
nilOp = do symbol' "NIL"; (t, v) <- notesTV; Mo.NIL t v <$> type_

consOp :: Parser Mo.ParsedInstr
consOp = do void $ symbol' "CONS"; Mo.CONS <$> noteVDef

ifConsOp :: Parser Mo.ParsedInstr
ifConsOp = do symbol' "IF_CONS"; a <- ops; Mo.IF_CONS a <$> ops

-- Operations on options

someOp :: Parser Mo.ParsedInstr
someOp = do symbol' "SOME"; (t, v, f) <- notesTVF; return $ Mo.SOME t v f

noneOp :: Parser Mo.ParsedInstr
noneOp = do symbol' "NONE"; (t, v, f) <- notesTVF; Mo.NONE t v f <$> type_

ifNoneOp :: Parser Mo.ParsedInstr
ifNoneOp = do symbol' "IF_NONE"; a <- ops; Mo.IF_NONE a <$> ops

-- Operations on unions

leftOp :: Parser Mo.ParsedInstr
leftOp = do symbol' "LEFT"; (t, v, (f, f')) <- notesTVF2;
               Mo.LEFT t v f f' <$> type_

rightOp :: Parser Mo.ParsedInstr
rightOp = do symbol' "RIGHT"; (t, v, (f, f')) <- notesTVF2;
               Mo.RIGHT t v f f' <$> type_

ifLeftOp :: Parser Mo.ParsedInstr
ifLeftOp = do symbol' "IF_LEFT"; a <- ops; Mo.IF_LEFT a <$> ops

-- Operations on contracts

createContractOp :: Parser Mo.ParsedInstr
createContractOp = do symbol' "CREATE_CONTRACT"; v <- noteVDef; v' <- noteVDef;
                       Mo.CREATE_CONTRACT v v' <$> braces contract

createAccountOp :: Parser Mo.ParsedInstr
createAccountOp = do symbol' "CREATE_ACCOUNT"; v <- noteVDef; v' <- noteVDef;
                       return $ Mo.CREATE_ACCOUNT v v'

transferTokensOp :: Parser Mo.ParsedInstr
transferTokensOp = do void $ symbol' "TRANSFER_TOKENS"; Mo.TRANSFER_TOKENS <$> noteVDef

setDelegateOp :: Parser Mo.ParsedInstr
setDelegateOp = do void $ symbol' "SET_DELEGATE"; Mo.SET_DELEGATE <$> noteVDef

balanceOp :: Parser Mo.ParsedInstr
balanceOp = do void $ symbol' "BALANCE"; Mo.BALANCE <$> noteVDef

contractOp :: Parser Mo.ParsedInstr
contractOp = do void $ symbol' "CONTRACT"; Mo.CONTRACT <$> noteVDef <*> type_

sourceOp :: Parser Mo.ParsedInstr
sourceOp = do void $ symbol' "SOURCE"; Mo.SOURCE <$> noteVDef

senderOp :: Parser Mo.ParsedInstr
senderOp = do void $ symbol' "SENDER"; Mo.SENDER <$> noteVDef

amountOp :: Parser Mo.ParsedInstr
amountOp = do void $ symbol' "AMOUNT"; Mo.AMOUNT <$> noteVDef

implicitAccountOp :: Parser Mo.ParsedInstr
implicitAccountOp = do void $ symbol' "IMPLICIT_ACCOUNT"; Mo.IMPLICIT_ACCOUNT <$> noteVDef

selfOp :: Parser Mo.ParsedInstr
selfOp = do void $ symbol' "SELF"; Mo.SELF <$> noteVDef

addressOp :: Parser Mo.ParsedInstr
addressOp = do void $ symbol' "ADDRESS"; Mo.ADDRESS <$> noteVDef

-- Special Operations
nowOp :: Parser Mo.ParsedInstr
nowOp = do void $ symbol' "NOW"; Mo.NOW <$> noteVDef

stepsToQuotaOp :: Parser Mo.ParsedInstr
stepsToQuotaOp = do void $ symbol' "STEPS_TO_QUOTA"; Mo.STEPS_TO_QUOTA <$> noteVDef

-- Operations on bytes
packOp :: Parser Mo.ParsedInstr
packOp = do void $ symbol' "PACK"; Mo.PACK <$> noteVDef

unpackOp :: Parser Mo.ParsedInstr
unpackOp = do symbol' "UNPACK"; v <- noteVDef; Mo.UNPACK v <$> type_

-- Cryptographic Primitives

checkSigOp :: Parser Mo.ParsedInstr
checkSigOp = do void $ symbol' "CHECK_SIGNATURE"; Mo.CHECK_SIGNATURE <$> noteVDef

blake2BOp :: Parser Mo.ParsedInstr
blake2BOp = do void $ symbol' "BLAKE2B"; Mo.BLAKE2B <$> noteVDef

sha256Op :: Parser Mo.ParsedInstr
sha256Op = do void $ symbol' "SHA256"; Mo.SHA256 <$> noteVDef

sha512Op :: Parser Mo.ParsedInstr
sha512Op = do void $ symbol' "SHA512"; Mo.SHA512 <$> noteVDef

hashKeyOp :: Parser Mo.ParsedInstr
hashKeyOp = do void $ symbol' "HASH_KEY"; Mo.HASH_KEY <$> noteVDef

{- Type operations -}
castOp :: Parser Mo.ParsedInstr
castOp = do void $ symbol' "CAST"; Mo.CAST <$> noteVDef <*> type_;

renameOp :: Parser Mo.ParsedInstr
renameOp = do void $ symbol' "RENAME"; Mo.RENAME <$> noteVDef

isNatOp :: Parser Mo.ParsedInstr
isNatOp = do void $ symbol' "ISNAT"; Mo.ISNAT <$> noteVDef

intOp :: Parser Mo.ParsedInstr
intOp = do void $ symbol' "INT"; Mo.INT <$> noteVDef

-------------------------------------------------------------------------------
-- Macro Parsers
-------------------------------------------------------------------------------
cmpOp :: Parser Mo.ParsedInstr
cmpOp = eqOp <|> neqOp <|> ltOp <|> gtOp <|> leOp <|> gtOp <|> geOp

macro :: Parser Mo.Macro
macro = do symbol' "CMP"; a <- cmpOp; Mo.CMP a <$> noteVDef
  <|> do void $ symbol' "IF_SOME"; Mo.IF_SOME <$> ops <*> ops
  <|> do void $ symbol' "IF_RIGHT"; Mo.IF_RIGHT <$> ops <*> ops
  <|> do symbol' "FAIL"; return Mo.FAIL
  <|> do void $ symbol' "ASSERT_CMP"; Mo.ASSERT_CMP <$> cmpOp
  <|> do symbol' "ASSERT_NONE"; return Mo.ASSERT_NONE
  <|> do symbol' "ASSERT_SOME"; return Mo.ASSERT_SOME
  <|> do symbol' "ASSERT_LEFT"; return Mo.ASSERT_LEFT
  <|> do symbol' "ASSERT_RIGHT"; return Mo.ASSERT_RIGHT
  <|> do void $ symbol' "ASSERT_"; Mo.ASSERTX <$> cmpOp
  <|> do symbol' "ASSERT"; return Mo.ASSERT
  <|> do string' "DI"; n <- num "I"; symbol' "P"; Mo.DIIP (n + 1) <$> ops
  <|> do string' "DU"; n <- num "U"; symbol' "P"; Mo.DUUP (n + 1) <$> noteVDef
  <|> unpairMac
  <|> cadrMac
  <|> setCadrMac
  where
   num str = fromIntegral . length <$> some (string' str)

pairMac :: Parser Mo.Macro
pairMac = do
  a <- pairMacInner
  symbol' "R"
  (tn, vn, fns) <- permute3Def noteTDef noteV (some noteF)
  let ps = Macro.mapLeaves ((Mo.noAnn,) <$> fns) a
  return $ Mo.PAPAIR ps tn vn

pairMacInner :: Parser Mo.PairStruct
pairMacInner = do
  string' "P"
  l <- (string' "A" >> return (Mo.F (Mo.noAnn, Mo.noAnn))) <|> pairMacInner
  r <- (string' "I" >> return (Mo.F (Mo.noAnn, Mo.noAnn))) <|> pairMacInner
  return $ Mo.P l r

unpairMac :: Parser Mo.Macro
unpairMac = do
  string' "UN"
  a <- pairMacInner
  symbol' "R"
  (vns, fns) <- permute2Def (some noteV) (some noteF)
  return $ Mo.UNPAIR (Macro.mapLeaves (zip vns fns) a)

cadrMac :: Parser Mo.Macro
cadrMac = lexeme $ do
  string' "C"
  a <- some $ try $ cadrInner <* notFollowedBy (string' "R")
  b <- cadrInner
  symbol' "R"
  (vn, fn) <- notesVF
  return $ Mo.CADR (a ++ pure b) vn fn

cadrInner :: Parser Mo.CadrStruct
cadrInner = (string' "A" >> return Mo.A) <|> (string' "D" >> return Mo.D)

setCadrMac :: Parser Mo.Macro
setCadrMac = do
  string' "SET_C"
  a <- some cadrInner
  symbol' "R"
  (v, f) <- notesVF
  return $ Mo.SET_CADR a v f

mapCadrMac :: Parser Mo.Macro
mapCadrMac = do
  string' "MAP_C"
  a <- some cadrInner
  symbol' "R"
  (v, f) <- notesVF
  Mo.MAP_CADR a v f <$> ops

ifCmpMac :: Parser Mo.Macro
ifCmpMac = symbol' "IFCMP" >> Mo.IFCMP <$> cmpOp <*> noteVDef <*> ops <*> ops

ifOrIfX :: Parser Mo.ParsedOp
ifOrIfX = do
  symbol' "IF"
  a <- eitherP cmpOp ops
  case a of
    Left cmp -> Mo.Mac <$> (Mo.IFX cmp <$> ops <*> ops)
    Right op -> Mo.Prim <$> (Mo.IF op <$> ops)

-- Some of the operations and macros have the same prefixes in their names
-- So this case should be handled separately
primOrMac :: Parser Mo.ParsedOp
primOrMac = ((Mo.Mac <$> ifCmpMac) <|> ifOrIfX)
  <|> ((Mo.Mac <$> mapCadrMac) <|> (Mo.Prim <$> mapOp))
  <|> (try (Mo.Prim <$> pairOp) <|> Mo.Mac <$> pairMac)

-------------------------------------------------------------------------------
-- Morley Instructions
-------------------------------------------------------------------------------

nopInstr :: Parser Mo.ParsedUExtInstr
nopInstr = choice [stackOp, testAssertOp, printOp]

stackOp :: Parser Mo.ParsedUExtInstr
stackOp = symbol' "STACKTYPE" >> Mo.STACKTYPE <$> stackType

testAssertOp :: Parser Mo.ParsedUExtInstr
testAssertOp = symbol' "TEST_ASSERT" >> Mo.UTEST_ASSERT <$> testAssert

printOp :: Parser Mo.ParsedUExtInstr
printOp = symbol' "PRINT" >> Mo.UPRINT <$> printComment

testAssert :: Parser Mo.ParsedUTestAssert
testAssert = do
  n <- lexeme (T.pack <$> some alphaNumChar)
  c <- printComment
  o <- ops
  return $ Mo.UTestAssert n c o

printComment :: Parser Mo.UPrintComment
printComment = do
  string "\""
  let validChar = T.pack <$> some (satisfy (\x -> x /= '%' && x /= '"'))
  c <- many (Right <$> stackRef <|> Left <$> validChar)
  symbol "\""
  return $ Mo.UPrintComment c

stackRef :: Parser Mo.UStackRef
stackRef = do
  string "%"
  n <- brackets' L.decimal
  return $ Mo.UStackRef n

stackType :: Parser Mo.StackTypePattern
stackType = symbol "'[" >> (emptyStk <|> stkCons <|> stkRest)
  where
    emptyStk = try $ symbol "]" >> return Mo.StkEmpty
    stkRest = try $ symbol "..." >> symbol "]" >> return Mo.StkRest
    stkCons = try $ do
      t <- tyVar
      s <- (symbol "," >> stkCons <|> stkRest) <|> emptyStk
      return $ Mo.StkCons t s
