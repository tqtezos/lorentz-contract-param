module Michelson.Parser
  ( program
  , parseNoEnv
  , codeEntry
  , ParserException (..)
  , type_
  , value
  , stackType
  , printComment
  , pushOp

  -- * For tests
  , stringLiteral
  , bytesLiteral
  , intLiteral
  ) where

import Prelude hiding (many, note, some, try)

import Control.Applicative.Permutations (intercalateEffect, toPermutation)
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import Text.Megaparsec (choice, eitherP, many, notFollowedBy, parse, satisfy, sepEndBy, some, try)
import Text.Megaparsec.Char (alphaNumChar, lowerChar, string, upperChar)
import qualified Text.Megaparsec.Char.Lexer as L

import Michelson.Lexer
import qualified Michelson.Macro as Macro
import Michelson.Parser.Annotations
import Michelson.Parser.Helpers
import Michelson.Parser.Type
import Michelson.Parser.Value
import Michelson.Types (CustomParserException(..), ParsedOp(..), Parser, ParserException(..))
import qualified Michelson.Types as Mi
import qualified Michelson.Untyped as U
import Util.Alternative (someNE)

-------------------------------------------------------------------------------
-- Top-Level Parsers
-------------------------------------------------------------------------------


-- Contracts
------------------

-- | Michelson contract with let definitions
program :: Mi.Parsec CustomParserException T.Text (Mi.Contract' ParsedOp)
program = runReaderT programInner Mi.noLetEnv

programInner :: Parser (Mi.Contract' ParsedOp)
programInner = do
  mSpace
  env <- fromMaybe Mi.noLetEnv <$> (optional letBlock)
  local (const env) contract

-- | Parse with empty environment
parseNoEnv :: Parser a -> String -> T.Text
       -> Either (Mi.ParseErrorBundle T.Text CustomParserException) a
parseNoEnv p = parse (runReaderT p Mi.noLetEnv)

-- | Michelson contract
contract :: Parser (Mi.Contract' ParsedOp)
contract = do
  mSpace
  (p,s,c) <- intercalateEffect semicolon $
              (,,) <$> toPermutation parameter
                   <*> toPermutation storage
                   <*> toPermutation code
  return $ Mi.Contract p s c

-- Value
------------------

value :: Parser Mi.ParsedValue
value = value' op'

-- Contract Blocks
------------------

-- | let block parser
letBlock :: Parser Mi.LetEnv
letBlock = do
  symbol "let"
  symbol "{"
  ls <- local (const Mi.noLetEnv) letInner
  symbol "}"
  semicolon
  return ls

parameter :: Parser Mi.Type
parameter = do void $ symbol "parameter"; type_

storage :: Parser Mi.Type
storage = do void $ symbol "storage"; type_

code :: Parser [ParsedOp]
code = do void $ symbol "code"; codeEntry

-- Michelson expressions
------------------------
-- | Parses code block after "code" keyword of a contract.
--
-- This function is part of the module API, its semantics should not change.
codeEntry :: Parser [ParsedOp]
codeEntry = ops

op' :: Parser Mi.ParsedOp
op' = do
  lms <- asks Mi.letMacros
  choice
    [ (Mi.Prim . Mi.EXT) <$> nopInstr
    , Mi.LMac <$> mkLetMac lms
    , Mi.Prim <$> prim
    , Mi.Mac <$> macro
    , primOrMac
    , Mi.Seq <$> ops
    ]

ops :: Parser [Mi.ParsedOp]
ops = braces $ sepEndBy op' semicolon

-------------------------------------------------------------------------------
-- Let block
-------------------------------------------------------------------------------

-- | Element of a let block
data Let = LetM Mi.LetMacro | LetV Mi.LetValue | LetT Mi.LetType

-- | Incrementally build the let environment
letInner :: Parser Mi.LetEnv
letInner = do
  env <- ask
  l <- lets
  semicolon
  (local (addLet l) letInner) <|> return (addLet l env)

-- | add a Let to the environment in the correct place
addLet :: Let -> Mi.LetEnv -> Mi.LetEnv
addLet l (Mi.LetEnv lms lvs lts) = case l of
  LetM lm -> Mi.LetEnv (Map.insert (Mi.lmName lm) lm lms) lvs lts
  LetV lv -> Mi.LetEnv lms (Map.insert (Mi.lvName lv) lv lvs) lts
  LetT lt -> Mi.LetEnv lms lvs (Map.insert (Mi.ltName lt) lt lts)

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

letMacro :: Parser Mi.LetMacro
letMacro = lexeme $ do
  n <- letName lowerChar
  symbol "::"
  s <- stackFn
  symbol "="
  o <- ops
  return $ Mi.LetMacro n s o

letType :: Parser Mi.LetType
letType = lexeme $ do
  symbol "type"
  n <- letName upperChar <|> letName lowerChar
  symbol "="
  t <- type_
  case t of
    (Mi.Type t' a) ->
      if a == Mi.noAnn
      then return $ Mi.LetType n (Mi.Type t' (Mi.ann n))
      else return $ Mi.LetType n t

letValue :: Parser Mi.LetValue
letValue = lexeme $ do
  n <- letName upperChar
  symbol "::"
  t <- type_
  symbol "="
  v <- value
  return $ Mi.LetValue n t v

mkLetMac :: Map Text Mi.LetMacro -> Parser Mi.LetMacro
mkLetMac lms = choice $ mkParser Mi.lmName <$> (Map.elems lms)

stackFn :: Parser Mi.StackFn
stackFn = do
  vs <- (optional (symbol "forall" >> some varID <* symbol "."))
  a <- stackType
  symbol "->"
  b <- stackType
  return $ Mi.StackFn (Set.fromList <$> vs) a b

tyVar :: Parser Mi.TyVar
tyVar = (Mi.TyCon <$> type_) <|> (Mi.VarID <$> varID)

lowerAlphaNumChar :: Parser Char
lowerAlphaNumChar = satisfy (\x -> Char.isLower x || Char.isDigit x)

varID :: Parser Mi.Var
varID = lexeme $ do
  v <- lowerChar
  vs <- many lowerAlphaNumChar
  return $ Mi.Var (T.pack (v:vs))

-------------------------------------------------------------------------------
-- Primitive Instruction Parsers
-------------------------------------------------------------------------------
prim :: Parser Mi.ParsedInstr
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

failWithOp :: Parser Mi.ParsedInstr
failWithOp = do symbol' "FAILWITH"; return Mi.FAILWITH

loopOp :: Parser Mi.ParsedInstr
loopOp  = do void $ symbol' "LOOP"; Mi.LOOP <$> ops

loopLOp :: Parser Mi.ParsedInstr
loopLOp = do void $ symbol' "LOOP_LEFT"; Mi.LOOP_LEFT <$> ops

execOp :: Parser Mi.ParsedInstr
execOp = do void $ symbol' "EXEC"; Mi.EXEC <$> noteVDef

dipOp :: Parser Mi.ParsedInstr
dipOp = do void $ symbol' "DIP"; Mi.DIP <$> ops

-- Stack Operations

dropOp :: Parser Mi.ParsedInstr
dropOp = do symbol' "DROP"; return Mi.DROP;

dupOp :: Parser Mi.ParsedInstr
dupOp = do void $ symbol' "DUP"; Mi.DUP <$> noteVDef

swapOp :: Parser Mi.ParsedInstr
swapOp = do symbol' "SWAP"; return Mi.SWAP;

pushOp :: Parser Mi.ParsedInstr
pushOp = do
  symbol' "PUSH"
  v <- noteVDef
  (try $ pushLet v) <|> (push' v)
  where
    pushLet v = do
      lvs <- asks Mi.letValues
      lv <- mkLetVal lvs
      return $ Mi.PUSH v (Mi.lvSig lv) (Mi.lvVal lv)
    push' v = do a <- type_; Mi.PUSH v a <$> value

unitOp :: Parser Mi.ParsedInstr
unitOp = do symbol' "UNIT"; (t, v) <- notesTV; return $ Mi.UNIT t v

lambdaOp :: Parser Mi.ParsedInstr
lambdaOp = do symbol' "LAMBDA"; v <- noteVDef; a <- type_; b <- type_;
              Mi.LAMBDA v a b <$> ops

-- Generic comparison

eqOp :: Parser Mi.ParsedInstr
eqOp = do void $ symbol' "EQ"; Mi.EQ <$> noteVDef

neqOp :: Parser Mi.ParsedInstr
neqOp = do void $ symbol' "NEQ"; Mi.NEQ <$> noteVDef

ltOp :: Parser Mi.ParsedInstr
ltOp = do void $ symbol' "LT"; Mi.LT <$> noteVDef

gtOp :: Parser Mi.ParsedInstr
gtOp = do void $ symbol' "GT"; Mi.GT <$> noteVDef

leOp :: Parser Mi.ParsedInstr
leOp = do void $ symbol' "LE"; Mi.LE <$> noteVDef

geOp :: Parser Mi.ParsedInstr
geOp = do void $ symbol' "GE"; Mi.GE <$> noteVDef

-- ad-hoc comparison

compareOp :: Parser Mi.ParsedInstr
compareOp = do void $ symbol' "COMPARE"; Mi.COMPARE <$> noteVDef

-- Operations on booleans

orOp :: Parser Mi.ParsedInstr
orOp = do void $ symbol' "OR";  Mi.OR <$> noteVDef

andOp :: Parser Mi.ParsedInstr
andOp = do void $ symbol' "AND"; Mi.AND <$> noteVDef

xorOp :: Parser Mi.ParsedInstr
xorOp = do void $ symbol' "XOR"; Mi.XOR <$> noteVDef

notOp :: Parser Mi.ParsedInstr
notOp = do void $ symbol' "NOT"; Mi.NOT <$> noteVDef

-- Operations on integers and natural numbers

addOp :: Parser Mi.ParsedInstr
addOp = do void $ symbol' "ADD"; Mi.ADD <$> noteVDef

subOp :: Parser Mi.ParsedInstr
subOp = do void $ symbol' "SUB"; Mi.SUB <$> noteVDef

mulOp :: Parser Mi.ParsedInstr
mulOp = do void $ symbol' "MUL"; Mi.MUL <$> noteVDef

edivOp :: Parser Mi.ParsedInstr
edivOp = do void $ symbol' "EDIV";Mi.EDIV <$> noteVDef

absOp :: Parser Mi.ParsedInstr
absOp = do void $ symbol' "ABS"; Mi.ABS <$> noteVDef

negOp :: Parser Mi.ParsedInstr
negOp = do symbol' "NEG"; return Mi.NEG;

-- Bitwise logical operators

lslOp :: Parser Mi.ParsedInstr
lslOp = do void $ symbol' "LSL"; Mi.LSL <$> noteVDef

lsrOp :: Parser Mi.ParsedInstr
lsrOp = do void $ symbol' "LSR"; Mi.LSR <$> noteVDef

-- Operations on string's

concatOp :: Parser Mi.ParsedInstr
concatOp = do void $ symbol' "CONCAT"; Mi.CONCAT <$> noteVDef

sliceOp :: Parser Mi.ParsedInstr
sliceOp = do void $ symbol' "SLICE"; Mi.SLICE <$> noteVDef

-- Operations on pairs
pairOp :: Parser Mi.ParsedInstr
pairOp = do symbol' "PAIR"; (t, v, (p, q)) <- notesTVF2; return $ Mi.PAIR t v p q

carOp :: Parser Mi.ParsedInstr
carOp = do symbol' "CAR"; (v, f) <- notesVF; return $ Mi.CAR v f

cdrOp :: Parser Mi.ParsedInstr
cdrOp = do symbol' "CDR"; (v, f) <- notesVF; return $ Mi.CDR v f

-- Operations on collections (sets, maps, lists)

emptySetOp :: Parser Mi.ParsedInstr
emptySetOp = do symbol' "EMPTY_SET"; (t, v) <- notesTV;
                Mi.EMPTY_SET t v <$> comparable

emptyMapOp :: Parser Mi.ParsedInstr
emptyMapOp = do symbol' "EMPTY_MAP"; (t, v) <- notesTV; a <- comparable;
                Mi.EMPTY_MAP t v a <$> type_

memOp :: Parser Mi.ParsedInstr
memOp = do void $ symbol' "MEM"; Mi.MEM <$> noteVDef

updateOp :: Parser Mi.ParsedInstr
updateOp = do symbol' "UPDATE"; return Mi.UPDATE

iterOp :: Parser Mi.ParsedInstr
iterOp = do void $ symbol' "ITER"; Mi.ITER <$> ops

sizeOp :: Parser Mi.ParsedInstr
sizeOp = do void $ symbol' "SIZE"; Mi.SIZE <$> noteVDef

mapOp :: Parser Mi.ParsedInstr
mapOp = do symbol' "MAP"; v <- noteVDef; Mi.MAP v <$> ops

getOp :: Parser Mi.ParsedInstr
getOp = do void $ symbol' "GET"; Mi.GET <$> noteVDef

nilOp :: Parser Mi.ParsedInstr
nilOp = do symbol' "NIL"; (t, v) <- notesTV; Mi.NIL t v <$> type_

consOp :: Parser Mi.ParsedInstr
consOp = do void $ symbol' "CONS"; Mi.CONS <$> noteVDef

ifConsOp :: Parser Mi.ParsedInstr
ifConsOp = do symbol' "IF_CONS"; a <- ops; Mi.IF_CONS a <$> ops

-- Operations on options

someOp :: Parser Mi.ParsedInstr
someOp = do symbol' "SOME"; (t, v, f) <- notesTVF; return $ Mi.SOME t v f

noneOp :: Parser Mi.ParsedInstr
noneOp = do symbol' "NONE"; (t, v, f) <- notesTVF; Mi.NONE t v f <$> type_

ifNoneOp :: Parser Mi.ParsedInstr
ifNoneOp = do symbol' "IF_NONE"; a <- ops; Mi.IF_NONE a <$> ops

-- Operations on unions

leftOp :: Parser Mi.ParsedInstr
leftOp = do symbol' "LEFT"; (t, v, (f, f')) <- notesTVF2;
               Mi.LEFT t v f f' <$> type_

rightOp :: Parser Mi.ParsedInstr
rightOp = do symbol' "RIGHT"; (t, v, (f, f')) <- notesTVF2;
               Mi.RIGHT t v f f' <$> type_

ifLeftOp :: Parser Mi.ParsedInstr
ifLeftOp = do symbol' "IF_LEFT"; a <- ops; Mi.IF_LEFT a <$> ops

-- Operations on contracts

createContractOp :: Parser Mi.ParsedInstr
createContractOp = do symbol' "CREATE_CONTRACT"; v <- noteVDef; v' <- noteVDef;
                       Mi.CREATE_CONTRACT v v' <$> braces contract

createAccountOp :: Parser Mi.ParsedInstr
createAccountOp = do symbol' "CREATE_ACCOUNT"; v <- noteVDef; v' <- noteVDef;
                       return $ Mi.CREATE_ACCOUNT v v'

transferTokensOp :: Parser Mi.ParsedInstr
transferTokensOp = do void $ symbol' "TRANSFER_TOKENS"; Mi.TRANSFER_TOKENS <$> noteVDef

setDelegateOp :: Parser Mi.ParsedInstr
setDelegateOp = do void $ symbol' "SET_DELEGATE"; Mi.SET_DELEGATE <$> noteVDef

balanceOp :: Parser Mi.ParsedInstr
balanceOp = do void $ symbol' "BALANCE"; Mi.BALANCE <$> noteVDef

contractOp :: Parser Mi.ParsedInstr
contractOp = do void $ symbol' "CONTRACT"; Mi.CONTRACT <$> noteVDef <*> type_

sourceOp :: Parser Mi.ParsedInstr
sourceOp = do void $ symbol' "SOURCE"; Mi.SOURCE <$> noteVDef

senderOp :: Parser Mi.ParsedInstr
senderOp = do void $ symbol' "SENDER"; Mi.SENDER <$> noteVDef

amountOp :: Parser Mi.ParsedInstr
amountOp = do void $ symbol' "AMOUNT"; Mi.AMOUNT <$> noteVDef

implicitAccountOp :: Parser Mi.ParsedInstr
implicitAccountOp = do void $ symbol' "IMPLICIT_ACCOUNT"; Mi.IMPLICIT_ACCOUNT <$> noteVDef

selfOp :: Parser Mi.ParsedInstr
selfOp = do void $ symbol' "SELF"; Mi.SELF <$> noteVDef

addressOp :: Parser Mi.ParsedInstr
addressOp = do void $ symbol' "ADDRESS"; Mi.ADDRESS <$> noteVDef

-- Special Operations
nowOp :: Parser Mi.ParsedInstr
nowOp = do void $ symbol' "NOW"; Mi.NOW <$> noteVDef

stepsToQuotaOp :: Parser Mi.ParsedInstr
stepsToQuotaOp = do void $ symbol' "STEPS_TO_QUOTA"; Mi.STEPS_TO_QUOTA <$> noteVDef

-- Operations on bytes
packOp :: Parser Mi.ParsedInstr
packOp = do void $ symbol' "PACK"; Mi.PACK <$> noteVDef

unpackOp :: Parser Mi.ParsedInstr
unpackOp = do symbol' "UNPACK"; v <- noteVDef; Mi.UNPACK v <$> type_

-- Cryptographic Primitives

checkSigOp :: Parser Mi.ParsedInstr
checkSigOp = do void $ symbol' "CHECK_SIGNATURE"; Mi.CHECK_SIGNATURE <$> noteVDef

blake2BOp :: Parser Mi.ParsedInstr
blake2BOp = do void $ symbol' "BLAKE2B"; Mi.BLAKE2B <$> noteVDef

sha256Op :: Parser Mi.ParsedInstr
sha256Op = do void $ symbol' "SHA256"; Mi.SHA256 <$> noteVDef

sha512Op :: Parser Mi.ParsedInstr
sha512Op = do void $ symbol' "SHA512"; Mi.SHA512 <$> noteVDef

hashKeyOp :: Parser Mi.ParsedInstr
hashKeyOp = do void $ symbol' "HASH_KEY"; Mi.HASH_KEY <$> noteVDef

{- Type operations -}
castOp :: Parser Mi.ParsedInstr
castOp = do void $ symbol' "CAST"; Mi.CAST <$> noteVDef <*> type_;

renameOp :: Parser Mi.ParsedInstr
renameOp = do void $ symbol' "RENAME"; Mi.RENAME <$> noteVDef

isNatOp :: Parser Mi.ParsedInstr
isNatOp = do void $ symbol' "ISNAT"; Mi.ISNAT <$> noteVDef

intOp :: Parser Mi.ParsedInstr
intOp = do void $ symbol' "INT"; Mi.INT <$> noteVDef

-------------------------------------------------------------------------------
-- Macro Parsers
-------------------------------------------------------------------------------
cmpOp :: Parser Mi.ParsedInstr
cmpOp = eqOp <|> neqOp <|> ltOp <|> gtOp <|> leOp <|> gtOp <|> geOp

macro :: Parser Mi.Macro
macro = do symbol' "CASE"; is <- someNE ops; return $ Mi.CASE is
  <|> do symbol' "VIEW"; a <- ops; return $ Mi.VIEW a
  <|> do symbol' "VOID"; a <- ops; return $ Mi.VOID a
  <|> do symbol' "CMP"; a <- cmpOp; Mi.CMP a <$> noteVDef
  <|> do void $ symbol' "IF_SOME"; Mi.IF_SOME <$> ops <*> ops
  <|> do void $ symbol' "IF_RIGHT"; Mi.IF_RIGHT <$> ops <*> ops
  <|> do symbol' "FAIL"; return Mi.FAIL
  <|> do void $ symbol' "ASSERT_CMP"; Mi.ASSERT_CMP <$> cmpOp
  <|> do symbol' "ASSERT_NONE"; return Mi.ASSERT_NONE
  <|> do symbol' "ASSERT_SOME"; return Mi.ASSERT_SOME
  <|> do symbol' "ASSERT_LEFT"; return Mi.ASSERT_LEFT
  <|> do symbol' "ASSERT_RIGHT"; return Mi.ASSERT_RIGHT
  <|> do void $ symbol' "ASSERT_"; Mi.ASSERTX <$> cmpOp
  <|> do symbol' "ASSERT"; return Mi.ASSERT
  <|> do string' "DI"; n <- num "I"; symbol' "P"; Mi.DIIP (n + 1) <$> ops
  <|> do string' "DU"; n <- num "U"; symbol' "P"; Mi.DUUP (n + 1) <$> noteVDef
  <|> unpairMac
  <|> cadrMac
  <|> setCadrMac
  where
   num str = fromIntegral . length <$> some (string' str)

pairMac :: Parser Mi.Macro
pairMac = do
  a <- pairMacInner
  symbol' "R"
  (tn, vn, fns) <- permute3Def noteTDef noteV (some noteF)
  let ps = Macro.mapLeaves ((Mi.noAnn,) <$> fns) a
  return $ Mi.PAPAIR ps tn vn

pairMacInner :: Parser Mi.PairStruct
pairMacInner = do
  string' "P"
  l <- (string' "A" >> return (Mi.F (Mi.noAnn, Mi.noAnn))) <|> pairMacInner
  r <- (string' "I" >> return (Mi.F (Mi.noAnn, Mi.noAnn))) <|> pairMacInner
  return $ Mi.P l r

unpairMac :: Parser Mi.Macro
unpairMac = do
  string' "UN"
  a <- pairMacInner
  symbol' "R"
  (vns, fns) <- permute2Def (some noteV) (some noteF)
  return $ Mi.UNPAIR (Macro.mapLeaves (zip vns fns) a)

cadrMac :: Parser Mi.Macro
cadrMac = lexeme $ do
  string' "C"
  a <- some $ try $ cadrInner <* notFollowedBy (string' "R")
  b <- cadrInner
  symbol' "R"
  (vn, fn) <- notesVF
  return $ Mi.CADR (a ++ pure b) vn fn

cadrInner :: Parser Mi.CadrStruct
cadrInner = (string' "A" >> return Mi.A) <|> (string' "D" >> return Mi.D)

setCadrMac :: Parser Mi.Macro
setCadrMac = do
  string' "SET_C"
  a <- some cadrInner
  symbol' "R"
  (v, f) <- notesVF
  return $ Mi.SET_CADR a v f

mapCadrMac :: Parser Mi.Macro
mapCadrMac = do
  string' "MAP_C"
  a <- some cadrInner
  symbol' "R"
  (v, f) <- notesVF
  Mi.MAP_CADR a v f <$> ops

ifCmpMac :: Parser Mi.Macro
ifCmpMac = symbol' "IFCMP" >> Mi.IFCMP <$> cmpOp <*> noteVDef <*> ops <*> ops

ifOrIfX :: Parser Mi.ParsedOp
ifOrIfX = do
  symbol' "IF"
  a <- eitherP cmpOp ops
  case a of
    Left cmp -> Mi.Mac <$> (Mi.IFX cmp <$> ops <*> ops)
    Right op -> Mi.Prim <$> (Mi.IF op <$> ops)

-- Some of the operations and macros have the same prefixes in their names
-- So this case should be handled separately
primOrMac :: Parser Mi.ParsedOp
primOrMac = ((Mi.Mac <$> ifCmpMac) <|> ifOrIfX)
  <|> ((Mi.Mac <$> mapCadrMac) <|> (Mi.Prim <$> mapOp))
  <|> (try (Mi.Prim <$> pairOp) <|> Mi.Mac <$> pairMac)

-------------------------------------------------------------------------------
-- Morley Instructions
-------------------------------------------------------------------------------

nopInstr :: Parser Mi.ParsedUExtInstr
nopInstr = choice [stackOp, testAssertOp, printOp]

stackOp :: Parser Mi.ParsedUExtInstr
stackOp = symbol' "STACKTYPE" >> U.STACKTYPE <$> stackType

testAssertOp :: Parser Mi.ParsedUExtInstr
testAssertOp = symbol' "TEST_ASSERT" >> U.UTEST_ASSERT <$> testAssert

printOp :: Parser Mi.ParsedUExtInstr
printOp = symbol' "PRINT" >> U.UPRINT <$> printComment

testAssert :: Parser Mi.ParsedUTestAssert
testAssert = do
  n <- lexeme (T.pack <$> some alphaNumChar)
  c <- printComment
  o <- ops
  return $ U.TestAssert n c o

printComment :: Parser U.PrintComment
printComment = do
  string "\""
  let validChar = T.pack <$> some (satisfy (\x -> x /= '%' && x /= '"'))
  c <- many (Right <$> stackRef <|> Left <$> validChar)
  symbol "\""
  return $ U.PrintComment c

stackRef :: Parser U.StackRef
stackRef = do
  string "%"
  n <- brackets' L.decimal
  return $ U.StackRef n

stackType :: Parser U.StackTypePattern
stackType = symbol "'[" >> (emptyStk <|> stkCons <|> stkRest)
  where
    emptyStk = try $ symbol "]" >> return U.StkEmpty
    stkRest = try $ symbol "..." >> symbol "]" >> return U.StkRest
    stkCons = try $ do
      t <- tyVar
      s <- (symbol "," >> stkCons <|> stkRest) <|> emptyStk
      return $ U.StkCons t s
