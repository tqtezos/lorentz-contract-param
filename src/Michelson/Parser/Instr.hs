-- | Parsing of Michelson instructions.

module Michelson.Parser.Instr
  ( primInstr
  , ops'
  -- * These are handled separately to have better error messages
  , mapOp
  , pairOp
  , cmpOp
  ) where

import Prelude hiding (EQ, GT, LT, many, note, some, try)

import Text.Megaparsec (choice, sepEndBy, try)

import Michelson.Lexer
import Michelson.Parser.Annotations
import Michelson.Parser.Type
import Michelson.Parser.Types (Parser, letValues)
import Michelson.Parser.Value
import Michelson.Types (LetValue(..), ParsedInstr, ParsedOp(..))
import Michelson.Untyped

-- | Parser for primitive Michelson instruction (no macros and extensions).
primInstr :: Parser (Contract' ParsedOp) -> Parser ParsedOp -> Parser ParsedInstr
primInstr contractParser opParser = choice
  [ dropOp, dupOp, swapOp, pushOp opParser, someOp, noneOp, unitOp
  , ifNoneOp opParser, carOp, cdrOp, leftOp, rightOp, ifLeftOp opParser, nilOp
  , consOp, ifConsOp opParser, sizeOp, emptySetOp, emptyMapOp, iterOp opParser
  , memOp, getOp, updateOp, loopLOp opParser, loopOp opParser
  , lambdaOp opParser, execOp, dipOp opParser, failWithOp, castOp, renameOp
  , concatOp, packOp, unpackOp, sliceOp, isNatOp, addressOp, addOp, subOp
  , mulOp, edivOp, absOp, negOp, lslOp, lsrOp, orOp, andOp, xorOp, notOp
  , compareOp, eqOp, neqOp, ltOp, leOp, gtOp, geOp, intOp, selfOp, contractOp
  , transferTokensOp, setDelegateOp, createAccountOp
  , createContractOp contractParser, implicitAccountOp, nowOp, amountOp
  , balanceOp, checkSigOp, sha256Op, sha512Op, blake2BOp, hashKeyOp
  , stepsToQuotaOp, sourceOp, senderOp
  ]

-- | Parse a sequence of instructions.
ops' :: Parser ParsedOp -> Parser [ParsedOp]
ops' opParser = braces $ sepEndBy opParser semicolon

-- Control Structures

failWithOp :: Parser ParsedInstr
failWithOp = do symbol' "FAILWITH"; return FAILWITH

loopOp :: Parser ParsedOp -> Parser ParsedInstr
loopOp opParser = do void $ symbol' "LOOP"; LOOP <$> ops' opParser

loopLOp :: Parser ParsedOp -> Parser ParsedInstr
loopLOp opParser = do void $ symbol' "LOOP_LEFT"; LOOP_LEFT <$> ops' opParser

execOp :: Parser ParsedInstr
execOp = do void $ symbol' "EXEC"; EXEC <$> noteVDef

dipOp :: Parser ParsedOp -> Parser ParsedInstr
dipOp opParser = do void $ symbol' "DIP"; DIP <$> ops' opParser

-- Stack Operations

dropOp :: Parser ParsedInstr
dropOp = do symbol' "DROP"; return DROP;

dupOp :: Parser ParsedInstr
dupOp = do void $ symbol' "DUP"; DUP <$> noteVDef

swapOp :: Parser ParsedInstr
swapOp = do symbol' "SWAP"; return SWAP;

pushOp :: Parser ParsedOp -> Parser ParsedInstr
pushOp opParser = do
  symbol' "PUSH"
  v <- noteVDef
  (try $ pushLet v) <|> (push' v)
  where
    pushLet v = do
      lvs <- asks letValues
      lv <- mkLetVal lvs
      return $ PUSH v (lvSig lv) (lvVal lv)
    push' v = PUSH v <$> type_ <*> value' opParser

unitOp :: Parser ParsedInstr
unitOp = do symbol' "UNIT"; (t, v) <- notesTV; return $ UNIT t v

lambdaOp :: Parser ParsedOp -> Parser ParsedInstr
lambdaOp opParser =
  symbol' "LAMBDA" *>
  (LAMBDA <$> noteVDef <*> type_ <*> type_ <*> ops' opParser)

-- Generic comparison

cmpOp :: Parser ParsedInstr
cmpOp = eqOp <|> neqOp <|> ltOp <|> gtOp <|> leOp <|> gtOp <|> geOp

eqOp :: Parser ParsedInstr
eqOp = do void $ symbol' "EQ"; EQ <$> noteVDef

neqOp :: Parser ParsedInstr
neqOp = do void $ symbol' "NEQ"; NEQ <$> noteVDef

ltOp :: Parser ParsedInstr
ltOp = do void $ symbol' "LT"; LT <$> noteVDef

gtOp :: Parser ParsedInstr
gtOp = do void $ symbol' "GT"; GT <$> noteVDef

leOp :: Parser ParsedInstr
leOp = do void $ symbol' "LE"; LE <$> noteVDef

geOp :: Parser ParsedInstr
geOp = do void $ symbol' "GE"; GE <$> noteVDef

-- ad-hoc comparison

compareOp :: Parser ParsedInstr
compareOp = do void $ symbol' "COMPARE"; COMPARE <$> noteVDef

-- Operations on booleans

orOp :: Parser ParsedInstr
orOp = do void $ symbol' "OR";  OR <$> noteVDef

andOp :: Parser ParsedInstr
andOp = do void $ symbol' "AND"; AND <$> noteVDef

xorOp :: Parser ParsedInstr
xorOp = do void $ symbol' "XOR"; XOR <$> noteVDef

notOp :: Parser ParsedInstr
notOp = do void $ symbol' "NOT"; NOT <$> noteVDef

-- Operations on integers and natural numbers

addOp :: Parser ParsedInstr
addOp = do void $ symbol' "ADD"; ADD <$> noteVDef

subOp :: Parser ParsedInstr
subOp = do void $ symbol' "SUB"; SUB <$> noteVDef

mulOp :: Parser ParsedInstr
mulOp = do void $ symbol' "MUL"; MUL <$> noteVDef

edivOp :: Parser ParsedInstr
edivOp = do void $ symbol' "EDIV";EDIV <$> noteVDef

absOp :: Parser ParsedInstr
absOp = do void $ symbol' "ABS"; ABS <$> noteVDef

negOp :: Parser ParsedInstr
negOp = do symbol' "NEG"; return NEG;

-- Bitwise logical operators

lslOp :: Parser ParsedInstr
lslOp = do void $ symbol' "LSL"; LSL <$> noteVDef

lsrOp :: Parser ParsedInstr
lsrOp = do void $ symbol' "LSR"; LSR <$> noteVDef

-- Operations on string's

concatOp :: Parser ParsedInstr
concatOp = do void $ symbol' "CONCAT"; CONCAT <$> noteVDef

sliceOp :: Parser ParsedInstr
sliceOp = do void $ symbol' "SLICE"; SLICE <$> noteVDef

-- Operations on pairs
pairOp :: Parser ParsedInstr
pairOp = do symbol' "PAIR"; (t, v, (p, q)) <- notesTVF2; return $ PAIR t v p q

carOp :: Parser ParsedInstr
carOp = do symbol' "CAR"; (v, f) <- notesVF; return $ CAR v f

cdrOp :: Parser ParsedInstr
cdrOp = do symbol' "CDR"; (v, f) <- notesVF; return $ CDR v f

-- Operations on collections (sets, maps, lists)

emptySetOp :: Parser ParsedInstr
emptySetOp = do symbol' "EMPTY_SET"; (t, v) <- notesTV;
                EMPTY_SET t v <$> comparable

emptyMapOp :: Parser ParsedInstr
emptyMapOp = do symbol' "EMPTY_MAP"; (t, v) <- notesTV; a <- comparable;
                EMPTY_MAP t v a <$> type_

memOp :: Parser ParsedInstr
memOp = do void $ symbol' "MEM"; MEM <$> noteVDef

updateOp :: Parser ParsedInstr
updateOp = do symbol' "UPDATE"; return UPDATE

iterOp :: Parser ParsedOp -> Parser ParsedInstr
iterOp opParser = do void $ symbol' "ITER"; ITER <$> ops' opParser

sizeOp :: Parser ParsedInstr
sizeOp = do void $ symbol' "SIZE"; SIZE <$> noteVDef

mapOp :: Parser ParsedOp -> Parser ParsedInstr
mapOp opParser = do symbol' "MAP"; v <- noteVDef; MAP v <$> ops' opParser

getOp :: Parser ParsedInstr
getOp = do void $ symbol' "GET"; GET <$> noteVDef

nilOp :: Parser ParsedInstr
nilOp = do symbol' "NIL"; (t, v) <- notesTV; NIL t v <$> type_

consOp :: Parser ParsedInstr
consOp = do void $ symbol' "CONS"; CONS <$> noteVDef

ifConsOp :: Parser ParsedOp -> Parser ParsedInstr
ifConsOp opParser =
  symbol' "IF_CONS" *>
  (IF_CONS <$> ops' opParser <*> ops' opParser)

-- Operations on options

someOp :: Parser ParsedInstr
someOp = do symbol' "SOME"; (t, v, f) <- notesTVF; return $ SOME t v f

noneOp :: Parser ParsedInstr
noneOp = do symbol' "NONE"; (t, v, f) <- notesTVF; NONE t v f <$> type_

ifNoneOp :: Parser ParsedOp -> Parser ParsedInstr
ifNoneOp opParser =
  symbol' "IF_NONE" *>
  (IF_NONE <$> ops' opParser <*> ops' opParser)

-- Operations on unions

leftOp :: Parser ParsedInstr
leftOp = do symbol' "LEFT"; (t, v, (f, f')) <- notesTVF2;
               LEFT t v f f' <$> type_

rightOp :: Parser ParsedInstr
rightOp = do symbol' "RIGHT"; (t, v, (f, f')) <- notesTVF2;
               RIGHT t v f f' <$> type_

ifLeftOp :: Parser ParsedOp -> Parser ParsedInstr
ifLeftOp opParser = do
  symbol' "IF_LEFT"
  a <- ops' opParser
  IF_LEFT a <$> ops' opParser

-- Operations on contracts

createContractOp :: Parser (Contract' ParsedOp) -> Parser ParsedInstr
createContractOp contractParser =
  symbol' "CREATE_CONTRACT" *>
  (CREATE_CONTRACT <$> noteVDef <*> noteVDef <*> braces contractParser)

createAccountOp :: Parser ParsedInstr
createAccountOp = do symbol' "CREATE_ACCOUNT"; v <- noteVDef; v' <- noteVDef;
                       return $ CREATE_ACCOUNT v v'

transferTokensOp :: Parser ParsedInstr
transferTokensOp = do void $ symbol' "TRANSFER_TOKENS"; TRANSFER_TOKENS <$> noteVDef

setDelegateOp :: Parser ParsedInstr
setDelegateOp = do void $ symbol' "SET_DELEGATE"; SET_DELEGATE <$> noteVDef

balanceOp :: Parser ParsedInstr
balanceOp = do void $ symbol' "BALANCE"; BALANCE <$> noteVDef

contractOp :: Parser ParsedInstr
contractOp = do void $ symbol' "CONTRACT"; CONTRACT <$> noteVDef <*> type_

sourceOp :: Parser ParsedInstr
sourceOp = do void $ symbol' "SOURCE"; SOURCE <$> noteVDef

senderOp :: Parser ParsedInstr
senderOp = do void $ symbol' "SENDER"; SENDER <$> noteVDef

amountOp :: Parser ParsedInstr
amountOp = do void $ symbol' "AMOUNT"; AMOUNT <$> noteVDef

implicitAccountOp :: Parser ParsedInstr
implicitAccountOp = do void $ symbol' "IMPLICIT_ACCOUNT"; IMPLICIT_ACCOUNT <$> noteVDef

selfOp :: Parser ParsedInstr
selfOp = do void $ symbol' "SELF"; SELF <$> noteVDef

addressOp :: Parser ParsedInstr
addressOp = do void $ symbol' "ADDRESS"; ADDRESS <$> noteVDef

-- Special Operations

nowOp :: Parser ParsedInstr
nowOp = do void $ symbol' "NOW"; NOW <$> noteVDef

stepsToQuotaOp :: Parser ParsedInstr
stepsToQuotaOp = do void $ symbol' "STEPS_TO_QUOTA"; STEPS_TO_QUOTA <$> noteVDef

-- Operations on bytes

packOp :: Parser ParsedInstr
packOp = do void $ symbol' "PACK"; PACK <$> noteVDef

unpackOp :: Parser ParsedInstr
unpackOp = do symbol' "UNPACK"; v <- noteVDef; UNPACK v <$> type_

-- Cryptographic Primitives

checkSigOp :: Parser ParsedInstr
checkSigOp = do void $ symbol' "CHECK_SIGNATURE"; CHECK_SIGNATURE <$> noteVDef

blake2BOp :: Parser ParsedInstr
blake2BOp = do void $ symbol' "BLAKE2B"; BLAKE2B <$> noteVDef

sha256Op :: Parser ParsedInstr
sha256Op = do void $ symbol' "SHA256"; SHA256 <$> noteVDef

sha512Op :: Parser ParsedInstr
sha512Op = do void $ symbol' "SHA512"; SHA512 <$> noteVDef

hashKeyOp :: Parser ParsedInstr
hashKeyOp = do void $ symbol' "HASH_KEY"; HASH_KEY <$> noteVDef

-- Type operations

castOp :: Parser ParsedInstr
castOp = do void $ symbol' "CAST"; CAST <$> noteVDef <*> type_;

renameOp :: Parser ParsedInstr
renameOp = do void $ symbol' "RENAME"; RENAME <$> noteVDef

isNatOp :: Parser ParsedInstr
isNatOp = do void $ symbol' "ISNAT"; ISNAT <$> noteVDef

intOp :: Parser ParsedInstr
intOp = do void $ symbol' "INT"; INT <$> noteVDef
