{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Michelson.Parser.Macro where

import  Text.Megaparsec

import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer       as L
import qualified Language.Michelson.Types         as M
import qualified Language.Michelson.Parser.Prim   as P
import qualified Data.Sequence                    as Seq
import Prelude hiding (fail)

symbol = P.symbol

--macro :: Parser M.Instructions
--macro =


{- compare macros:
    * cmp{eq}
    * if{eq}
    * ifcmp{eq}
-}

cmpOp = P.eq <|> P.neq <|> P.lt <|> P.gt <|> P.le <|> P.gt


cmp_ :: P.Parser M.Instructions
cmp_ = do
  string "CMP"
  a <- cmpOp
  return $ cmp' a

cmp' :: M.Op -> M.Instructions
cmp' op = M.instructionsFromList [M.COMPARE P.noVNote, op]

if_ :: P.Parser M.Instructions
if_ = do
  string "IF"
  a <- cmpOp
  bt <- P.ops
  bf <- P.ops
  return $ if' a bt bf

if' :: M.Op -> M.Instructions -> M.Instructions -> M.Instructions
if' op bt bf = M.instructionsFromList [op, M.IF bt bf]

ifcmp_ :: P.Parser M.Instructions
ifcmp_ = do
  string "IFCMP"
  a <- cmpOp
  bt <- P.ops
  bf <- P.ops
  return $ ifcmp' a bt bf

ifcmp' :: M.Op -> M.Instructions -> M.Instructions -> M.Instructions
ifcmp' op bt bf = M.instructionsFromList [M.COMPARE P.noVNote, op, M.IF bt bf]

-- fail

fail :: P.Parser M.Instructions
fail = symbol "FAIL" >> return fail'

fail' :: M.Instructions
fail' = M.instructionsFromList [M.UNIT P.noTNote, M.FAILWITH]

{- assertion macros:
  * assert
  * assert_{eq}
  * assert_none
  * assert_some
  * assert_left
  * assert_right
-}


assert :: P.Parser M.Instructions
assert = do
  symbol "ASSERT"
  return $ M.instructionsFromList [M.IF M.noInstructions fail']

assert_ :: P.Parser M.Instructions
assert_ = do
  string "ASSERT_"
  a <- cmpOp
  return $ if' a (M.noInstructions) fail'

assert_cmp_ :: P.Parser M.Instructions
assert_cmp_ = do
  string "ASSERT_CMP"
  a <- cmpOp
  return $ ifcmp' a (M.noInstructions) fail'

assert_none :: P.Parser M.Instructions
assert_none = do
  symbol "ASSERT_NONE"
  return $ M.instructionsFromList [M.IF_NONE M.noInstructions fail']

assert_some :: P.Parser M.Instructions
assert_some = do
  symbol "ASSERT_SOME"
  return $ M.instructionsFromList [M.IF_NONE fail' M.noInstructions]

assert_left :: P.Parser M.Instructions
assert_left = do
  symbol "ASSERT_LEFT"
  return $ M.instructionsFromList [M.IF_LEFT M.noInstructions fail']

assert_right :: P.Parser M.Instructions
assert_right = do
  symbol "ASSERT_RIGHT"
  return $ M.instructionsFromList [M.IF_LEFT fail' M.noInstructions]

{- syntactic "conveniences"
   * DII+P code
   * DUU+P
   * [PAIR]
   * UNPAIR
   * C[AD]R
   * IF_SOME
   * SET_C[AD]R
   * MAP_C[AD]R
-}



