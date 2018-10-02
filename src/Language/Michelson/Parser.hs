{-# LANGUAGE OverloadedStrings #-}

module Language.Michelson.Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T

import qualified Language.Michelson.Types as M

import Data.Void (Void)

type Parser = Parsec Void T.Text

-- Data

-- todo: natural numbers, signed numbers

l_int :: Parser Integer
l_int = L.decimal <|> (string "0x">> L.hexadecimal)

-- todo: escape sequences

l_string :: Parser T.Text
l_string = do
  char '"'
  str <- some asciiChar
  char '"'
  return $ T.pack str

-- todo: various "string" literals

d_Unit :: Parser M.D
d_Unit = string "Unit" >> return M.DUnit

d_True :: Parser M.D
d_True = string "True" >> return M.DTrue

d_False :: Parser M.D
d_False = string "False" >> return M.DFalse

d_Pair :: Parser M.D
d_Pair = do
  string "Pair"
  a <- parse_d
  b <- parse_d
  return $ M.DPair a b

d_Left :: Parser M.D
d_Left = do
  string "Left"
  a <- parse_d
  return $ M.DLeft a

d_Right :: Parser M.D
d_Right = do
  string "Right"
  a <- parse_d
  return $ M.DRight a

d_Some :: Parser M.D
d_Some = do
  string "Some"
  a <- parse_d
  return $ M.DSome a

d_None :: Parser M.D
d_None = string "None" >> return M.DNone

parse_d :: Parser M.D
parse_d = undefined

-- Instructions

i_SEQ :: Parser M.I
i_SEQ = undefined

i_DROP :: Parser M.I
i_DROP = string "DROP" >> return M.DROP

i_DUP :: Parser M.I
i_DUP = string "DUP" >> return M.DUP

i_SWAP :: Parser M.I
i_SWAP = string "SWAP" >> return M.SWAP

i_PUSH :: Parser M.I
i_PUSH = undefined

i_SOME :: Parser M.I
i_SOME = string "SOME" >> return M.SOME

i_NONE :: Parser M.I
i_NONE = undefined

i_IF_NONE :: Parser M.I
i_IF_NONE = undefined

i_PAIR :: Parser M.I
i_PAIR = string "PAIR" >> return M.PAIR

i_CAR :: Parser M.I
i_CAR = string "CAR" >> return M.CAR

i_CDR :: Parser M.I
i_CDR = string "CDR" >> return M.CDR

i_LEFT :: Parser M.I
i_LEFT = undefined

i_RIGHT :: Parser M.I
i_RIGHT = undefined

i_IF_LEFT :: Parser M.I
i_IF_LEFT = undefined

i_NIL :: Parser M.I
i_NIL = undefined

i_CONS :: Parser M.I
i_CONS = string "CONS" >> return M.CONS

i_IF_CONS :: Parser M.I
i_IF_CONS = undefined

i_EMPTY_SET :: Parser M.I
i_EMPTY_SET = undefined

i_EMPTY_MAP :: Parser M.I
i_EMPTY_MAP = undefined

i_MAP :: Parser M.I
i_MAP = undefined

i_ITER :: Parser M.I
i_ITER = undefined

i_MEM :: Parser M.I
i_MEM = string "MEM" >> return M.MEM

i_GET :: Parser M.I
i_GET = string "GET" >> return M.GET

i_UPDATE :: Parser M.I
i_UPDATE = string "UPDATE" >> return M.UPDATE

i_IF :: Parser M.I
i_IF = undefined

i_LOOP :: Parser M.I
i_LOOP = undefined

i_LOOP_LEFT :: Parser M.I
i_LOOP_LEFT = undefined

i_LAMBDA :: Parser M.I
i_LAMBDA = undefined

i_EXEC :: Parser M.I
i_EXEC = string "EXEC" >> return M.EXEC

i_DIP :: Parser M.I
i_DIP = undefined

i_FAILWITH :: Parser M.I
i_FAILWITH = string "FAILWITH" >> return M.FAILWITH

i_CAST :: Parser M.I
i_CAST = string "CAST" >> return M.CAST

i_RENAME :: Parser M.I
i_RENAME = string "RENAME" >> return M.RENAME

i_CONCAT :: Parser M.I
i_CONCAT = string "CONCAT" >> return M.CONCAT

i_ADD :: Parser M.I
i_ADD = string "ADD" >> return M.ADD

i_SUB :: Parser M.I
i_SUB = string "SUB" >> return M.SUB

i_MUL :: Parser M.I
i_MUL = string "MUL" >> return M.MUL

i_DIV :: Parser M.I
i_DIV = string "DIV" >> return M.DIV

i_ABS :: Parser M.I
i_ABS = string "ABS" >> return M.ABS

i_NEG :: Parser M.I
i_NEG = string "NEG" >> return M.NEG

i_MOD :: Parser M.I
i_MOD = string "MOD" >> return M.MOD

i_LSL :: Parser M.I
i_LSL = string "LSL" >> return M.LSL

i_LSR :: Parser M.I
i_LSR = string "LSR" >> return M.LSR

i_OR :: Parser M.I
i_OR = string "OR" >> return M.OR

i_AND :: Parser M.I
i_AND = string "AND" >> return M.AND

i_NOT :: Parser M.I
i_NOT = string "NOT" >> return M.NOT

i_COMPARE :: Parser M.I
i_COMPARE = string "COMPARE" >> return M.COMPARE

i_EQ :: Parser M.I
i_EQ = string "EQ" >> return M.EQ

i_NEQ :: Parser M.I
i_NEQ = string "NEQ" >> return M.NEQ

i_LT :: Parser M.I
i_LT = string "LT" >> return M.LT

i_GT :: Parser M.I
i_GT = string "GT" >> return M.GT

i_LE :: Parser M.I
i_LE = string "LE" >> return M.LE

i_GE :: Parser M.I
i_GE = string "GE" >> return M.GE

i_INT :: Parser M.I
i_INT = string "INT" >> return M.INT

i_SELF :: Parser M.I
i_SELF = string "SELF" >> return M.SELF

i_TRANSFER_TOKENS :: Parser M.I
i_TRANSFER_TOKENS = string "TRANSFER_TOKENS" >> return M.TRANSFER_TOKENS

i_SET_DELEGATE :: Parser M.I
i_SET_DELEGATE = string "SET_DELEGATE" >> return M.SET_DELEGATE

i_CREATE_ACCOUNT :: Parser M.I
i_CREATE_ACCOUNT = string "CREATE_ACCOUNT" >> return M.CREATE_ACCOUNT

i_CREATE_CONTRACT :: Parser M.I
i_CREATE_CONTRACT = string "CREATE_CONTRACT" >> return M.CREATE_CONTRACT

i_IMPLICIT_ACCOUNT :: Parser M.I
i_IMPLICIT_ACCOUNT = string "IMPLICIT_ACCOUNT" >> return M.IMPLICIT_ACCOUNT

i_NOW :: Parser M.I
i_NOW = string "NOW" >> return M.NOW

i_AMOUNT :: Parser M.I
i_AMOUNT = string "AMOUNT" >> return M.AMOUNT

i_BALANCE :: Parser M.I
i_BALANCE = string "BALANCE" >> return M.BALANCE

i_CHECK_SIGNATURE :: Parser M.I
i_CHECK_SIGNATURE = string "CHECK_SIGNATURE" >> return M.CHECK_SIGNATURE

i_BLAKE2B :: Parser M.I
i_BLAKE2B = string "BLAKE2B" >> return M.BLAKE2B

i_HASH_KEY :: Parser M.I
i_HASH_KEY = string "HASH_KEY" >> return M.HASH_KEY

i_STEPS_TO_QUOTA :: Parser M.I
i_STEPS_TO_QUOTA = string "STEPS_TO_QUOTA" >> return M.STEPS_TO_QUOTA

i_SOURCE :: Parser M.I
i_SOURCE = string "SOURCE" >> return M.SOURCE

i_SENDER :: Parser M.I
i_SENDER = string "SENDER" >> return M.SENDER

-- Types


-- Comparable Types

parse_ct :: Parser M.CT
parse_ct = ct_int
  <|> ct_nat
  <|> ct_string
  <|> ct_tez
  <|> ct_bool
  <|> ct_keyhash
  <|> ct_timestamp

ct_int :: Parser M.CT
ct_int = string "int" >> return M.TInt

ct_nat :: Parser M.CT
ct_nat = string "nat" >> return M.TNat

ct_string :: Parser M.CT
ct_string = string "string" >> return M.TString

ct_tez :: Parser M.CT
ct_tez = string "tez" >> return M.TTez

ct_bool :: Parser M.CT
ct_bool = string "bool" >> return M.TBool

ct_keyhash :: Parser M.CT
ct_keyhash = string "key_hash" >> return M.TKeyHash

ct_timestamp :: Parser M.CT
ct_timestamp = string "timestamp" >> return M.TTimestamp
