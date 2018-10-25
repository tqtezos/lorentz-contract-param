{-# LANGUAGE OverloadedStrings #-}

module Language.Michelson.Printer where


import qualified Language.Michelson.Types as M

import           Data.Sequence
import qualified Data.Text                as T

pp_SC :: M.SC -> T.Text
pp_SC (M.SC p s c) = T.concat [pp_Parameter p, pp_Storage s, pp_Code c]

pp_Parameter :: M.Parameter -> Text
pp_Parameter (M.Parameter t) = T.concat ["parameter ", pp_T t, ";"]

pp_Storage :: M.Storage -> Text
pp_Storage (M.Storage t) = T.concat ["storage ", pp_T t, ";"]

pp_Code :: M.Code -> T.Text
pp_Code (M.Code s) = T.concat ["code ", pp_ISeq s, ";"]

pp_T :: M.T -> T.Text
pp_T (T_comparable ct) = pp_CT ct
pp_T T_key             = "key"
pp_T T_unit            = "unit"
pp_T T_signature       = "signature"
pp_T (T_option x)      = T.concat ["option ", pp_T x]
pp_T (T_list x)        = T.concat ["list ", pp_T x]
pp_T (T_set x)         = T.concat ["set ", pp_CT x]
pp_T (T_contract x)    = T.concat ["contract ", pp_T x]
pp_T (T_pair x y)      = T.concat ["pair ", pp_T x, " ", ppT y]
pp_T (T_or x y)        = T.concat ["or ", pp_T x, " ", ppT y]
pp_T (T_lambda x y)    = T.concat ["pair ", pp_T x, " ", ppT y]
pp_T (T_map x y)       = T.concat ["map ", pp_CT x, " ", ppT y]
pp_T (T_big_map x y)   = T.concat ["big_map ", pp_CT x, " ", ppT y]

pp_CT :: M.CT -> T.Text
pp_CT T_int       = "int"
pp_CT T_nat       = "nat"
pp_CT T_string    = "int"
pp_CT T_mutez     = "mutez"
pp_CT T_bool      = "bool"
pp_CT T_key_hash  = "key_hash"
pp_CT T_timestamp = "int"


pp_ISeq :: Sequence M.ISeq -> T.Text
pp_ISeq s = T.concat ["{", go s, "}"]
  where
    go Empty      = ""
    go (a :<| as) = T.append (pp_I a) (go as)

pp_I :: M.I -> T.Text
pp_I DROP            = "DROP;"
pp_I DUP             = "DROP;"
pp_I SWAP            = "SWAP;"
pp_I (PUSH t d)      = T.concat ["PUSH ", pp_T t, " ", pp_D d, ";"]
pp_I SOME            = "SOME;"
pp_I (NONE t)        = T.concat ["NONE ", pp_T t, ";"]
pp_I (IF_NONE x y)   = T.concat ["IF_NONE ", pp_ISeq x, pp_ISeq y, ";"]
pp_I PAIR            = "PAIR;"
pp_I CAR             = "CAR;"
pp_I CDR             = "CDR;"
pp_I LEFT            = "LEFT;"
pp_I RIGHT           = "RIGHT;"
pp_I (IF_LEFT x y)   = T.concat ["IF_LEFT ", pp_ISeq x, pp_ISeq y, ";"]
pp_I (IF_RIGHT x y)  = T.concat ["IF_RIGHT ", pp_ISeq x, pp_ISeq y, ";"]
pp_I (NIL t)         = T.concat ["NIL ", pp_T t, ";"]
pp_I CONS            = "CONS;"
pp_I (IF_CONS x y)   = T.concat ["IF_CONS ", pp_ISeq x, pp_ISeq y, ";"]
pp_I (EMPTY_SET t)   = T.concat ["EMPTY_SET ", pp_T t, ";"]
pp_I (EMPTY_MAP c t) = T.concat ["EMPTY_MAP ", pp_CT c, " ", pp_T t, ";"]
pp_I (MAP s)         = T.concat ["MAP ", pp_ISeq s, ";"]
pp_I (ITER s)        = T.concat ["ITER ", pp_ISeq s, ";"]
pp_I MEM             = "MEM;"
pp_I GET             = "GET;"
pp_I UPDATE          = "UPDATE;"
pp_I (IF x y)        = T.concat ["IF ", pp_ISeq x, pp_ISeq y, ";"]
pp_I (LOOP s)        = T.concat ["LOOP ", pp_ISeq s, ";"]
pp_I (LOOP_LEFT s)   = T.concat ["LOOP_LEFT ", pp_ISeq s, ";"]
pp_I (LAMBDA t r s)  =
  T.concat ["LAMBDA ", pp_T t, " ", pp_T r, " ", pp_ISeq s, ";"]
pp_I EXEC = "EXEC;"
pp_I (DIP s) = T.concat ["DIP ", pp_ISeq s, ";"]
pp_I FAILWITH = "FAILWITH;"
pp_I CAST = "CAST;"
pp_I RENAME = "RENAME;"
pp_I CONCAT = "CONCAT;"
pp_I ADD = "ADD;"
pp_I SUB = "SUB;"
pp_I MUL = "MUL;"
pp_I DIV = "DIV;"
pp_I ABS = "ABS;"
pp_I NEG = "NEG;"
pp_I MOD = "MOD;"
pp_I LSL = "LSL;"
pp_I LSR = "LSR;"
pp_I OR = "OR;"
pp_I AND = "AND;"
pp_I NOT = "NOT;"
pp_I COMPARE = "COMPARE;"
pp_I EQ = "EQ;"
pp_I NEQ = "NEQ;"
pp_I LT = "LT;"
pp_I GT = "GT;"
pp_I LE = "LE;"
pp_I GE = "GE;"
pp_I INT = "INT;"
pp_I SELF = "SELF;"
pp_I TRANSFER_TOKENS = "TRANSFER_TOKENS;"
pp_I SET_DELEGATE = "SET_DELEGATE;"
pp_I CREATE_ACCOUNT = "CREATE_ACCOUNT;"
-- pp_I CREATE_CONTRACT = "CREATE_CONTRACT;"
pp_I (CREATE_CONTRACT s)= T.concat ["CREATE_CONTRACT ", pp_ISeq s, ";"]
pp_I IMPLICIT_ACCOUNT = "IMPLICIT_ACCOUNT;"
pp_I NOW = "NOW;"
pp_I AMOUNT = "AMOUNT;"
pp_I BALANCE = "BALANCE;"
pp_I CHECK_SIGNATURE = "CHECK_SIGNATURE;"
pp_I BLAKE2B = "BLAKE2B;"
pp_I HASH_KEY = "HASH_KEY;"
pp_I STEPS_TO_QUOTA = "STEPS_TO_QUOTA;"
pp_I SOURCE = "SOURCE;"
pp_I SENDER = "SENDER;"


pp_D :: M.D -> T.Text
-- todo literals
pp_D (LInt n) = T.pack $ show n
pp_D (LNat n) = T.pack $ show n
pp_D DUnit = "Unit"
pp_D DTrue = "True"
pp_D DFalse = "False"
pp_D (DPair l r) = T.concat ["(Pair ", pp_D l, " ", pp_D r, ")"]
pp_D (DLeft x)   = T.concat ["(Left ", pp_D x, ")"]
pp_D (DRight x)   = T.concat ["(Right ", pp_D x, ")"]
pp_D (DSome x)   = T.concat ["(Some ", pp_D x, ")"]
pp_D DNone       = "None"
-- todo: list, set, map, inst
