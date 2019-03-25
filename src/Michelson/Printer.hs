module Language.Michelson.Printer where

import qualified Data.Text as T

import Michelson.Types

{-
pp_Contract :: Contract -> T.Text
pp_Contract (Contract p s c) = T.concat [pp_Parameter p, pp_Storage s, pp_Code c]

pp_Parameter :: Parameter -> Text
pp_Parameter (Parameter t) = T.concat ["parameter ", pp_T t, ";"]

pp_Storage :: Storage -> Text
pp_Storage (Storage t) = T.concat ["storage ", pp_T t, ";"]

pp_Code :: Code -> T.Text
pp_Code (Code s) = T.concat ["code ", pp_ISeq s, ";"]

pp_T :: T -> T.Text
pp_T (Tc ct) = pp_CT ct
pp_T TKey             = "key"
pp_T TUnit            = "unit"
pp_T TSignature       = "signature"
pp_T (TOption x)      = T.concat ["option ", pp_T x]
pp_T (TList x)        = T.concat ["list ", pp_T x]
pp_T (TSet x)         = T.concat ["set ", pp_CT x]
pp_T (TContract x)    = T.concat ["contract ", pp_T x]
pp_T (TPair x y)      = T.concat ["pair ", pp_T x, " ", ppT y]
pp_T (TOr x y)        = T.concat ["or ", pp_T x, " ", ppT y]
pp_T (TLambda x y)    = T.concat ["pair ", pp_T x, " ", ppT y]
pp_T (TMap x y)       = T.concat ["map ", pp_CT x, " ", ppT y]
pp_T (TBigMap x y)   = T.concat ["big_map ", pp_CT x, " ", ppT y]

pp_CT :: Un.CT -> T.Text
pp_CT CInt       = "int"
pp_CT CNat       = "nat"
pp_CT CString    = "int"
pp_CT CMutez     = "mutez"
pp_CT CBool      = "bool"
pp_CT CKeyHash  = "key_hash"
pp_CT CTimestamp = "int"

pp_ISeq :: Sequence Un.ISeq -> T.Text
pp_ISeq s = T.concat ["{", go s, "}"]
  where
    go Empty      = ""
    go (a :<| as) = T.append (pp_I a) (go as)

pp_I :: Un.I -> T.Text
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


pp_D :: Un.D -> T.Text
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
-}
