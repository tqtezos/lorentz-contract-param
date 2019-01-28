{-# LANGUAGE OverloadedStrings #-}

module Language.Michelson.Typecheck where

import           Control.Monad.State.Lazy
import qualified Language.Michelson.Macro as Macro
import           Language.Michelson.Types (Contract (..), Data, FieldNote,
                                           I (..), Op (..), T (..), Type (..),
                                           TypeNote)
import qualified Language.Michelson.Types as M


initStackType :: Contract -> [Type]
initStackType c = [Type (T_pair (para c) (stor c)) Nothing Nothing]

data CodeST = CodeST { instructions :: [I], stack :: [Type]}

type Result = Either TypeError [Type]
data TypeError = TypeError I [Type] deriving Show

--typecheck :: Contract -> Result
--typecheck c = let instrs = Macro.flatten $ Macro.expand (code c) in
--  evalState codeST (CodeST instrs (initStackType c))

--run :: [I] -> [Type] -> Result
--run is ts = evalState codeST (CodeST is ts)
--
codeST :: State CodeST Result
codeST = do
  ins  <- gets instructions
  stk  <- gets stack
  case ins of
    []     -> return $ Right stk
    i:is   -> case applyI i stk of
      Right stk' -> put (CodeST is stk') >> codeST
      l          -> return l

notate :: TypeNote -> FieldNote -> Type -> Type
notate tn' fn' (Type t tn fn) = Type t tn' fn'

notateT :: TypeNote -> Type -> Type
notateT tn' (Type t tn fn) = Type t tn' fn

notateF :: FieldNote -> Type -> Type
notateF fn' (Type t tn fn) = Type t tn fn'

isField :: FieldNote -> Type -> Bool
isField fn (Type _ _ fn') = fn == fn'

applyI :: I -> [Type] -> Result
applyI i stk = case (i, stk) of
  (DROP, t:ts)              -> Right ts
  (DUP _, t:ts)             -> Right $ t:t:ts
  (SWAP, t:t':ts)           -> Right $ t':t:ts
  (PUSH _ t _, ts)          -> Right $ t:ts
  (SOME tn _ f, t:ts)       -> Right $ (notate tn f t):ts
  (NONE tn _  f t, ts)      -> Right $ (notate tn f t):ts
  --(IF_NONE bt bf, t:ts) ->
  --  let (Type (T_option a) _ _) = t
  --      tt = run bt (t:ts)
  --      ft = run bf (a:ts)
  --   in if tt == ft then Right $ tt:ts else Left $ TypeError i stk
  (UNIT tn _, ts)           -> Right $ (Type T_unit tn n):ts
  (PAIR tn _ f f', t:t':ts) ->
    Right $ (Type (T_pair (nF f t) (nF f' t')) tn n):ts
  (CAR vn fn, (Type (T_pair l r) _ _):xs) ->
    if isField fn l then Right $ l:xs else Left $ TypeError i stk
  (CDR vn fn, (Type (T_pair l r) _ _):xs) ->
    if isField fn r then Right $ r:xs else Left $ TypeError i stk
  (i, stk)                   -> Left $ TypeError i stk
  where
    n = Nothing
    nF = notateF
    nT = notateT
  --LEFT              TypeNote VarNote FieldNote FieldNote Type
  --RIGHT             TypeNote VarNote FieldNote FieldNote Type
  --IF_LEFT           [Op] [Op]
  --IF_RIGHT          [Op] [Op]
  --NIL               TypeNote VarNote Type
  --CONS              VarNote
  --IF_CONS           [Op] [Op]
  --SIZE              VarNote
  --EMPTY_SET         TypeNote VarNote Comparable
  --EMPTY_MAP         TypeNote VarNote Comparable Type
  --MAP               VarNote [Op]
  --ITER              VarNote [Op]
  --MEM               VarNote
  --GET               VarNote
  --UPDATE
  --IF                [Op] [Op]
  --LOOP              [Op]
  --LOOP_LEFT         [Op]
  --LAMBDA            VarNote Type Type [Op]
  --EXEC              VarNote
  --DIP               [Op]
  --FAILWITH
  --CAST              TypeNote VarNote
  --RENAME            VarNote
  --PACK              VarNote
  --UNPACK            VarNote Type
  --CONCAT            VarNote
  --SLICE             VarNote
  --ISNAT
  --ADD               VarNote
  --SUB               VarNote
  --MUL               VarNote
  --EDIV              VarNote
  --ABS               VarNote
  --NEG
  --MOD
  --LSL               VarNote
  --LSR               VarNote
  --OR                VarNote
  --AND               VarNote
  --XOR               VarNote
  --NOT               VarNote
  --COMPARE           VarNote
  --EQ                VarNote
  --NEQ               VarNote
  --LT                VarNote
  --GT                VarNote
  --LE                VarNote
  --GE                VarNote
  --INT               VarNote
  --SELF              VarNote
  --CONTRACT          Type
  --TRANSFER_TOKENS   VarNote
  --SET_DELEGATE
  --CREATE_ACCOUNT    VarNote VarNote
  --CREATE_CONTRACT   VarNote VarNote
  --CREATE_CONTRACT2  VarNote VarNote Contract
  --IMPLICIT_ACCOUNT  VarNote
  --NOW               VarNote
  --AMOUNT            VarNote
  --BALANCE           VarNote
  --CHECK_SIGNATURE   VarNote
  --SHA256            VarNote
  --SHA512            VarNote
  --BLAKE2B           VarNote
  --HASH_KEY          VarNote
  --STEPS_TO_QUOTA    VarNote
  --SOURCE            VarNote
  --SENDER            VarNote
  --ADDRESS           VarNote
