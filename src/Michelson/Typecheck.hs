module Michelson.Typecheck
  ( Result
  , TypeError (..)
  , typecheck
  ) where

import Michelson.Types (Contract(..), Instr, InstrAbstract(..), Op(..), T(..), Type(..), TypeAnn, noAnn)

initStackType :: Contract Op -> [Type]
initStackType c = [Type (T_pair noAnn noAnn (para c) (stor c)) noAnn]

data CodeST = CodeST { instructions :: [Instr], stack :: [Type]}

type Result = Either TypeError [Type]
data TypeError = TypeError Instr [Type] deriving Show

typecheck :: Contract Op -> Result
typecheck c =
  let instrs = unOp <$> code c
  in evalState codeST (CodeST instrs (initStackType c))

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

_notate :: TypeAnn -> Type -> Type
_notate tn' (Type t _tn) = Type t tn'

applyI :: Instr -> [Type] -> Result
applyI i stk = case (i, stk) of
  (DROP, _t:ts)             -> Right ts
  (DUP _, t:ts)             -> Right $ t:t:ts
  (SWAP, t:t':ts)           -> Right $ t':t:ts
  (PUSH _ t _, ts)          -> Right $ t:ts
  (SOME tn _ f, t:ts)       -> Right $ Type (T_option f t) tn : ts
  (NONE tn _  f t, ts)      -> Right $ Type (T_option f t) tn : ts
  --(IF_NONE bt bf, t:ts) ->
  --  let (Type (T_option f a) _ ) = t
  --      tt = run bt (t:ts)
  --      ft = run bf (a:ts)
  --   in if tt == ft then Right $ tt:ts else Left $ TypeError i stk
  (UNIT tn _, ts)           -> Right $ Type T_unit tn :ts
  (PAIR tn _ f f', t:t':ts) -> Right $ Type (T_pair f f' t t') tn:ts
  (CAR _ f, Type (T_pair l _ a _) _ : xs) -> if f == l then Right $ a:xs
                                             else Left $ TypeError i stk
  (CDR _ f, Type (T_pair _ r _ b) _ : xs) -> if f == r then Right $ b:xs
                                             else Left $ TypeError i stk
  _                         -> Left $ TypeError i stk

  --LEFT              TypeAnn VarNote FieldNote FieldNote Type
  --RIGHT             TypeAnn VarNote FieldNote FieldNote Type
  --IF_LEFT           [Op] [Op]
  --IF_RIGHT          [Op] [Op]
  --NIL               TypeAnn VarNote Type
  --CONS              VarNote
  --IF_CONS           [Op] [Op]
  --SIZE              VarNote
  --EMPTY_SET         TypeAnn VarNote Comparable
  --EMPTY_MAP         TypeAnn VarNote Comparable Type
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
  --CAST              TypeAnn VarNote
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
