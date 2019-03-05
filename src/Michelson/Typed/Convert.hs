module Michelson.Typed.Convert
  ( unsafeValToValue
  ) where

import qualified Data.Map as M
import Data.Singletons (SingI(sing))

import Michelson.Typed.CValue
import Michelson.Typed.Extract (toMType)
import Michelson.Typed.Instr as Instr
import Michelson.Typed.Sing (fromSingCT, fromSingT)
import Michelson.Typed.T (CT(..), T(..))
import Michelson.Typed.Value
import qualified Michelson.Untyped as M
import Tezos.Address (formatAddress)
import Tezos.Core (unMutez)
import Tezos.Crypto (formatKeyHash, formatPublicKey, formatSignature)

-- | Function @unsafeValToValue@ converts typed @Val@ to untyped @Value@
-- from @Michelson.Untyped.Type@ module
--
-- VOp cannot be represented in @Value@ from untyped types, so calling this function
-- on it will cause an error
unsafeValToValue :: HasCallStack => Val (Instr cp) t -> M.Value (M.Op nop)
unsafeValToValue (VC cVal) = cValToValue cVal
unsafeValToValue (VKey b) = M.ValueString $ formatPublicKey b
unsafeValToValue VUnit = M.ValueUnit
unsafeValToValue (VSignature b) = M.ValueString $ formatSignature b
unsafeValToValue (VOption (Just x)) = M.ValueSome $ unsafeValToValue x
unsafeValToValue (VOption Nothing) = M.ValueNone
unsafeValToValue (VList l) = M.ValueSeq $ map unsafeValToValue l
unsafeValToValue (VSet s) = M.ValueSeq $ map cValToValue $ toList s
unsafeValToValue (VOp _) =
  error "unexpected unsafeValToValue call trying to convert VOp to untyped Value"
unsafeValToValue (VContract b) = M.ValueString $ formatAddress b
unsafeValToValue (VPair (l, r)) = M.ValuePair (unsafeValToValue l) (unsafeValToValue r)
unsafeValToValue (VOr (Left x)) = M.ValueLeft $ unsafeValToValue x
unsafeValToValue (VOr (Right x)) = M.ValueRight $ unsafeValToValue x
unsafeValToValue (VLam ops) = M.ValueLambda $ instrToOps ops
unsafeValToValue (VMap m) =
  M.ValueMap (map (\(k, v) -> M.Elt (cValToValue k) (unsafeValToValue v)) (M.toList m))
unsafeValToValue (VBigMap m) =
  M.ValueMap (map (\(k, v) -> M.Elt (cValToValue k) (unsafeValToValue v)) (M.toList m))

cValToValue :: CVal t -> M.Value (M.Op nop)
cValToValue cVal = case cVal of
  CvInt i -> M.ValueInt i
  CvNat i -> M.ValueInt $ toInteger i
  CvString s -> M.ValueString s
  CvBytes b -> M.ValueBytes $ M.InternalByteString b
  CvMutez m -> M.ValueInt $ toInteger $ unMutez m
  CvBool True -> M.ValueTrue
  CvBool False -> M.ValueFalse
  CvKeyHash h -> M.ValueString $ formatKeyHash h
  CvTimestamp t -> M.ValueString $ show t
  CvAddress a -> M.ValueString $ formatAddress a

instrToOps :: Instr cp inp out -> [M.Op nop]
instrToOps instr = M.Op <$> handleInstr instr
  where
    handleInstr :: Instr cp inp out -> [M.Instr nop]
    handleInstr (Seq i1 i2) = handleInstr i1 <> handleInstr i2
    handleInstr Nop = []
    handleInstr DROP = [M.DROP]
    handleInstr DUP = [M.DUP M.noAnn]
    handleInstr SWAP = [M.SWAP]
    handleInstr i@(PUSH val) = handle i
      where
        handle :: Instr cp inp (t ': s) -> [M.Instr nop]
        handle (PUSH _ :: Instr cp inp (t ': s)) =
          [M.PUSH M.noAnn (toMType $ fromSingT (sing @t)) (unsafeValToValue val)]
        handle _ = error "unexcepted call"
    handleInstr i@(NONE) = handle i
      where
        handle :: Instr cp inp ('T_option a ': inp) -> [M.Instr nop]
        handle (NONE :: Instr cp inp ('T_option a ': inp)) =
          [M.NONE M.noAnn M.noAnn M.noAnn (toMType $ fromSingT (sing @a))]
        handle _ = error "unexcepted call"
    handleInstr SOME = [M.SOME M.noAnn M.noAnn M.noAnn]
    handleInstr UNIT = [M.UNIT M.noAnn M.noAnn]
    handleInstr (IF_NONE i1 i2) = [M.IF_NONE (instrToOps i1) (instrToOps i2)]
    handleInstr PAIR = [M.PAIR M.noAnn M.noAnn M.noAnn M.noAnn]
    handleInstr CAR = [M.CAR M.noAnn M.noAnn]
    handleInstr CDR = [M.CDR M.noAnn M.noAnn]
    handleInstr i@(LEFT) = handle i
      where
        handle :: Instr cp (a ': s) ('T_or a b ': s) -> [M.Instr nop]
        handle (LEFT :: Instr cp (a ': s) ('T_or a b ': s)) =
          [M.LEFT M.noAnn M.noAnn M.noAnn M.noAnn (toMType $ fromSingT (sing @b))]
        handle _ = error "unexcepted call"
    handleInstr i@(RIGHT) = handle i
      where
        handle :: Instr cp (b ': s) ('T_or a b ': s) -> [M.Instr nop]
        handle (RIGHT :: Instr cp (b ': s) ('T_or a b ': s)) =
          [M.RIGHT M.noAnn M.noAnn M.noAnn M.noAnn (toMType $ fromSingT (sing @a))]
        handle _ = error "unexcepted call"
    handleInstr (IF_LEFT i1 i2) = [M.IF_LEFT (instrToOps i1) (instrToOps i2)]
    handleInstr (IF_RIGHT i1 i2) = [M.IF_RIGHT (instrToOps i1) (instrToOps i2)]
    handleInstr i@(NIL) = handle i
      where
        handle :: Instr cp s ('T_list p ': s) -> [M.Instr nop]
        handle (NIL :: Instr cp s ('T_list p ': s)) =
          [M.NIL M.noAnn M.noAnn (toMType $ fromSingT (sing @p))]
        handle _ = error "unexcepted call"
    handleInstr CONS = [M.CONS M.noAnn]
    handleInstr (IF_CONS i1 i2) = [M.IF_CONS (instrToOps i1) (instrToOps i2)]
    handleInstr SIZE = [M.SIZE M.noAnn]
    handleInstr i@(EMPTY_SET) = handle i
      where
        handle :: Instr cp s ('T_set e ': s) -> [M.Instr nop]
        handle (EMPTY_SET :: Instr cp s ('T_set e ': s)) =
          [M.EMPTY_SET M.noAnn M.noAnn (M.Comparable (fromSingCT (sing @e)) M.noAnn)]
        handle _ = error "unexcepted call"
    handleInstr i@(EMPTY_MAP) = handle i
      where
        handle :: Instr cp s ('T_map a b ': s) -> [M.Instr nop]
        handle (EMPTY_MAP :: Instr cp s ('T_map a b ': s)) =
          [M.EMPTY_MAP M.noAnn M.noAnn (M.Comparable (fromSingCT (sing @a)) M.noAnn)
           (toMType $ fromSingT (sing @b))
          ]
        handle _ = error "unexcepted call"
    handleInstr (MAP op) = [M.MAP M.noAnn $ instrToOps op]
    handleInstr (ITER op) = [M.ITER $ instrToOps op]
    handleInstr MEM = [M.MEM M.noAnn]
    handleInstr GET = [M.GET M.noAnn]
    handleInstr UPDATE = [M.UPDATE]
    handleInstr (IF op1 op2) = [M.IF (instrToOps op1) (instrToOps op2)]
    handleInstr (LOOP op) = [M.LOOP (instrToOps op)]
    handleInstr (LOOP_LEFT op) = [M.LOOP_LEFT (instrToOps op)]
    handleInstr i@(LAMBDA l) = handle i
      where
        handle :: Instr cp s ('T_lambda i o ': s) -> [M.Instr nop]
        handle (LAMBDA _ :: Instr cp s ('T_lambda i o ': s)) =
          [M.LAMBDA M.noAnn (toMType $ fromSingT (sing @i))
            (toMType $ fromSingT (sing @i)) (convertLambdaBody l)
          ]
        handle _ = error "unexcepted call"
        convertLambdaBody :: Val (Instr cp) ('T_lambda i o) -> [M.Op nop]
        convertLambdaBody (VLam ops) = instrToOps ops
    handleInstr EXEC = [M.EXEC M.noAnn]
    handleInstr (DIP op) = [M.DIP (instrToOps op)]
    handleInstr FAILWITH = [M.FAILWITH]
    handleInstr i@(CAST) = handle i
      where
        handle :: Instr cp (a ': s) (a ': s) -> [M.Instr nop]
        handle (CAST :: Instr cp (a ': s) (a ': s)) =
          [M.CAST M.noAnn (toMType $ fromSingT (sing @a))]
        handle _ = error "unexcepted call"
    handleInstr RENAME = [M.RENAME M.noAnn]
    handleInstr PACK = [M.PACK M.noAnn]
    handleInstr i@(UNPACK) = handle i
      where
        handle :: Instr cp ('T_c 'T_bytes ': s) ('T_option a ': s) -> [M.Instr nop]
        handle (UNPACK :: Instr cp ('T_c 'T_bytes ': s) ('T_option a ': s)) =
          [M.UNPACK M.noAnn (toMType $ fromSingT (sing @a))]
        handle _ = error "unexcepted call"
    handleInstr CONCAT = [M.CONCAT M.noAnn]
    handleInstr CONCAT' = [M.CONCAT M.noAnn]
    handleInstr SLICE = [M.SLICE M.noAnn]
    handleInstr ISNAT = [M.ISNAT M.noAnn]
    handleInstr ADD = [M.ADD M.noAnn]
    handleInstr SUB = [M.SUB M.noAnn]
    handleInstr MUL = [M.MUL M.noAnn]
    handleInstr EDIV = [M.EDIV M.noAnn]
    handleInstr ABS = [M.ABS M.noAnn]
    handleInstr NEG = [M.NEG]
    handleInstr LSL = [M.LSL M.noAnn]
    handleInstr LSR = [M.LSR M.noAnn]
    handleInstr OR = [M.OR M.noAnn]
    handleInstr AND = [M.AND M.noAnn]
    handleInstr XOR = [M.XOR M.noAnn]
    handleInstr NOT = [M.NOT M.noAnn]
    handleInstr COMPARE = [M.COMPARE M.noAnn]
    handleInstr Instr.EQ = [M.EQ M.noAnn]
    handleInstr NEQ = [M.NEQ M.noAnn]
    handleInstr Instr.LT = [M.LT M.noAnn]
    handleInstr Instr.GT = [M.GT M.noAnn]
    handleInstr LE = [M.LE M.noAnn]
    handleInstr GE = [M.GE M.noAnn]
    handleInstr INT = [M.INT M.noAnn]
    handleInstr SELF = [M.SELF M.noAnn]
    handleInstr i@(CONTRACT) = handle i
      where
        handle :: Instr cp ('T_c 'T_address ': s) ('T_option ('T_contract p) ': s)
               -> [M.Instr nop]
        handle (CONTRACT :: Instr cp ('T_c 'T_address ': s) ('T_option ('T_contract p) ': s)) =
          [M.CONTRACT M.noAnn (toMType $ fromSingT (sing @p))]
        handle _ = error "unexcepted call"
    handleInstr TRANSFER_TOKENS = [M.TRANSFER_TOKENS M.noAnn]
    handleInstr SET_DELEGATE = [M.SET_DELEGATE M.noAnn]
    handleInstr CREATE_ACCOUNT = [M.CREATE_ACCOUNT M.noAnn M.noAnn]
    handleInstr CREATE_CONTRACT = [M.CREATE_CONTRACT M.noAnn M.noAnn]
    handleInstr i@(CREATE_CONTRACT2 _) = handle i
      where
        handle :: Instr cp ('T_c 'T_key_hash ': 'T_option ('T_c 'T_key_hash)
                    ': 'T_c 'T_bool ': 'T_c 'T_bool ': 'T_c 'T_mutez ': g ': s)
                   ('T_operation ': 'T_c 'T_address ': s) -> [M.Instr nop]
        handle (CREATE_CONTRACT2 ops :: Instr cp ('T_c 'T_key_hash
                    ': 'T_option ('T_c 'T_key_hash)
                    ': 'T_c 'T_bool ': 'T_c 'T_bool ': 'T_c 'T_mutez ': g ': s)
                   ('T_operation ': 'T_c 'T_address ': s)) =
          case ops of
            (code :: Instr p '[ 'T_pair p g ] '[ 'T_pair ('T_list 'T_operation) g ]) ->
              let contract = M.Contract (toMType $ fromSingT (sing @p))
                    (toMType $ fromSingT (sing @g)) (instrToOps code) in
              [M.CREATE_CONTRACT2 M.noAnn M.noAnn contract]
        handle _ = error "unexcepted call"
    handleInstr IMPLICIT_ACCOUNT = [M.IMPLICIT_ACCOUNT M.noAnn]
    handleInstr NOW = [M.NOW M.noAnn]
    handleInstr AMOUNT = [M.AMOUNT M.noAnn]
    handleInstr BALANCE = [M.BALANCE M.noAnn]
    handleInstr CHECK_SIGNATURE = [M.CHECK_SIGNATURE M.noAnn]
    handleInstr SHA256 = [M.SHA256 M.noAnn]
    handleInstr SHA512 = [M.SHA512 M.noAnn]
    handleInstr BLAKE2B = [M.BLAKE2B M.noAnn]
    handleInstr HASH_KEY = [M.HASH_KEY M.noAnn]
    handleInstr STEPS_TO_QUOTA = [M.STEPS_TO_QUOTA M.noAnn]
    handleInstr SOURCE = [M.SOURCE M.noAnn]
    handleInstr SENDER = [M.SENDER M.noAnn]
    handleInstr ADDRESS = [M.ADDRESS M.noAnn]
