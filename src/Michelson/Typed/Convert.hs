module Michelson.Typed.Convert
  ( convertContract
  , instrToOps
  , unsafeValToValue
  , valToOpOrValue
  , Conversible (..)
  , ConversibleExt
  ) where

import qualified Data.Map as Map
import Data.Singletons (SingI(sing))

import Michelson.Typed.CValue
import Michelson.Typed.Extract (toUType)
import Michelson.Typed.Instr as Instr
import Michelson.Typed.Sing (fromSingCT, fromSingT)
import Michelson.Typed.T (CT(..), T(..))
import Michelson.Typed.Value
import qualified Michelson.Untyped as Un
import Tezos.Address (formatAddress)
import Tezos.Core (unMutez)
import Tezos.Crypto (formatKeyHash, formatPublicKey, formatSignature)

class Conversible ext1 ext2 where
  convert :: ext1 -> ext2

type ConversibleExt = Conversible (ExtT Instr) (Un.ExtU Un.InstrAbstract Un.Op)

convertContract
  :: forall param store . (SingI param, SingI store, ConversibleExt)
  => Contract param store -> Un.Contract Un.Op
convertContract contract =
  Un.Contract
    { para = toUType $ fromSingT (sing @param)
    , stor = toUType $ fromSingT (sing @store)
    , code = instrToOps contract
    }

-- | Function @unsafeValToValue@ converts typed @Val@ to untyped @Value@
-- from @Michelson.Untyped.Value@ module
--
-- VOp cannot be represented in @Value@ from untyped types, so calling this function
-- on it will cause an error
unsafeValToValue :: (ConversibleExt, HasCallStack) => Val Instr t -> Un.Value Un.Op
unsafeValToValue = fromMaybe (error err) . valToOpOrValue
  where
    err =
      "unexpected unsafeValToValue call trying to convert VOp to untyped Value"

-- | Convert a typed 'Val' to an untyped 'Value', or fail if it contains operations
-- which are unrepresentable there.
valToOpOrValue ::
     forall t . ConversibleExt
  => Val Instr t
  -> Maybe (Un.Value Un.Op)
valToOpOrValue = \case
  VC cVal -> Just $ cValToValue cVal
  VKey b -> Just $ Un.ValueString $ formatPublicKey b
  VUnit -> Just $ Un.ValueUnit
  VSignature b -> Just $ Un.ValueString $ formatSignature b
  VOption (Just x) -> Un.ValueSome <$> valToOpOrValue x
  VOption Nothing -> Just $ Un.ValueNone
  VList l -> Un.ValueSeq <$> mapM valToOpOrValue l
  VSet s -> Just $ Un.ValueSeq $ map cValToValue $ toList s
  VOp _op -> Nothing
  VContract b -> Just $ Un.ValueString $ formatAddress b
  VPair (l, r) -> Un.ValuePair <$> valToOpOrValue l <*> valToOpOrValue r
  VOr (Left x) -> Un.ValueLeft <$> valToOpOrValue x
  VOr (Right x) -> Un.ValueRight <$> valToOpOrValue x
  VLam ops -> Just $ Un.ValueLambda $ instrToOps ops
  VMap m ->
    fmap Un.ValueMap . forM (Map.toList m) $ \(k, v) ->
      Un.Elt (cValToValue k) <$> valToOpOrValue v
  VBigMap m ->
    fmap Un.ValueMap . forM (Map.toList m) $ \(k, v) ->
      Un.Elt (cValToValue k) <$> valToOpOrValue v

cValToValue :: CVal t -> Un.Value Un.Op
cValToValue cVal = case cVal of
  CvInt i -> Un.ValueInt i
  CvNat i -> Un.ValueInt $ toInteger i
  CvString s -> Un.ValueString s
  CvBytes b -> Un.ValueBytes $ Un.InternalByteString b
  CvMutez m -> Un.ValueInt $ toInteger $ unMutez m
  CvBool True -> Un.ValueTrue
  CvBool False -> Un.ValueFalse
  CvKeyHash h -> Un.ValueString $ formatKeyHash h
  CvTimestamp t -> Un.ValueString $ show t
  CvAddress a -> Un.ValueString $ formatAddress a

instrToOps :: ConversibleExt => Instr inp out -> [Un.Op]
instrToOps instr = Un.Op <$> handleInstr instr
  where
    handleInstr :: Instr inp out -> [Un.Instr]
    handleInstr (Seq i1 i2) = handleInstr i1 <> handleInstr i2
    handleInstr Nop = []
    handleInstr (Ext nop) = [Un.EXT $ convert nop]
    handleInstr DROP = [Un.DROP]
    handleInstr DUP = [Un.DUP Un.noAnn]
    handleInstr SWAP = [Un.SWAP]
    handleInstr i@(PUSH val) = handle i
      where
        handle :: Instr inp1 (t ': s) -> [Un.Instr]
        handle (PUSH _ :: Instr inp1 (t ': s)) =
          let value = unsafeValToValue val
              --- ^ safe because PUSH cannot have operation as argument
          in [Un.PUSH Un.noAnn (toUType $ fromSingT (sing @t)) value]
        handle _ = error "unexcepted call"
    handleInstr i@NONE = handle i
      where
        handle :: Instr inp1 ('T_option a ': inp1) -> [Un.Instr]
        handle (NONE :: Instr inp1 ('T_option a ': inp1)) =
          [Un.NONE Un.noAnn Un.noAnn Un.noAnn (toUType $ fromSingT (sing @a))]
        handle _ = error "unexcepted call"
    handleInstr SOME = [Un.SOME Un.noAnn Un.noAnn Un.noAnn]
    handleInstr UNIT = [Un.UNIT Un.noAnn Un.noAnn]
    handleInstr (IF_NONE i1 i2) = [Un.IF_NONE (instrToOps i1) (instrToOps i2)]
    handleInstr PAIR = [Un.PAIR Un.noAnn Un.noAnn Un.noAnn Un.noAnn]
    handleInstr CAR = [Un.CAR Un.noAnn Un.noAnn]
    handleInstr CDR = [Un.CDR Un.noAnn Un.noAnn]
    handleInstr i@LEFT = handle i
      where
        handle :: Instr (a ': s) ('T_or a b ': s) -> [Un.Instr]
        handle (LEFT :: Instr (a ': s) ('T_or a b ': s)) =
          [Un.LEFT Un.noAnn Un.noAnn Un.noAnn Un.noAnn (toUType $ fromSingT (sing @b))]
        handle _ = error "unexcepted call"
    handleInstr i@(RIGHT) = handle i
      where
        handle :: Instr (b ': s) ('T_or a b ': s) -> [Un.Instr]
        handle (RIGHT :: Instr (b ': s) ('T_or a b ': s)) =
          [Un.RIGHT Un.noAnn Un.noAnn Un.noAnn Un.noAnn (toUType $ fromSingT (sing @a))]
        handle _ = error "unexcepted call"
    handleInstr (IF_LEFT i1 i2) = [Un.IF_LEFT (instrToOps i1) (instrToOps i2)]
    handleInstr (IF_RIGHT i1 i2) = [Un.IF_RIGHT (instrToOps i1) (instrToOps i2)]
    handleInstr i@(NIL) = handle i
      where
        handle :: Instr s ('T_list p ': s) -> [Un.Instr]
        handle (NIL :: Instr s ('T_list p ': s)) =
          [Un.NIL Un.noAnn Un.noAnn (toUType $ fromSingT (sing @p))]
        handle _ = error "unexcepted call"
    handleInstr CONS = [Un.CONS Un.noAnn]
    handleInstr (IF_CONS i1 i2) = [Un.IF_CONS (instrToOps i1) (instrToOps i2)]
    handleInstr SIZE = [Un.SIZE Un.noAnn]
    handleInstr i@EMPTY_SET = handle i
      where
        handle :: Instr s ('T_set e ': s) -> [Un.Instr]
        handle (EMPTY_SET :: Instr s ('T_set e ': s)) =
          [Un.EMPTY_SET Un.noAnn Un.noAnn (Un.Comparable (fromSingCT (sing @e)) Un.noAnn)]
        handle _ = error "unexcepted call"
    handleInstr i@EMPTY_MAP = handle i
      where
        handle :: Instr s ('T_map a b ': s) -> [Un.Instr]
        handle (EMPTY_MAP :: Instr s ('T_map a b ': s)) =
          [Un.EMPTY_MAP Un.noAnn Un.noAnn (Un.Comparable (fromSingCT (sing @a)) Un.noAnn)
           (toUType $ fromSingT (sing @b))
          ]
        handle _ = error "unexcepted call"
    handleInstr (MAP op) = [Un.MAP Un.noAnn $ instrToOps op]
    handleInstr (ITER op) = [Un.ITER $ instrToOps op]
    handleInstr MEM = [Un.MEM Un.noAnn]
    handleInstr GET = [Un.GET Un.noAnn]
    handleInstr UPDATE = [Un.UPDATE]
    handleInstr (IF op1 op2) = [Un.IF (instrToOps op1) (instrToOps op2)]
    handleInstr (LOOP op) = [Un.LOOP (instrToOps op)]
    handleInstr (LOOP_LEFT op) = [Un.LOOP_LEFT (instrToOps op)]
    handleInstr i@(LAMBDA l) = handle i
      where
        handle :: Instr s ('T_lambda i o ': s) -> [Un.Instr]
        handle (LAMBDA _ :: Instr s ('T_lambda i o ': s)) =
          [Un.LAMBDA Un.noAnn (toUType $ fromSingT (sing @i))
            (toUType $ fromSingT (sing @i)) (convertLambdaBody l)
          ]
        handle _ = error "unexcepted call"
        convertLambdaBody :: Val Instr ('T_lambda i o) -> [Un.Op]
        convertLambdaBody (VLam ops) = instrToOps ops
    handleInstr EXEC = [Un.EXEC Un.noAnn]
    handleInstr (DIP op) = [Un.DIP (instrToOps op)]
    handleInstr FAILWITH = [Un.FAILWITH]
    handleInstr i@(CAST) = handle i
      where
        handle :: Instr (a ': s) (a ': s) -> [Un.Instr]
        handle (CAST :: Instr (a ': s) (a ': s)) =
          [Un.CAST Un.noAnn (toUType $ fromSingT (sing @a))]
        handle _ = error "unexcepted call"
    handleInstr RENAME = [Un.RENAME Un.noAnn]
    handleInstr PACK = [Un.PACK Un.noAnn]
    handleInstr i@(UNPACK) = handle i
      where
        handle :: Instr ('T_c 'T_bytes ': s) ('T_option a ': s) -> [Un.Instr]
        handle (UNPACK :: Instr ('T_c 'T_bytes ': s) ('T_option a ': s)) =
          [Un.UNPACK Un.noAnn (toUType $ fromSingT (sing @a))]
        handle _ = error "unexcepted call"
    handleInstr CONCAT = [Un.CONCAT Un.noAnn]
    handleInstr CONCAT' = [Un.CONCAT Un.noAnn]
    handleInstr SLICE = [Un.SLICE Un.noAnn]
    handleInstr ISNAT = [Un.ISNAT Un.noAnn]
    handleInstr ADD = [Un.ADD Un.noAnn]
    handleInstr SUB = [Un.SUB Un.noAnn]
    handleInstr MUL = [Un.MUL Un.noAnn]
    handleInstr EDIV = [Un.EDIV Un.noAnn]
    handleInstr ABS = [Un.ABS Un.noAnn]
    handleInstr NEG = [Un.NEG]
    handleInstr LSL = [Un.LSL Un.noAnn]
    handleInstr LSR = [Un.LSR Un.noAnn]
    handleInstr OR = [Un.OR Un.noAnn]
    handleInstr AND = [Un.AND Un.noAnn]
    handleInstr XOR = [Un.XOR Un.noAnn]
    handleInstr NOT = [Un.NOT Un.noAnn]
    handleInstr COMPARE = [Un.COMPARE Un.noAnn]
    handleInstr Instr.EQ = [Un.EQ Un.noAnn]
    handleInstr NEQ = [Un.NEQ Un.noAnn]
    handleInstr Instr.LT = [Un.LT Un.noAnn]
    handleInstr Instr.GT = [Un.GT Un.noAnn]
    handleInstr LE = [Un.LE Un.noAnn]
    handleInstr GE = [Un.GE Un.noAnn]
    handleInstr INT = [Un.INT Un.noAnn]
    handleInstr SELF = [Un.SELF Un.noAnn]
    handleInstr i@CONTRACT = handle i
      where
        handle :: Instr ('T_c 'T_address ': s) ('T_option ('T_contract p) ': s)
               -> [Un.Instr]
        handle (CONTRACT :: Instr ('T_c 'T_address ': s) ('T_option ('T_contract p) ': s)) =
          [Un.CONTRACT Un.noAnn (toUType $ fromSingT (sing @p))]
        handle _ = error "unexcepted call"
    handleInstr TRANSFER_TOKENS = [Un.TRANSFER_TOKENS Un.noAnn]
    handleInstr SET_DELEGATE = [Un.SET_DELEGATE Un.noAnn]
    handleInstr CREATE_ACCOUNT = [Un.CREATE_ACCOUNT Un.noAnn Un.noAnn]
    handleInstr CREATE_CONTRACT = [Un.CREATE_CONTRACT Un.noAnn Un.noAnn]
    handleInstr i@(CREATE_CONTRACT2 _) = handle i
      where
        handle :: Instr ('T_c 'T_key_hash ': 'T_option ('T_c 'T_key_hash)
                    ': 'T_c 'T_bool ': 'T_c 'T_bool ': 'T_c 'T_mutez ': g ': s)
                   ('T_operation ': 'T_c 'T_address ': s) -> [Un.Instr]
        handle (CREATE_CONTRACT2 ops :: Instr ('T_c 'T_key_hash
                    ': 'T_option ('T_c 'T_key_hash)
                    ': 'T_c 'T_bool ': 'T_c 'T_bool ': 'T_c 'T_mutez ': g ': s)
                   ('T_operation ': 'T_c 'T_address ': s)) =
          case ops of
            (code :: Instr '[ 'T_pair p g ] '[ 'T_pair ('T_list 'T_operation) g ]) ->
              let contract = Un.Contract (toUType $ fromSingT (sing @p))
                    (toUType $ fromSingT (sing @g)) (instrToOps code) in
              [Un.CREATE_CONTRACT2 Un.noAnn Un.noAnn contract]
        handle _ = error "unexcepted call"
    handleInstr IMPLICIT_ACCOUNT = [Un.IMPLICIT_ACCOUNT Un.noAnn]
    handleInstr NOW = [Un.NOW Un.noAnn]
    handleInstr AMOUNT = [Un.AMOUNT Un.noAnn]
    handleInstr BALANCE = [Un.BALANCE Un.noAnn]
    handleInstr CHECK_SIGNATURE = [Un.CHECK_SIGNATURE Un.noAnn]
    handleInstr SHA256 = [Un.SHA256 Un.noAnn]
    handleInstr SHA512 = [Un.SHA512 Un.noAnn]
    handleInstr BLAKE2B = [Un.BLAKE2B Un.noAnn]
    handleInstr HASH_KEY = [Un.HASH_KEY Un.noAnn]
    handleInstr STEPS_TO_QUOTA = [Un.STEPS_TO_QUOTA Un.noAnn]
    handleInstr SOURCE = [Un.SOURCE Un.noAnn]
    handleInstr SENDER = [Un.SENDER Un.noAnn]
    handleInstr ADDRESS = [Un.ADDRESS Un.noAnn]
