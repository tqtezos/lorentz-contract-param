{-# OPTIONS_GHC -fno-warn-orphans #-}

module Michelson.Typed.Convert
  ( convertContract
  , instrToOps
  , untypeValue
  ) where

import qualified Data.Map as Map
import Data.Singletons (SingI(sing))
import Fmt (pretty)

import Michelson.EqParam (eqParam1, eqParam2)
import Michelson.Typed.CValue
import Michelson.Typed.Extract (mkUType, toUType)
import Michelson.Typed.Instr as Instr
import Michelson.Typed.Scope
import Michelson.Typed.Sing (Sing(..), fromSingCT, fromSingT)
import Michelson.Typed.T (CT(..), T(..))
import Michelson.Typed.Value
import qualified Michelson.Untyped as U
import Tezos.Address (formatAddress)
import Tezos.Core (unMutez)
import Tezos.Crypto (formatKeyHash, formatPublicKey, formatSignature)
import Util.Peano

convertContract
  :: forall param store . (SingI param, SingI store)
  => Contract param store -> U.Contract
convertContract contract =
  U.Contract
    { para = toUType $ fromSingT (sing @param)
    , stor = toUType $ fromSingT (sing @store)
    , code = instrToOps contract
    }

-- | Convert a typed 'Val' to an untyped 'Value'.
--
-- For full isomorphism type of the given 'Val' should not contain
-- 'TOperation' - a compile error will be raised otherwise.
-- You can analyse its presence with 'checkOpPresence' function.
untypeValue ::
     forall t . (SingI t, HasNoOp t)
  => Value' Instr t
  -> U.Value
untypeValue val = case (val, sing @t) of
  (VC cVal, _) ->
    untypeCValue cVal
  (VKey b, _) ->
    U.ValueString $ formatPublicKey b
  (VUnit, _) ->
    U.ValueUnit
  (VSignature b, _) ->
    U.ValueString $ formatSignature b
  (VOption (Just x), STOption _) ->
    U.ValueSome (untypeValue x)
  (VOption Nothing, STOption _) ->
    U.ValueNone
  (VList l, STList _) ->
    vList U.ValueSeq $ map untypeValue l
  (VSet s, _) ->
    vList U.ValueSeq $ map untypeCValue $ toList s
  (VContract b, _) ->
    U.ValueString $ formatAddress b

  (VPair (l, r), STPair lt _) ->
    case checkOpPresence lt of
      OpAbsent -> U.ValuePair (untypeValue l) (untypeValue r)

  (VOr (Left x), STOr lt _) ->
    case checkOpPresence lt of
      OpAbsent -> U.ValueLeft (untypeValue x)

  (VOr (Right x), STOr lt _) ->
    case checkOpPresence lt of
      OpAbsent -> U.ValueRight (untypeValue x)

  (VLam (ops :: Instr '[inp] '[out]), _) ->
    vList U.ValueLambda $ instrToOps ops

  (VMap m, STMap _ vt) ->
    case checkOpPresence vt of
      OpAbsent ->
        vList U.ValueMap $ Map.toList m <&> \(k, v) ->
        U.Elt (untypeCValue k) (untypeValue v)

  (VBigMap m, STBigMap _ vt) ->
    case checkOpPresence vt of
      OpAbsent ->
        vList U.ValueMap $ Map.toList m <&> \(k, v) ->
        U.Elt (untypeCValue k) (untypeValue v)
  where
    vList ctor = maybe U.ValueNil ctor . nonEmpty
untypeCValue :: CValue t -> U.Value
untypeCValue cVal = case cVal of
  CvInt i -> U.ValueInt i
  CvNat i -> U.ValueInt $ toInteger i
  CvString s -> U.ValueString s
  CvBytes b -> U.ValueBytes $ U.InternalByteString b
  CvMutez m -> U.ValueInt $ toInteger $ unMutez m
  CvBool True -> U.ValueTrue
  CvBool False -> U.ValueFalse
  CvKeyHash h -> U.ValueString $ formatKeyHash h
  CvTimestamp t -> U.ValueString $ pretty t
  CvAddress a -> U.ValueString $ formatAddress a

instrToOps :: Instr inp out -> [U.ExpandedOp]
instrToOps instr = case instr of
  Seq i1 i2 -> instrToOps i1 <> instrToOps i2
  Nested sq -> one $ U.SeqEx $ instrToOps sq
  i -> U.PrimEx <$> handleInstr i
  -- TODO pva701: perphaps, typed instr has to hold a position too
  -- to make it possible to report a precise location of a runtime error
  where
    handleInstr :: Instr inp out -> [U.ExpandedInstr]
    handleInstr (Seq _ _) = error "impossible"
    handleInstr Nop = []
    handleInstr (Ext (nop :: ExtInstr inp)) = [U.EXT $ extInstrToOps nop]
    handleInstr (Nested _) = error "impossible"
    handleInstr DROP = [U.DROP]
    handleInstr DUP = [U.DUP U.noAnn]
    handleInstr SWAP = [U.SWAP]
    handleInstr i@(PUSH val) | _ :: Instr inp1 (t ': s) <- i =
      let value = untypeValue val
      in [U.PUSH U.noAnn (toUType $ fromSingT (sing @t)) value]
    handleInstr i@NONE | _ :: Instr inp1 ('TOption a ': inp1) <- i =
      [U.NONE U.noAnn U.noAnn U.noAnn (toUType $ fromSingT (sing @a))]
    handleInstr SOME = [U.SOME U.noAnn U.noAnn U.noAnn]
    handleInstr UNIT = [U.UNIT U.noAnn U.noAnn]
    handleInstr (IF_NONE i1 i2) = [U.IF_NONE (instrToOps i1) (instrToOps i2)]
    handleInstr PAIR = [U.PAIR U.noAnn U.noAnn U.noAnn U.noAnn]
    handleInstr CAR = [U.CAR U.noAnn U.noAnn]
    handleInstr CDR = [U.CDR U.noAnn U.noAnn]
    handleInstr i@LEFT | _ :: Instr (a ': s) ('TOr a b ': s) <- i =
      [U.LEFT U.noAnn U.noAnn U.noAnn U.noAnn (toUType $ fromSingT (sing @b))]
    handleInstr i@RIGHT | _ :: Instr (b ': s) ('TOr a b ': s) <- i =
      [U.RIGHT U.noAnn U.noAnn U.noAnn U.noAnn (toUType $ fromSingT (sing @a))]
    handleInstr (IF_LEFT i1 i2) = [U.IF_LEFT (instrToOps i1) (instrToOps i2)]
    handleInstr i@NIL | _ :: Instr s ('TList p ': s) <- i =
      [U.NIL U.noAnn U.noAnn (toUType $ fromSingT (sing @p))]
    handleInstr CONS = [U.CONS U.noAnn]
    handleInstr (IF_CONS i1 i2) = [U.IF_CONS (instrToOps i1) (instrToOps i2)]
    handleInstr SIZE = [U.SIZE U.noAnn]
    handleInstr i@EMPTY_SET | _ :: Instr s ('TSet e ': s) <- i =
      [U.EMPTY_SET U.noAnn U.noAnn (U.Comparable (fromSingCT (sing @e)) U.noAnn)]
    handleInstr i@EMPTY_MAP | _ :: Instr s ('TMap a b ': s) <- i =
      [U.EMPTY_MAP U.noAnn U.noAnn (U.Comparable (fromSingCT (sing @a)) U.noAnn)
       (toUType $ fromSingT (sing @b))
      ]
    handleInstr (MAP op) = [U.MAP U.noAnn $ instrToOps op]
    handleInstr (ITER op) = [U.ITER $ instrToOps op]
    handleInstr MEM = [U.MEM U.noAnn]
    handleInstr GET = [U.GET U.noAnn]
    handleInstr UPDATE = [U.UPDATE]
    handleInstr (IF op1 op2) = [U.IF (instrToOps op1) (instrToOps op2)]
    handleInstr (LOOP op) = [U.LOOP (instrToOps op)]
    handleInstr (LOOP_LEFT op) = [U.LOOP_LEFT (instrToOps op)]
    handleInstr i@(LAMBDA {}) | LAMBDA (VLam l) :: Instr s ('TLambda i o ': s) <- i =
      [U.LAMBDA U.noAnn (toUType $ fromSingT (sing @i))
       (toUType $ fromSingT (sing @i)) (instrToOps l)
      ]
    handleInstr EXEC = [U.EXEC U.noAnn]
    handleInstr (DIP op) = [U.DIP (instrToOps op)]
    handleInstr FAILWITH = [U.FAILWITH]
    handleInstr i@CAST | _ :: Instr (a ': s) (a ': s) <- i =
      [U.CAST U.noAnn (toUType $ fromSingT (sing @a))]
    handleInstr RENAME = [U.RENAME U.noAnn]
    handleInstr PACK = [U.PACK U.noAnn]
    handleInstr i@UNPACK
      | _ :: Instr ('Tc 'CBytes ': s) ('TOption a ': s) <- i =
          [U.UNPACK U.noAnn (toUType $ fromSingT (sing @a))]
    handleInstr CONCAT = [U.CONCAT U.noAnn]
    handleInstr CONCAT' = [U.CONCAT U.noAnn]
    handleInstr SLICE = [U.SLICE U.noAnn]
    handleInstr ISNAT = [U.ISNAT U.noAnn]
    handleInstr ADD = [U.ADD U.noAnn]
    handleInstr SUB = [U.SUB U.noAnn]
    handleInstr MUL = [U.MUL U.noAnn]
    handleInstr EDIV = [U.EDIV U.noAnn]
    handleInstr ABS = [U.ABS U.noAnn]
    handleInstr NEG = [U.NEG]
    handleInstr LSL = [U.LSL U.noAnn]
    handleInstr LSR = [U.LSR U.noAnn]
    handleInstr OR = [U.OR U.noAnn]
    handleInstr AND = [U.AND U.noAnn]
    handleInstr XOR = [U.XOR U.noAnn]
    handleInstr NOT = [U.NOT U.noAnn]
    handleInstr COMPARE = [U.COMPARE U.noAnn]
    handleInstr Instr.EQ = [U.EQ U.noAnn]
    handleInstr NEQ = [U.NEQ U.noAnn]
    handleInstr Instr.LT = [U.LT U.noAnn]
    handleInstr Instr.GT = [U.GT U.noAnn]
    handleInstr LE = [U.LE U.noAnn]
    handleInstr GE = [U.GE U.noAnn]
    handleInstr INT = [U.INT U.noAnn]
    handleInstr SELF = [U.SELF U.noAnn]
    handleInstr i@(CONTRACT nt)
      | _ :: Instr ('Tc 'CAddress ': s) ('TOption ('TContract p) ': s) <- i =
          [U.CONTRACT (U.noAnn) (mkUType (sing @p) nt)]
    handleInstr TRANSFER_TOKENS = [U.TRANSFER_TOKENS U.noAnn]
    handleInstr SET_DELEGATE = [U.SET_DELEGATE U.noAnn]
    handleInstr CREATE_ACCOUNT = [U.CREATE_ACCOUNT U.noAnn U.noAnn]
    handleInstr i@(CREATE_CONTRACT ops)
      | _ :: Instr
          (  'Tc 'CKeyHash
          ': 'TOption ('Tc 'CKeyHash)
          ': 'Tc 'CBool
          ': 'Tc 'CBool
          ': 'Tc 'CMutez
          ': g
          ': s)
          ('TOperation ': 'Tc 'CAddress ': s) <- i
      , code :: Instr '[ 'TPair p g] '[ 'TPair ('TList 'TOperation) g] <- ops =
        let contract = U.Contract (toUType $ fromSingT (sing @p))
              (toUType $ fromSingT (sing @g)) (instrToOps code)
        in [U.CREATE_CONTRACT U.noAnn U.noAnn contract]
    handleInstr IMPLICIT_ACCOUNT = [U.IMPLICIT_ACCOUNT U.noAnn]
    handleInstr NOW = [U.NOW U.noAnn]
    handleInstr AMOUNT = [U.AMOUNT U.noAnn]
    handleInstr BALANCE = [U.BALANCE U.noAnn]
    handleInstr CHECK_SIGNATURE = [U.CHECK_SIGNATURE U.noAnn]
    handleInstr SHA256 = [U.SHA256 U.noAnn]
    handleInstr SHA512 = [U.SHA512 U.noAnn]
    handleInstr BLAKE2B = [U.BLAKE2B U.noAnn]
    handleInstr HASH_KEY = [U.HASH_KEY U.noAnn]
    handleInstr STEPS_TO_QUOTA = [U.STEPS_TO_QUOTA U.noAnn]
    handleInstr SOURCE = [U.SOURCE U.noAnn]
    handleInstr SENDER = [U.SENDER U.noAnn]
    handleInstr ADDRESS = [U.ADDRESS U.noAnn]

untypeStackRef :: StackRef s -> U.StackRef
untypeStackRef (StackRef n) = U.StackRef (peanoVal n)

untypePrintComment :: PrintComment s -> U.PrintComment
untypePrintComment (PrintComment pc) = U.PrintComment $ map (second untypeStackRef) pc

extInstrToOps :: ExtInstr s -> U.ExtInstrAbstract U.ExpandedOp
extInstrToOps = \case
  PRINT pc -> U.UPRINT (untypePrintComment pc)
  TEST_ASSERT (TestAssert nm pc i) ->
    U.UTEST_ASSERT $ U.TestAssert nm (untypePrintComment pc) (instrToOps i)

-- It's an orphan instance, but it's better than checking all cases manually.
-- We can also move this convertion to the place where `Instr` is defined,
-- but then there will be a very large module (as we'll have to move a lot of
-- stuff as well).
instance Eq (Instr inp out) where
  i1 == i2 = instrToOps i1 == instrToOps i2

instance Typeable s => Eq (TestAssert s) where
  TestAssert   name1 pattern1 instr1
    ==
    TestAssert name2 pattern2 instr2
    = and
    [ name1 == name2
    , pattern1 `eqParam1` pattern2
    , instr1 `eqParam2` instr2
    ]

deriving instance Typeable s => Eq (ExtInstr s)
