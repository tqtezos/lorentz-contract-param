{-# OPTIONS -Wno-missing-export-lists #-}

module Lorentz.Contracts.Util.Strip where

import Michelson.Macro (ParsedOp(..), LetMacro(..), Macro(..), PairStruct(..))
import Prelude
import Util.Default (Default(..))
import qualified Michelson.Untyped.Contract as U
import qualified Michelson.Untyped.Ext as U
import qualified Michelson.Untyped.Instr as U
import qualified Michelson.Untyped.Type as U

-- | Strip annotations from @`U.Contract'` `ParsedOp`@
stripContractAnns :: U.Contract' ParsedOp -> U.Contract' ParsedOp
stripContractAnns (U.Contract para' stor' code') =
  U.Contract
    (stripTypeAnns para')
    (stripTypeAnns stor')
    (code' >>= stripParsedOpAnns)

-- | Strip annotations from `U.Comparable`
stripComparableAnns :: U.Comparable -> U.Comparable
stripComparableAnns (U.Comparable cType' _) = U.Comparable cType' def

-- | Strip annotations from `U.T`
stripTAnns :: U.T -> U.T
stripTAnns (U.TOption type') = U.TOption (stripTypeAnns type')
stripTAnns (U.TList type') = U.TList (stripTypeAnns type')
stripTAnns (U.TSet comparable') = U.TSet (stripComparableAnns comparable')
stripTAnns (U.TContract type') = U.TContract (stripTypeAnns type')
stripTAnns (U.TPair _ _ typeX typeY) =
  U.TPair def def (stripTypeAnns typeX) (stripTypeAnns typeY)
stripTAnns (U.TOr _ _ typeX typeY) =
  U.TOr def def (stripTypeAnns typeX) (stripTypeAnns typeY)
stripTAnns (U.TLambda typeX typeY) =
  U.TLambda (stripTypeAnns typeX) (stripTypeAnns typeY)
stripTAnns (U.TMap comparable' type') =
  U.TMap (stripComparableAnns comparable') (stripTypeAnns type')
stripTAnns (U.TBigMap comparable' type') =
  U.TMap (stripComparableAnns comparable') (stripTypeAnns type')
stripTAnns t@(U.Tc _) = t
stripTAnns t@U.TKey = t
stripTAnns t@U.TUnit = t
stripTAnns t@U.TChainId = t
stripTAnns t@U.TSignature = t
stripTAnns t@U.TOperation = t

-- | Strip annotations from `U.Type`
stripTypeAnns :: U.Type -> U.Type
stripTypeAnns (U.Type t tAnn) = U.Type (stripTAnns t) (def `asTypeOf` tAnn)

-- | Strip annotations from `ParsedOp` and join `Seq`'s of `ParsedOp`'s
stripParsedOpAnns :: ParsedOp -> [ParsedOp]
stripParsedOpAnns (Prim parsedInstr' _) =
  [Prim (stripInstrAbstractAnns parsedInstr') def]
stripParsedOpAnns (Mac macro' _) = [Mac (stripMacroAnns macro') def]
stripParsedOpAnns (LMac letMacro' _) = [LMac (stripLetMacroAnns letMacro') def]
stripParsedOpAnns (Seq parsedOps' _) = parsedOps' >>= stripParsedOpAnns

-- | Strip annotations from `ExtInstrAbstract`
stripExtInstrAbstractAnns ::
     U.ExtInstrAbstract ParsedOp -> U.ExtInstrAbstract ParsedOp
stripExtInstrAbstractAnns (U.FN str' stackFn' parsedOps') =
  U.FN str' stackFn' (parsedOps' >>= stripParsedOpAnns)
stripExtInstrAbstractAnns extInstrAbstract' = extInstrAbstract'

-- | Strip annotations from `InstrAbstract`
stripInstrAbstractAnns :: U.InstrAbstract ParsedOp -> U.InstrAbstract ParsedOp
stripInstrAbstractAnns (U.ABS _) = U.ABS def
stripInstrAbstractAnns (U.ADD _) = U.ADD def
stripInstrAbstractAnns (U.ADDRESS _) = U.ADDRESS def
stripInstrAbstractAnns (U.CHAIN_ID _) = U.CHAIN_ID def
stripInstrAbstractAnns (U.AMOUNT _) = U.AMOUNT def
stripInstrAbstractAnns (U.AND _) = U.AND def
stripInstrAbstractAnns (U.BALANCE _) = U.BALANCE def
stripInstrAbstractAnns (U.BLAKE2B _) = U.BLAKE2B def
stripInstrAbstractAnns (U.CAR _ _) = U.CAR def def
stripInstrAbstractAnns (U.CAST _ type') = U.CAST def (stripTypeAnns type')
stripInstrAbstractAnns (U.CDR _ _) = U.CDR def def
stripInstrAbstractAnns (U.CHECK_SIGNATURE _) = U.CHECK_SIGNATURE def
stripInstrAbstractAnns (U.COMPARE _) = U.COMPARE def
stripInstrAbstractAnns (U.CONCAT _) = U.CONCAT def
stripInstrAbstractAnns (U.CONS _) = U.CONS def
stripInstrAbstractAnns (U.CONTRACT _ _ type') = U.CONTRACT def def (stripTypeAnns type')
stripInstrAbstractAnns (U.CREATE_CONTRACT _ _ contract') =
  U.CREATE_CONTRACT def def (stripContractAnns contract')
stripInstrAbstractAnns (U.DIP parsedOps') =
  U.DIP (parsedOps' >>= stripParsedOpAnns)
stripInstrAbstractAnns (U.DIPN x parsedOps') =
  U.DIPN x (parsedOps' >>= stripParsedOpAnns)
stripInstrAbstractAnns (U.DUP _) = U.DUP def
stripInstrAbstractAnns (U.DROPN _) = U.DROPN def
stripInstrAbstractAnns (U.DIG x) = U.DIG x
stripInstrAbstractAnns (U.DUG x) = U.DUG x
stripInstrAbstractAnns (U.EDIV _) = U.EDIV def
stripInstrAbstractAnns (U.EMPTY_MAP _ _ comparable' type') =
  U.EMPTY_MAP def def (stripComparableAnns comparable') (stripTypeAnns type')
stripInstrAbstractAnns (U.EMPTY_BIG_MAP _ _ comparable' type') =
  U.EMPTY_BIG_MAP def def (stripComparableAnns comparable') (stripTypeAnns type')
stripInstrAbstractAnns (U.EMPTY_SET _ _ comparable') =
  U.EMPTY_SET def def (stripComparableAnns comparable')
stripInstrAbstractAnns (U.EQ _) = U.EQ def
stripInstrAbstractAnns (U.EXEC _) = U.EXEC def
stripInstrAbstractAnns (U.APPLY _) = U.APPLY def
stripInstrAbstractAnns (U.EXT extInstrAbstract') =
  U.EXT $ stripExtInstrAbstractAnns extInstrAbstract'
stripInstrAbstractAnns (U.GE _) = U.GE def
stripInstrAbstractAnns (U.GET _) = U.GET def
stripInstrAbstractAnns (U.GT _) = U.GT def
stripInstrAbstractAnns (U.HASH_KEY _) = U.HASH_KEY def
stripInstrAbstractAnns (U.IF parsedOpsX parsedOpsY) =
  U.IF (parsedOpsX >>= stripParsedOpAnns) (parsedOpsY >>= stripParsedOpAnns)
stripInstrAbstractAnns (U.IF_CONS parsedOpsX parsedOpsY) =
  U.IF_CONS
    (parsedOpsX >>= stripParsedOpAnns)
    (parsedOpsY >>= stripParsedOpAnns)
stripInstrAbstractAnns (U.IF_LEFT parsedOpsX parsedOpsY) =
  U.IF_LEFT
    (parsedOpsX >>= stripParsedOpAnns)
    (parsedOpsY >>= stripParsedOpAnns)
stripInstrAbstractAnns (U.IF_NONE parsedOpsX parsedOpsY) =
  U.IF_NONE
    (parsedOpsX >>= stripParsedOpAnns)
    (parsedOpsY >>= stripParsedOpAnns)
stripInstrAbstractAnns (U.IMPLICIT_ACCOUNT _) = U.IMPLICIT_ACCOUNT def
stripInstrAbstractAnns (U.INT _) = U.INT def
stripInstrAbstractAnns (U.ISNAT _) = U.ISNAT def
stripInstrAbstractAnns (U.ITER parsedOps') =
  U.ITER (parsedOps' >>= stripParsedOpAnns)
stripInstrAbstractAnns (U.LAMBDA _ typeX typeY parsedOps') =
  U.LAMBDA
    def
    (stripTypeAnns typeX)
    (stripTypeAnns typeY)
    (parsedOps' >>= stripParsedOpAnns)
stripInstrAbstractAnns (U.LE _) = U.LE def
stripInstrAbstractAnns (U.LEFT _ _ _ _ type') =
  U.LEFT def def def def (stripTypeAnns type')
stripInstrAbstractAnns (U.LOOP parsedOps') =
  U.LOOP (parsedOps' >>= stripParsedOpAnns)
stripInstrAbstractAnns (U.LOOP_LEFT parsedOps') =
  U.LOOP_LEFT (parsedOps' >>= stripParsedOpAnns)
stripInstrAbstractAnns (U.LSL _) = U.LSL def
stripInstrAbstractAnns (U.LSR _) = U.LSR def
stripInstrAbstractAnns (U.LT _) = U.LT def
stripInstrAbstractAnns (U.MAP _ parsedOps') =
  U.MAP def (parsedOps' >>= stripParsedOpAnns)
stripInstrAbstractAnns (U.MEM _) = U.MEM def
stripInstrAbstractAnns (U.MUL _) = U.MUL def
stripInstrAbstractAnns (U.NEQ _) = U.NEQ def
stripInstrAbstractAnns (U.NIL _ _ type') = U.NIL def def (stripTypeAnns type')
stripInstrAbstractAnns (U.NONE _ _ type') =
  U.NONE def def (stripTypeAnns type')
stripInstrAbstractAnns (U.NOT _) = U.NOT def
stripInstrAbstractAnns (U.NOW _) = U.NOW def
stripInstrAbstractAnns (U.OR _) = U.OR def
stripInstrAbstractAnns (U.PACK _) = U.PACK def
stripInstrAbstractAnns (U.PAIR _ _ _ _) = U.PAIR def def def def
stripInstrAbstractAnns (U.PUSH _ type' value') =
  U.PUSH def (stripTypeAnns type') value'
stripInstrAbstractAnns (U.RENAME _) = U.RENAME def
stripInstrAbstractAnns (U.RIGHT _ _ _ _ type') =
  U.RIGHT def def def def (stripTypeAnns type')
stripInstrAbstractAnns (U.SELF _) = U.SELF def
stripInstrAbstractAnns (U.SENDER _) = U.SENDER def
stripInstrAbstractAnns (U.SET_DELEGATE _) = U.SET_DELEGATE def
stripInstrAbstractAnns (U.SHA256 _) = U.SHA256 def
stripInstrAbstractAnns (U.SHA512 _) = U.SHA512 def
stripInstrAbstractAnns (U.SIZE _) = U.SIZE def
stripInstrAbstractAnns (U.SLICE _) = U.SLICE def
stripInstrAbstractAnns (U.SOME _ _) = U.SOME def def
stripInstrAbstractAnns (U.SOURCE _) = U.SOURCE def
stripInstrAbstractAnns (U.STEPS_TO_QUOTA _) = U.STEPS_TO_QUOTA def
stripInstrAbstractAnns (U.SUB _) = U.SUB def
stripInstrAbstractAnns (U.TRANSFER_TOKENS _) = U.TRANSFER_TOKENS def
stripInstrAbstractAnns (U.UNIT _ _) = U.UNIT def def
stripInstrAbstractAnns (U.UNPACK _ _ type') = U.UNPACK def def (stripTypeAnns type')
stripInstrAbstractAnns (U.XOR _) = U.XOR def
stripInstrAbstractAnns U.DROP = U.DROP
stripInstrAbstractAnns U.FAILWITH = U.FAILWITH
stripInstrAbstractAnns (U.NEG _) = U.NEG def
stripInstrAbstractAnns U.SWAP = U.SWAP
stripInstrAbstractAnns (U.UPDATE _) = U.UPDATE def

-- | Strip annotations from `PairStruct`
stripPairStructAnns :: PairStruct -> PairStruct
stripPairStructAnns (F (_, _)) = F (def, def)
stripPairStructAnns (P xs ys) =
  P (stripPairStructAnns xs) (stripPairStructAnns ys)

-- | Strip annotations from `Macro`
stripMacroAnns :: Macro -> Macro
stripMacroAnns (CASE cases') = CASE $ concatMap stripParsedOpAnns <$> cases'
stripMacroAnns (TAG n types') = TAG n $ stripTypeAnns <$> types'
stripMacroAnns (ACCESS n i) = ACCESS n i
stripMacroAnns (SET n i) = SET n i
stripMacroAnns (CONSTRUCT opSeqs) = CONSTRUCT $ (>>= stripParsedOpAnns) <$> opSeqs
stripMacroAnns (VIEW parsedOps') = VIEW $ parsedOps' >>= stripParsedOpAnns
stripMacroAnns (VOID parsedOps') = VOID $ parsedOps' >>= stripParsedOpAnns
stripMacroAnns (CMP parsedInstr' _) =
  CMP (stripInstrAbstractAnns parsedInstr') def
stripMacroAnns (IFX parsedInstr' parsedOpsX parsedOpsY) =
  IFX
    (stripInstrAbstractAnns parsedInstr')
    (parsedOpsX >>= stripParsedOpAnns)
    (parsedOpsY >>= stripParsedOpAnns)
stripMacroAnns (IFCMP parsedInstr' _ parsedOpsX parsedOpsY) =
  IFCMP
    (stripInstrAbstractAnns parsedInstr')
    def
    (parsedOpsX >>= stripParsedOpAnns)
    (parsedOpsY >>= stripParsedOpAnns)
stripMacroAnns (PAPAIR pairStruct' _ _) =
  PAPAIR (stripPairStructAnns pairStruct') def def
stripMacroAnns (UNPAIR pairStruct') = UNPAIR (stripPairStructAnns pairStruct')
stripMacroAnns (CADR cadrStructs' _ _) = CADR cadrStructs' def def
stripMacroAnns (SET_CADR cadrStructs' _ _) = SET_CADR cadrStructs' def def
stripMacroAnns (MAP_CADR cadrStructs' _ _ parsedOps') =
  MAP_CADR cadrStructs' def def (parsedOps' >>= stripParsedOpAnns)
stripMacroAnns (DIIP n parsedOps') = DIIP n (parsedOps' >>= stripParsedOpAnns)
stripMacroAnns (DUUP n _) = DUUP n def
stripMacroAnns (ASSERTX parsedInstr') =
  ASSERTX (stripInstrAbstractAnns parsedInstr')
stripMacroAnns (ASSERT_CMP parsedInstr') =
  ASSERT_CMP (stripInstrAbstractAnns parsedInstr')
stripMacroAnns (IF_SOME parsedOpsX parsedOpsY) =
  IF_SOME (parsedOpsX >>= stripParsedOpAnns) (parsedOpsY >>= stripParsedOpAnns)
stripMacroAnns (IF_RIGHT parsedOpsX parsedOpsY) =
  IF_RIGHT (parsedOpsX >>= stripParsedOpAnns) (parsedOpsY >>= stripParsedOpAnns)
stripMacroAnns FAIL = FAIL
stripMacroAnns ASSERT = ASSERT
stripMacroAnns ASSERT_NONE = ASSERT_NONE
stripMacroAnns ASSERT_SOME = ASSERT_SOME
stripMacroAnns ASSERT_LEFT = ASSERT_LEFT
stripMacroAnns ASSERT_RIGHT = ASSERT_RIGHT

-- | Strip annotations from `LetMacro`
stripLetMacroAnns :: LetMacro -> LetMacro
stripLetMacroAnns (LetMacro name' sig' body') =
  LetMacro name' sig' $ body' >>= stripParsedOpAnns


-- | Compare and print if not equal
compareParams :: U.Parameter -> U.Parameter -> IO ()
compareParams paraX paraY =
  bool
    (putStrLn ("Parameters match" :: String))
    (do
      putStrLn ("Parameters don't match:" :: String)
      print paraX
      print paraY
      putStrLn ("" :: String)
    )
    (paraX == paraY)

-- | Compare and print if not equal
compareStorages :: U.Storage -> U.Storage -> IO ()
compareStorages storX storY =
  bool
    (putStrLn ("Storages match" :: String))
    (do
      putStrLn ("Storages don't match:" :: String)
      print storX
      print storY
      putStrLn ("" :: String)
    )
    (storX == storY)

-- | Compare and print if not equal
compareOperations :: [ParsedOp] -> [ParsedOp] -> IO ()
compareOperations codeX codeY =
  bool
    (putStrLn ("Codes match" :: String))
    (do
      putStrLn ("Codes don't match:" :: String)
      print codeX
      print codeY
      putStrLn ("" :: String)
    )
    (codeX == codeY)

-- | Strip annotations and compare contracts
compareContracts :: U.Contract' ParsedOp -> U.Contract' ParsedOp -> IO ()
compareContracts (U.Contract paraX storX codeX) (U.Contract paraY storY codeY) = do
  compareParams (stripTypeAnns paraX) (stripTypeAnns paraY)
  compareStorages (stripTypeAnns storX) (stripTypeAnns storY)
  compareOperations (codeX >>= stripParsedOpAnns) (codeY >>= stripParsedOpAnns)

