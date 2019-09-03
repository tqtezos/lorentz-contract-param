{-# OPTIONS -Wno-missing-export-lists #-}

module Lorentz.Contracts.Util.Strip where

import Michelson.Macro (ParsedOp(..), LetMacro(..), Macro(..), PairStruct(..), ParsedInstr)
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
stripTAnns (U.TOption _ type') = U.TOption def (stripTypeAnns type')
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
stripTAnns t@U.TSignature = t
stripTAnns t@U.TOperation = t

-- | Strip annotations from `U.Type`
stripTypeAnns :: U.Type -> U.Type
stripTypeAnns (U.Type t tAnn) = U.Type (stripTAnns t) (def `asTypeOf` tAnn)
stripTypeAnns U.TypeParameter = U.TypeParameter
stripTypeAnns U.TypeStorage = U.TypeStorage

-- | Strip annotations from `ParsedOp` and join `Seq`'s of `ParsedOp`'s
stripParsedOpAnns :: ParsedOp -> [ParsedOp]
stripParsedOpAnns (Prim parsedInstr' _) =
  [Prim (stripParsedInstrAnns parsedInstr') def]
stripParsedOpAnns (Mac macro' _) = [Mac (stripMacroAnns macro') def]
stripParsedOpAnns (LMac letMacro' _) = [LMac (stripLetMacroAnns letMacro') def]
stripParsedOpAnns (Seq parsedOps' _) = parsedOps' >>= stripParsedOpAnns

-- | Strip annotations from `ExtInstrAbstract`
stripExtInstrAbstractAnns ::
     U.ExtInstrAbstract ParsedOp -> U.ExtInstrAbstract ParsedOp
stripExtInstrAbstractAnns (U.FN str' stackFn' parsedOps') =
  U.FN str' stackFn' (parsedOps' >>= stripParsedOpAnns)
stripExtInstrAbstractAnns extInstrAbstract' = extInstrAbstract'

-- | Strip annotations from `ParsedInstr`
stripParsedInstrAnns :: ParsedInstr -> ParsedInstr
stripParsedInstrAnns (U.ABS _) = U.ABS def
stripParsedInstrAnns (U.ADD _) = U.ADD def
stripParsedInstrAnns (U.ADDRESS _) = U.ADDRESS def
stripParsedInstrAnns (U.AMOUNT _) = U.AMOUNT def
stripParsedInstrAnns (U.AND _) = U.AND def
stripParsedInstrAnns (U.BALANCE _) = U.BALANCE def
stripParsedInstrAnns (U.BLAKE2B _) = U.BLAKE2B def
stripParsedInstrAnns (U.CAR _ _) = U.CAR def def
stripParsedInstrAnns (U.CAST _ type') = U.CAST def (stripTypeAnns type')
stripParsedInstrAnns (U.CDR _ _) = U.CDR def def
stripParsedInstrAnns (U.CHECK_SIGNATURE _) = U.CHECK_SIGNATURE def
stripParsedInstrAnns (U.COMPARE _) = U.COMPARE def
stripParsedInstrAnns (U.CONCAT _) = U.CONCAT def
stripParsedInstrAnns (U.CONS _) = U.CONS def
stripParsedInstrAnns (U.CONTRACT _ type') = U.CONTRACT def (stripTypeAnns type')
stripParsedInstrAnns (U.CREATE_ACCOUNT _ _) = U.CREATE_ACCOUNT def def
stripParsedInstrAnns (U.CREATE_CONTRACT _ _ contract') =
  U.CREATE_CONTRACT def def (stripContractAnns contract')
stripParsedInstrAnns (U.DIP parsedOps') =
  U.DIP (parsedOps' >>= stripParsedOpAnns)
stripParsedInstrAnns (U.DUP _) = U.DUP def
stripParsedInstrAnns (U.EDIV _) = U.EDIV def
stripParsedInstrAnns (U.EMPTY_MAP _ _ comparable' type') =
  U.EMPTY_MAP def def (stripComparableAnns comparable') (stripTypeAnns type')
stripParsedInstrAnns (U.EMPTY_SET _ _ comparable') =
  U.EMPTY_SET def def (stripComparableAnns comparable')
stripParsedInstrAnns (U.EQ _) = U.EQ def
stripParsedInstrAnns (U.EXEC _) = U.EXEC def
stripParsedInstrAnns (U.EXT extInstrAbstract') =
  U.EXT $ stripExtInstrAbstractAnns extInstrAbstract'
stripParsedInstrAnns (U.GE _) = U.GE def
stripParsedInstrAnns (U.GET _) = U.GET def
stripParsedInstrAnns (U.GT _) = U.GT def
stripParsedInstrAnns (U.HASH_KEY _) = U.HASH_KEY def
stripParsedInstrAnns (U.IF parsedOpsX parsedOpsY) =
  U.IF (parsedOpsX >>= stripParsedOpAnns) (parsedOpsY >>= stripParsedOpAnns)
stripParsedInstrAnns (U.IF_CONS parsedOpsX parsedOpsY) =
  U.IF_CONS
    (parsedOpsX >>= stripParsedOpAnns)
    (parsedOpsY >>= stripParsedOpAnns)
stripParsedInstrAnns (U.IF_LEFT parsedOpsX parsedOpsY) =
  U.IF_LEFT
    (parsedOpsX >>= stripParsedOpAnns)
    (parsedOpsY >>= stripParsedOpAnns)
stripParsedInstrAnns (U.IF_NONE parsedOpsX parsedOpsY) =
  U.IF_NONE
    (parsedOpsX >>= stripParsedOpAnns)
    (parsedOpsY >>= stripParsedOpAnns)
stripParsedInstrAnns (U.IMPLICIT_ACCOUNT _) = U.IMPLICIT_ACCOUNT def
stripParsedInstrAnns (U.INT _) = U.INT def
stripParsedInstrAnns (U.ISNAT _) = U.ISNAT def
stripParsedInstrAnns (U.ITER parsedOps') =
  U.ITER (parsedOps' >>= stripParsedOpAnns)
stripParsedInstrAnns (U.LAMBDA _ typeX typeY parsedOps') =
  U.LAMBDA
    def
    (stripTypeAnns typeX)
    (stripTypeAnns typeY)
    (parsedOps' >>= stripParsedOpAnns)
stripParsedInstrAnns (U.LE _) = U.LE def
stripParsedInstrAnns (U.LEFT _ _ _ _ type') =
  U.LEFT def def def def (stripTypeAnns type')
stripParsedInstrAnns (U.LOOP parsedOps') =
  U.LOOP (parsedOps' >>= stripParsedOpAnns)
stripParsedInstrAnns (U.LOOP_LEFT parsedOps') =
  U.LOOP_LEFT (parsedOps' >>= stripParsedOpAnns)
stripParsedInstrAnns (U.LSL _) = U.LSL def
stripParsedInstrAnns (U.LSR _) = U.LSR def
stripParsedInstrAnns (U.LT _) = U.LT def
stripParsedInstrAnns (U.MAP _ parsedOps') =
  U.MAP def (parsedOps' >>= stripParsedOpAnns)
stripParsedInstrAnns (U.MEM _) = U.MEM def
stripParsedInstrAnns (U.MUL _) = U.MUL def
stripParsedInstrAnns (U.NEQ _) = U.NEQ def
stripParsedInstrAnns (U.NIL _ _ type') = U.NIL def def (stripTypeAnns type')
stripParsedInstrAnns (U.NONE _ _ _ type') =
  U.NONE def def def (stripTypeAnns type')
stripParsedInstrAnns (U.NOT _) = U.NOT def
stripParsedInstrAnns (U.NOW _) = U.NOW def
stripParsedInstrAnns (U.OR _) = U.OR def
stripParsedInstrAnns (U.PACK _) = U.PACK def
stripParsedInstrAnns (U.PAIR _ _ _ _) = U.PAIR def def def def
stripParsedInstrAnns (U.PUSH _ type' value') =
  U.PUSH def (stripTypeAnns type') value'
stripParsedInstrAnns (U.RENAME _) = U.RENAME def
stripParsedInstrAnns (U.RIGHT _ _ _ _ type') =
  U.RIGHT def def def def (stripTypeAnns type')
stripParsedInstrAnns (U.SELF _) = U.SELF def
stripParsedInstrAnns (U.SENDER _) = U.SENDER def
stripParsedInstrAnns (U.SET_DELEGATE _) = U.SET_DELEGATE def
stripParsedInstrAnns (U.SHA256 _) = U.SHA256 def
stripParsedInstrAnns (U.SHA512 _) = U.SHA512 def
stripParsedInstrAnns (U.SIZE _) = U.SIZE def
stripParsedInstrAnns (U.SLICE _) = U.SLICE def
stripParsedInstrAnns (U.SOME _ _ _) = U.SOME def def def
stripParsedInstrAnns (U.SOURCE _) = U.SOURCE def
stripParsedInstrAnns (U.STEPS_TO_QUOTA _) = U.STEPS_TO_QUOTA def
stripParsedInstrAnns (U.SUB _) = U.SUB def
stripParsedInstrAnns (U.TRANSFER_TOKENS _) = U.TRANSFER_TOKENS def
stripParsedInstrAnns (U.UNIT _ _) = U.UNIT def def
stripParsedInstrAnns (U.UNPACK _ type') = U.UNPACK def (stripTypeAnns type')
stripParsedInstrAnns (U.XOR _) = U.XOR def
stripParsedInstrAnns U.DROP = U.DROP
stripParsedInstrAnns U.FAILWITH = U.FAILWITH
stripParsedInstrAnns U.NEG = U.NEG
stripParsedInstrAnns U.SWAP = U.SWAP
stripParsedInstrAnns U.UPDATE = U.UPDATE

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
  CMP (stripParsedInstrAnns parsedInstr') def
stripMacroAnns (IFX parsedInstr' parsedOpsX parsedOpsY) =
  IFX
    (stripParsedInstrAnns parsedInstr')
    (parsedOpsX >>= stripParsedOpAnns)
    (parsedOpsY >>= stripParsedOpAnns)
stripMacroAnns (IFCMP parsedInstr' _ parsedOpsX parsedOpsY) =
  IFCMP
    (stripParsedInstrAnns parsedInstr')
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
  ASSERTX (stripParsedInstrAnns parsedInstr')
stripMacroAnns (ASSERT_CMP parsedInstr') =
  ASSERT_CMP (stripParsedInstrAnns parsedInstr')
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

