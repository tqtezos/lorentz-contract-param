module Test.Printer.Michelson
  ( unit_Roundtrip
  , unit_let_macro
  ) where

import Fmt (pretty)
import Test.HUnit (Assertion, assertEqual, assertFailure, (@?=))
import Generics.SYB (everywhere, mkT)

import Michelson.Printer (printUntypedContract)
import Michelson.Runtime (parseExpandContract)
import Michelson.Test (importUntypedContract)
import Michelson.Untyped.Instr (ExpandedOp(..))
import qualified Michelson.Untyped as U

import Test.Util.Contracts

unit_Roundtrip :: Assertion
unit_Roundtrip = do
  morleyContractFiles <- getWellTypedMorleyContracts
  mapM_ morleyRoundtripPrintTest morleyContractFiles
  michelsonContractFiles <- getWellTypedMichelsonContracts
  mapM_ michelsonRoundtripPrintTest michelsonContractFiles
  where
    morleyRoundtripPrintTest :: FilePath -> Assertion
    morleyRoundtripPrintTest filePath = do
      contract1 <- importUntypedContract filePath
      contract2 <- printAndParse filePath contract1
      -- We don't expect that `contract1` equals `contract2`,
      -- because during printing we lose extra instructions.
      assertEqual ("After printing and parsing " <> filePath <>
                   " is printed differently")
        (printUntypedContract contract1)
        (printUntypedContract contract2)
    michelsonRoundtripPrintTest :: FilePath -> Assertion
    michelsonRoundtripPrintTest filePath = do
      contract1 <- importUntypedContract filePath
      contract2 <- printAndParse filePath contract1
      -- We expect `contract1` equals `contract2`.
      assertEqual ("After printing and parsing " <> filePath <>
                   " contracts are different")
        (transformContract contract1) (transformContract contract2)

unit_let_macro :: Assertion
unit_let_macro = do
  let filePath = "contracts/ill-typed/letblock_trivial.mtz"
  contract <- printAndParse filePath =<< importUntypedContract filePath
  let ops = concatMap U.flattenExpandedOp (U.code contract)
  ops @?= [U.CDR U.noAnn U.noAnn, U.UNIT U.noAnn U.noAnn, U.DROP]

printAndParse :: FilePath -> U.Contract -> IO U.Contract
printAndParse fp contract1 =
  case parseExpandContract (Just fp) (toText $ printUntypedContract contract1) of
    Left err ->
      assertFailure ("Failed to parse printed " <> fp <> ": " <> pretty err)
    Right contract2 -> pure contract2


-- | Remove all `WithSrcEx` from contract code because `SrcPos`es
-- and such stuff can change during printing and parsing
transformContract :: U.Contract -> U.Contract
transformContract (U.Contract c s code) =
  U.Contract c s (map transform code)
  where
    transform :: ExpandedOp -> ExpandedOp
    transform = everywhere $ mkT removeWithSrcEx
    removeWithSrcEx :: ExpandedOp -> ExpandedOp
    removeWithSrcEx (WithSrcEx _ op) = op
    removeWithSrcEx op = op
