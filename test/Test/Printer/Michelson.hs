module Test.Printer.Michelson
  ( unit_Roundtrip
  , unit_let_macro
  ) where

import Fmt (pretty)
import Test.HUnit (Assertion, assertEqual, assertFailure, (@?=))

import Michelson.Printer (printUntypedContract)
import Michelson.Runtime (parseExpandContract)
import Michelson.Test (importUntypedContract)
import qualified Michelson.Untyped as U

import Test.Util.Contracts (getWellTypedContracts)

unit_Roundtrip :: Assertion
unit_Roundtrip = do
  contractFiles <- getWellTypedContracts
  mapM_ roundtripPrintTest contractFiles
  where
    roundtripPrintTest :: FilePath -> Assertion
    roundtripPrintTest filePath = do
      contract1 <- importUntypedContract filePath
      contract2 <- printAndParse filePath contract1
      -- We don't expect that `contract1` equals `contract2`,
      -- because during printing we lose extra instructions.
      assertEqual ("After printing and parsing " <> filePath <>
                   " is printed differently")
        (printUntypedContract contract1)
        (printUntypedContract contract2)

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
