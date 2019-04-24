module Test.Printer.Michelson
  ( unit_Roundtrip
  ) where

import Fmt (pretty)
import Test.HUnit (Assertion, assertEqual, assertFailure)

import Michelson.Printer (printUntypedContract)
import Michelson.Runtime (parseExpandContract)
import Michelson.Test (importUntypedContract)

import Test.Util.Contracts (getWellTypedContracts)

unit_Roundtrip :: Assertion
unit_Roundtrip = do
  contractFiles <- getWellTypedContracts
  mapM_ roundtripPrintTest contractFiles
  where
    roundtripPrintTest :: FilePath -> Assertion
    roundtripPrintTest filePath = do
      contract1 <- importUntypedContract filePath
      case parseExpandContract (Just filePath) (toText $ printUntypedContract contract1) of
        Left err -> assertFailure
          ("Failed to parse printed " <> filePath <> ": " ++ pretty err)
        -- We don't expect that `contract1` equals `contract2`,
        -- because during printing we lose extra instructions.
        Right contract2 ->
          assertEqual ("After printing and parsing " <> filePath <> " is printed differently")
          (printUntypedContract contract1)
          (printUntypedContract contract2)
