module Test.Printer.Michelson
  ( spec_Printer
  ) where

import Fmt (pretty)
import Test.Hspec (Spec, it, runIO, shouldBe)

import Michelson.Printer (printUntypedContract)
import Michelson.Runtime (parseExpandContract)
import Michelson.Test (specWithUntypedContract)

import Test.Util.Contracts (getWellTypedContracts)

spec_Printer :: Spec
spec_Printer = do
  contractFiles <- runIO getWellTypedContracts
  mapM_ roundtripPrintTest contractFiles

roundtripPrintTest :: FilePath -> Spec
roundtripPrintTest filePath =
  -- these are untyped and expanded contracts, they might have macros
  specWithUntypedContract filePath $ \contract1 ->
    it "roundtrip printUntypedContract test" $ do
      case parseExpandContract (Just filePath) (toText $ printUntypedContract contract1) of
        Left err -> fail ("Failed to read 'printUntypedContract contract1': " ++ pretty err)
        Right contract2 ->
          printUntypedContract contract1 `shouldBe` printUntypedContract contract2
