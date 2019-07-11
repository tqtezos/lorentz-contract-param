module Test.Printer.Michelson
  ( unit_Roundtrip
  , unit_let_macro
  , unit_PrettyPrint
  ) where

import Data.Text.Lazy (strip)
import Fmt (pretty)
import Generics.SYB (everywhere, mkT)
import Test.HUnit (Assertion, assertEqual, assertFailure, (@?=))

import Michelson.Printer (printUntypedContract)
import Michelson.Runtime (parseExpandContract)
import Michelson.Test (importUntypedContract)
import qualified Michelson.Untyped as U
import Michelson.Untyped.Instr (ExpandedOp(..))
import Util.IO (readFileUtf8)

import Test.Util.Contracts

unit_PrettyPrint :: Assertion
unit_PrettyPrint = do
  contracts <- getPrettyPrintContracts
  mapM_ prettyTest contracts
  where
    prettyTest :: (FilePath, FilePath) -> Assertion
    prettyTest (srcPath, dstPath) = do
      contract <- importUntypedContract srcPath
      targetSrc <- strip . fromStrict <$> readFileUtf8 dstPath
      assertEqual
        ("Prettifying " <> srcPath <> " does not match the expected format")
        (printUntypedContract False contract)
        targetSrc
      assertEqual
        ("Single line pretty printer output "
          <> srcPath <> " contain new lines.")
        (find (=='\n') $ printUntypedContract True contract) Nothing

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
        (printUntypedContract True contract1) -- using single line output here
        (printUntypedContract True contract2)
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
  case parseExpandContract (Just fp) (toText $ printUntypedContract True contract1) of
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
