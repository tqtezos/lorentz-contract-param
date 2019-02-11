module Test.Parser
  ( spec
  ) where

import Data.List (isSuffixOf)
import Morley.Parser (contract, value)
import Morley.Types as M
import System.Directory (listDirectory)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (parse)


spec :: Spec
spec = describe "Parser tests" $ do
  it "Successfully parses contracts examples from contracts/" parseContractsTest
  it "value parser test" valueParserTest

parseContractsTest :: Expectation
parseContractsTest = do
  let dir = "contracts"
  files <- (fmap . fmap) (\s -> dir ++ "/" ++ s) $
    fmap (filter (\ x -> (isSuffixOf ".tz" x) || (isSuffixOf ".mtz" x))) $ listDirectory dir
  void $ mapM checkFile files

checkFile :: FilePath -> Expectation
checkFile file = do
  code <- readFile file
  parse contract file code `shouldSatisfy` isRight

valueParserTest :: Expectation
valueParserTest = do
  parse value "" "{PUSH int 5;}" `shouldBe`
    (Right $ M.ValueLambda [M.PRIM (M.PUSH noAnn (M.Type (M.T_comparable M.T_int) noAnn) (M.ValueInt 5))])
  parse value "" "{1; 2}" `shouldBe`
    (Right $ M.ValueSeq [M.ValueInt 1, M.ValueInt 2])
  parse value "" "{Elt 1 2; Elt 3 4}" `shouldBe`
    (Right $ M.ValueMap [M.Elt (M.ValueInt 1) (M.ValueInt 2), M.Elt (M.ValueInt 3) (M.ValueInt 4)])
