module Test.Parser
  ( spec
  ) where

import Data.List (isSuffixOf)
import Morley.Parser (contract)
import System.Directory (listDirectory)
import Test.Hspec (Expectation, Spec, describe, it, shouldSatisfy)
import Text.Megaparsec (parse)


spec :: Spec
spec = describe "Parser tests" $ do
  it "Successfully parses contracts examples from contracts/" parseContractsTest

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
