module Test.Typecheck
  ( spec
  ) where

import Data.List (isSuffixOf)
import Michelson.Typecheck (typecheckContract)
import Michelson.Types (Contract(..))
import Morley.Macro (expandFlat)
import Morley.Parser (contract)
import System.Directory (listDirectory)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldSatisfy)
import Text.Megaparsec (parse)

spec :: Spec
spec = describe "Typechecker tests" $ do
  it "Successfully typechecks contracts examples from contracts/" parseContractsTest

parseContractsTest :: Expectation
parseContractsTest = do
  let dir = "contracts"
  files <- (fmap . fmap) (\s -> dir ++ "/" ++ s) $
    fmap (filter (\ x -> (isSuffixOf ".tz" x) || (isSuffixOf ".mtz" x))) $ listDirectory dir
  void $ mapM checkFile files

checkFile :: FilePath -> Expectation
checkFile file = do
  cd <- readFile file
  case parse contract file cd of
      Right c' -> do
          let c = Contract (para c') (stor c') (expandFlat $ code c')
          typecheckContract c `shouldSatisfy` isRight
      Left e -> expectationFailure $ "Parser error: " <> show e
