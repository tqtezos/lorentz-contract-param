module Test.Typecheck
  ( spec
  ) where

import Michelson.Typecheck (typecheckContract)
import Michelson.Types (Contract(..))
import Morley.Macro (expandFlat)
import Morley.Parser (contract)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it)
import Text.Megaparsec (parse)

import Test.Util.Contracts (getIllTypedContracts, getWellTypedContracts)

spec :: Spec
spec = describe "Typechecker tests" $ do
  it "Successfully typechecks contracts examples from contracts/" goodContractsTest
  it "Reports errors on contracts examples from contracts/ill-typed" badContractsTest

goodContractsTest :: Expectation
goodContractsTest = mapM_ (checkFile True) =<< getWellTypedContracts

badContractsTest :: Expectation
badContractsTest = mapM_ (checkFile False) =<< getIllTypedContracts

checkFile :: Bool -> FilePath -> Expectation
checkFile wellTyped file = do
  cd <- readFile file
  case parse contract file cd of
    Right c' -> do
      let c = Contract (para c') (stor c') (expandFlat $ code c')
      case typecheckContract c of
        Left err
          | wellTyped ->
            expectationFailure $
            "Typechecker unexpectedly failed on " <>
            show file <> ": " <> displayException err
          | otherwise ->
            pass
        Right _
          | not wellTyped ->
            expectationFailure $
            "Typechecker unexpectedly considered " <> show file <> "well-typed."
          | otherwise ->
            pass
    Left e -> expectationFailure $ "Parser error: " <> show e
