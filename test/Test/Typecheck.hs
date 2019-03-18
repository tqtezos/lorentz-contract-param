module Test.Typecheck
  ( typeCheckSpec
  ) where

import Test.Hspec (Expectation, Spec, describe, expectationFailure, it)
import Text.Megaparsec (parse)

import Michelson.Untyped (Contract(..), Op(..))
import Morley.Macro (expandFlat)
import Morley.Ext (typeCheckMorleyContract)
import Morley.Parser (program)

import Test.Util.Contracts (getIllTypedContracts, getWellTypedContracts)

typeCheckSpec :: Spec
typeCheckSpec = describe "Typechecker tests" $ do
  it "Successfully typechecks contracts examples from contracts/" goodContractsTest
  it "Reports errors on contracts examples from contracts/ill-typed" badContractsTest
  where
    doTC = either (Left . displayException) (\_ -> pure ()) .
            typeCheckMorleyContract . fmap unOp

    goodContractsTest = mapM_ (checkFile doTC True) =<< getWellTypedContracts

    badContractsTest = mapM_ (checkFile doTC False) =<< getIllTypedContracts


checkFile :: (Contract Op -> Either String ()) -> Bool -> FilePath -> Expectation
checkFile doTypeCheck wellTyped file = do
  cd <- readFile file
  case parse program file cd of
    Right c' -> do
      let c = Contract (para c') (stor c') (expandFlat $ code c')
      case doTypeCheck c of
        Left err
          | wellTyped ->
            expectationFailure $
            "Typechecker unexpectedly failed on " <>
            show file <> ": " <> err
          | otherwise ->
            pass
        Right _
          | not wellTyped ->
            expectationFailure $
            "Typechecker unexpectedly considered " <> show file <> " well-typed."
          | otherwise ->
            pass
    Left e -> expectationFailure $ "Parser error: " <> show e
