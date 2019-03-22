module Test.Typecheck
  ( typeCheckSpec
  ) where

import Test.Hspec (Expectation, Spec, describe, expectationFailure, it)

import Michelson.Untyped (Contract)
import Morley.Ext (typeCheckMorleyContract)
import Morley.Runtime (prepareContract)

import Test.Util.Contracts (getIllTypedContracts, getWellTypedContracts)

typeCheckSpec :: Spec
typeCheckSpec = describe "Typechecker tests" $ do
  it "Successfully typechecks contracts examples from contracts/" goodContractsTest
  it "Reports errors on contracts examples from contracts/ill-typed" badContractsTest
  where
    doTC = either (Left . displayException) (\_ -> pure ()) .
            typeCheckMorleyContract mempty

    goodContractsTest = mapM_ (checkFile doTC True) =<< getWellTypedContracts

    badContractsTest = mapM_ (checkFile doTC False) =<< getIllTypedContracts


checkFile :: (Contract -> Either String ()) -> Bool -> FilePath -> Expectation
checkFile doTypeCheck wellTyped file = do
  c <- prepareContract (Just file)
  case doTypeCheck c of
    Left err
      | wellTyped ->
        expectationFailure $
        "Typechecker unexpectedly failed on " <> show file <> ": " <> err
      | otherwise -> pass
    Right _
      | not wellTyped ->
        expectationFailure $
        "Typechecker unexpectedly considered " <> show file <> " well-typed."
      | otherwise -> pass
