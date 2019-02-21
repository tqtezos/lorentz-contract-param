module Test.Typecheck
  ( simpleSpec
  , advancedSpec
  ) where

import Test.Hspec (Expectation, Spec, describe, expectationFailure, it)
import Text.Megaparsec (parse)

import qualified Michelson.Typecheck as S
import Michelson.Types (Contract(..), Op (..))
import Morley.Macro (expandFlat)
import Morley.Parser (contract)
import qualified Michelson.Advanced as A

import Test.Util.Contracts (getIllTypedContracts, getWellTypedContracts)

simpleSpec :: Spec
simpleSpec = describe "Simple type typechecker tests" $ specImpl doTC
  where
    doTC = either (Left . displayException) pure . S.typecheckContract

advancedSpec :: Spec
advancedSpec = describe "Advanced type typechecker tests" $ do
    specImpl doTC
  where
    doTC = either (Left . displayException) (\_ -> pure ()) .
            A.typeCheckContract . fmap unOp

specImpl :: TypeCheckFun -> Spec
specImpl doTC = do
    it "Successfully typechecks contracts examples from contracts/" $
      goodContractsTest doTC
    it "Reports errors on contracts examples from contracts/ill-typed" $
      badContractsTest doTC

type TypeCheckFun = Contract Op -> Either String ()

goodContractsTest :: TypeCheckFun -> Expectation
goodContractsTest doTC = mapM_ (checkFile doTC True) =<< getWellTypedContracts

badContractsTest :: TypeCheckFun -> Expectation
badContractsTest doTC = mapM_ (checkFile doTC False) =<< getIllTypedContracts

checkFile :: TypeCheckFun -> Bool -> FilePath -> Expectation
checkFile doTypeCheck wellTyped file = do
  cd <- readFile file
  case parse contract file cd of
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
            "Typechecker unexpectedly considered " <> show file <> "well-typed."
          | otherwise ->
            pass
    Left e -> expectationFailure $ "Parser error: " <> show e
