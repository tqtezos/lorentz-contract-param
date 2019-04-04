module Test.Typecheck
  ( typeCheckSpec
  ) where

import qualified Data.Map as M
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe)

import Michelson.TypeCheck (TCError(..))
import qualified Michelson.Typed as T
import Michelson.Untyped (CT(..), T(..), Type(..), noAnn)
import qualified Michelson.Untyped as Un
import Morley.Ext (typeCheckMorleyContract)
import Morley.Runtime (prepareContract)
import Morley.Test.Import (ImportContractError(..), readContract)
import Tezos.Address (unsafeParseAddress)

import Test.Util.Contracts (getIllTypedContracts, getWellTypedContracts)

typeCheckSpec :: Spec
typeCheckSpec = describe "Typechecker tests" $ do
  it "Successfully typechecks contracts examples from contracts/" goodContractsTest
  it "Reports errors on contracts examples from contracts/ill-typed" badContractsTest

  it "Successfully typechecked PUSH contract considering originated contracts" $ do
    checkFile (doTC [(cAddr, tPair intP intQ)]) True pushContrFile
    checkFile (doTC [(cAddr, tPair int intQ)]) True pushContrFile
    checkFile (doTC [(cAddr, tPair int int)]) True pushContrFile
  it "Report an error on PUSH contract because of mismatched types" $ do
    checkFile (doTC [(cAddr, tPair intP intP)]) False pushContrFile
    checkFile (doTC [(cAddr, tPair intQ intQ)]) False pushContrFile
    checkFile (doTC [(cAddr, tPair string intQ)]) False pushContrFile

  it "Unreachable code is handled properly" $ do
    let file = "contracts/ill-typed/fail-before-nop.tz"
    econtract <- readContract @'T.TUnit @'T.TUnit file <$> readFile file
    econtract `shouldBe` Left (ICETypeCheck $ TCUnreachableCode (one $ Un.SeqEx []))

  where
    pushContrFile = "contracts/ill-typed/push_contract.tz"
    tPair t1 t2 = Type (TPair noAnn noAnn t1 t2) noAnn
    intP = Type (Tc CInt) "p"
    intQ = Type (Tc CInt) "q"
    int = Type (Tc CInt) noAnn
    string = Type (Tc CString) noAnn

    cAddr = unsafeParseAddress "KT1WsLzQ61xtMNJHfwgCHh2RnALGgFAzeSx9"

    doTC cs = either (Left . displayException) (\_ -> pure ()) .
            typeCheckMorleyContract (M.fromList cs)

    doTCSimple = doTC []

    goodContractsTest = mapM_ (checkFile doTCSimple True) =<< getWellTypedContracts

    badContractsTest = mapM_ (checkFile doTCSimple False) =<< getIllTypedContracts


checkFile :: (Un.Contract -> Either String ()) -> Bool -> FilePath -> Expectation
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
