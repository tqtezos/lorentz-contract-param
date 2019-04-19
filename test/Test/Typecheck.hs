module Test.Typecheck
  ( typeCheckSpec
  ) where

import qualified Data.Map as M
import Data.Singletons (sing)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe)
import Test.QuickCheck (total, within)

import Michelson.Runtime (prepareContract)
import Michelson.Test.Import (ImportContractError(..), readContract)
import Michelson.TypeCheck
import qualified Michelson.Typed as T
import Michelson.Untyped (CT(..), T(..), Type(..), noAnn)
import qualified Michelson.Untyped as Un
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

  describe "StackRef"
    stackRefSpec

  where
    pushContrFile = "contracts/ill-typed/push_contract.tz"
    tPair t1 t2 = Type (TPair noAnn noAnn t1 t2) noAnn
    intP = Type (Tc CInt) "p"
    intQ = Type (Tc CInt) "q"
    int = Type (Tc CInt) noAnn
    string = Type (Tc CString) noAnn

    cAddr = unsafeParseAddress "KT1WsLzQ61xtMNJHfwgCHh2RnALGgFAzeSx9"

    doTC cs = either (Left . displayException) (\_ -> pure ()) .
            typeCheckContract (M.fromList cs)

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

stackRefSpec :: Spec
stackRefSpec = do
  it "Typecheck fails when ref is out of bounds" $
    let instr = printStRef 2
        hst = stackEl ::& stackEl ::& SNil
    in case
        runTypeCheckT (error "no contract param") mempty $
        typeCheckList [Un.PrimEx instr] hst
        of
          Left err -> total $ show @Text err
          Right _ -> error "Typecheck unexpectedly succeded"

  it "Typecheck time is reasonably bounded" $ within 1000 $
    -- Making code processing performance scale with code size looks like a
    -- good property, so we'd like to avoid scenario when user tries to
    -- access 100500-th element of stack and typecheck hangs as a result
    let instr = printStRef 100000000000
        hst = stackEl ::& SNil
    in case
        runTypeCheckT (error "no contract param") mempty $
        typeCheckList [Un.PrimEx instr] hst
      of
        Left err -> total $ show @Text err
        Right _ -> error "Typecheck unexpectedly succeded"
  where
    printStRef i = Un.EXT . Un.UPRINT $ Un.PrintComment [Right (Un.StackRef i)]
    stackEl = (sing @'T.TUnit, T.NStar, noAnn)
