{-# LANGUAGE ViewPatterns #-}

module Test.Typecheck
  ( unit_Good_contracts
  , unit_Bad_contracts
  , test_OriginatedContracts
  , test_srcPosition
  , unit_Unreachable_code
  , test_StackRef
  ) where

import Data.Default (def)
import qualified Data.Map as M
import Data.Singletons (sing)
import Test.Hspec (expectationFailure)
import Test.Hspec.Expectations (Expectation, shouldBe)
import Test.HUnit (Assertion, assertFailure, (@?=))
import Test.QuickCheck (total, within)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)

import Michelson.ErrorPos (InstrCallStack(..), LetName(..), Pos(..), SrcPos(..), srcPos)
import Michelson.Runtime (prepareContract)
import Michelson.Test.Import (ImportContractError(..), readContract)
import Michelson.TypeCheck
import qualified Michelson.Typed as T
import Michelson.Untyped (CT(..), T(..), Type(..), noAnn)
import qualified Michelson.Untyped as Un
import Tezos.Address (Address, unsafeParseAddress)

import Test.Util.Contracts (getIllTypedContracts, getWellTypedContracts)

unit_Good_contracts :: Assertion
unit_Good_contracts = mapM_ (\f -> checkFile [] True f (const $ pure ())) =<< getWellTypedContracts

unit_Bad_contracts :: Assertion
unit_Bad_contracts = mapM_ (\f -> checkFile [] False f (const $ pure ())) =<< getIllTypedContracts

test_OriginatedContracts :: [TestTree]
test_OriginatedContracts =
  [ testCase "Successfully typechecked PUSH contract considering originated contracts" $ do
    checkFile' [(cAddr, tPair intP intQ)] True pushContrFile
    checkFile' [(cAddr, tPair int intQ)] True pushContrFile
    checkFile' [(cAddr, tPair int int)] True pushContrFile
  , testCase "Report an error on PUSH contract because of mismatched types" $ do
    checkFile' [(cAddr, tPair intP intP)] False pushContrFile
    checkFile' [(cAddr, tPair intQ intQ)] False pushContrFile
    checkFile' [(cAddr, tPair string intQ)] False pushContrFile
  ]
  where
    checkFile' a b f = checkFile a b f (const $ pure ())
    pushContrFile = "contracts/ill-typed/push_contract.tz"
    tPair t1 t2 = Type (TPair noAnn noAnn t1 t2) noAnn
    intP = Type (Tc CInt) "p"
    intQ = Type (Tc CInt) "q"
    int = Type (Tc CInt) noAnn
    string = Type (Tc CString) noAnn
    cAddr = unsafeParseAddress "KT1WsLzQ61xtMNJHfwgCHh2RnALGgFAzeSx9"

pattern IsSrcPos :: Word -> Word -> InstrCallStack
pattern IsSrcPos l c <- InstrCallStack [] (SrcPos (Pos l) (Pos c))

test_srcPosition :: [TestTree]
test_srcPosition =
  [ testCase "Unreachable code is handled properly" $ do
      let file = "contracts/ill-typed/fail-before-nop.tz"
      let ics = InstrCallStack [] (srcPos 3 13)
      econtract <- readContract @'T.TUnit @'T.TUnit file <$> readFile file
      econtract `shouldBe` Left (ICETypeCheck $ TCUnreachableCode ics $ one $ Un.WithSrcEx ics $ Un.SeqEx [])
  , testProperty "Verify instruction position in a typecheck error" $ do
      checkIllFile "contracts/ill-typed/basic3.tz" $ \case
          TCFailedOnInstr (Un.CONS _) _ _ (IsSrcPos 4 6) (Just (AnnError _)) -> True
          _ -> False

      checkIllFile "contracts/ill-typed/testassert_invalid_stack3.mtz" $ \case
          TCFailedOnInstr Un.DROP _ _ (IsSrcPos 6 17) Nothing -> True
          _ -> False

      checkIllFile "contracts/ill-typed/testassert_invalid_stack2.mtz" $ \case
          TCExtError _ (IsSrcPos 5 2) (TestAssertError _) -> True
          _ -> False

      checkIllFile "contracts/ill-typed/macro_in_let_fail.mtz" $ \case
          TCFailedOnInstr (Un.COMPARE _) _ _ (InstrCallStack [LetName "cmpLet"] (SrcPos (Pos 3) (Pos 6)))
                                              (Just (UnsupportedTypes _)) -> True
          _ -> False
  ]
  where
    unexpected f e =
      expectationFailure $ "Unexpected typecheck error: " <> displayException e <> " in file: " <> f
    checkIllFile file check = checkFile [] False file $
      \e -> if check e then pass else unexpected file e

checkFile
  :: [(Address, Type)]
  -> Bool
  -> FilePath
  -> (TCError -> Expectation)
  -> Expectation
checkFile originatedContracts wellTyped file onError = do
  c <- prepareContract (Just file)
  case doTC c of
    Left err
      | wellTyped ->
        expectationFailure $
        "Typechecker unexpectedly failed on " <> show file <> ": " <> displayException err
      | otherwise -> onError err
    Right _
      | not wellTyped ->
        assertFailure $
        "Typechecker unexpectedly considered " <> show file <> " well-typed."
      | otherwise -> pass
  where
    doTC = typeCheckContract (M.fromList originatedContracts)

unit_Unreachable_code :: Assertion
unit_Unreachable_code = do
  let file = "contracts/ill-typed/fail-before-nop.tz"
  let ics = InstrCallStack [] (srcPos 3 13)
  econtract <- readContract @'T.TUnit @'T.TUnit file <$> readFile file
  econtract @?= Left (ICETypeCheck $ TCUnreachableCode ics (one $ Un.WithSrcEx ics $ Un.SeqEx []))

test_StackRef :: [TestTree]
test_StackRef =
  [ testProperty "Typecheck fails when ref is out of bounds" $
      let instr = printStRef 2
          hst = stackEl ::& stackEl ::& SNil
      in case
          runTypeCheck (error "no contract param") mempty $
          typeCheckList [Un.WithSrcEx def $ Un.PrimEx instr] hst
          of
            Left err -> total $ show @Text err
            Right _ -> error "Typecheck unexpectedly succeded"
  , testProperty "Typecheck time is reasonably bounded" $ within 1000 $
    -- Making code processing performance scale with code size looks like a
    -- good property, so we'd like to avoid scenario when user tries to
    -- access 100500-th element of stack and typecheck hangs as a result
    let instr = printStRef 100000000000
        hst = stackEl ::& SNil
    in case
        runTypeCheck (error "no contract param") mempty $
        typeCheckList [Un.WithSrcEx def $ Un.PrimEx instr] hst
      of
        Left err -> total $ show @Text err
        Right _ -> error "Typecheck unexpectedly succeded"
  ]
  where
    printStRef i = Un.EXT . Un.UPRINT $ Un.PrintComment [Right (Un.StackRef i)]
    stackEl = (sing @'T.TUnit, T.NStar, noAnn)
