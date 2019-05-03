module Test.Typecheck
  ( unit_Good_contracts
  , unit_Bad_contracts
  , test_OriginatedContracts
  , unit_Unreachable_code
  , test_StackRef
  ) where

import qualified Data.Map as M
import Data.Singletons (sing)
import Test.HUnit (Assertion, assertFailure, (@?=))
import Test.QuickCheck (total, within)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)

import Michelson.Runtime (prepareContract)
import Michelson.Test.Import (ImportContractError(..), readContract)
import Michelson.TypeCheck
import qualified Michelson.Typed as T
import Michelson.Untyped (CT(..), T(..), Type(..), noAnn)
import qualified Michelson.Untyped as Un
import Tezos.Address (Address, unsafeParseAddress)

import Test.Util.Contracts (getIllTypedContracts, getWellTypedContracts)

unit_Good_contracts :: Assertion
unit_Good_contracts = mapM_ (checkFile [] True) =<< getWellTypedContracts

unit_Bad_contracts :: Assertion
unit_Bad_contracts = mapM_ (checkFile [] False) =<< getIllTypedContracts

test_OriginatedContracts :: [TestTree]
test_OriginatedContracts =
  [ testCase "Successfully typechecked PUSH contract considering originated contracts" $ do
    checkFile [(cAddr, tPair intP intQ)] True pushContrFile
    checkFile [(cAddr, tPair int intQ)] True pushContrFile
    checkFile [(cAddr, tPair int int)] True pushContrFile
  , testCase "Report an error on PUSH contract because of mismatched types" $ do
    checkFile [(cAddr, tPair intP intP)] False pushContrFile
    checkFile [(cAddr, tPair intQ intQ)] False pushContrFile
    checkFile [(cAddr, tPair string intQ)] False pushContrFile
  ]
  where
    pushContrFile = "contracts/ill-typed/push_contract.tz"
    tPair t1 t2 = Type (TPair noAnn noAnn t1 t2) noAnn
    intP = Type (Tc CInt) "p"
    intQ = Type (Tc CInt) "q"
    int = Type (Tc CInt) noAnn
    string = Type (Tc CString) noAnn

    cAddr = unsafeParseAddress "KT1WsLzQ61xtMNJHfwgCHh2RnALGgFAzeSx9"

checkFile :: [(Address, Type)] -> Bool -> FilePath -> Assertion
checkFile originatedContracts wellTyped file = do
  c <- prepareContract (Just file)
  case doTC c of
    Left err
      | wellTyped ->
        assertFailure $
        "Typechecker unexpectedly failed on " <> show file <> ": " <> err
      | otherwise -> pass
    Right _
      | not wellTyped ->
        assertFailure $
        "Typechecker unexpectedly considered " <> show file <> " well-typed."
      | otherwise -> pass
  where
    doTC = either (Left . displayException) (\_ -> pure ()) .
      typeCheckContract (M.fromList originatedContracts)

unit_Unreachable_code :: Assertion
unit_Unreachable_code = do
  let file = "contracts/ill-typed/fail-before-nop.tz"
  econtract <- readContract @'T.TUnit @'T.TUnit file <$> readFile file
  econtract @?= Left (ICETypeCheck $ TCUnreachableCode (one $ Un.SeqEx []))

test_StackRef :: [TestTree]
test_StackRef =
  [ testProperty "Typecheck fails when ref is out of bounds" $
      let instr = printStRef 2
          hst = stackEl ::& stackEl ::& SNil
      in case
          runTypeCheckT (error "no contract param") mempty $
          typeCheckList [Un.PrimEx instr] hst
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
        runTypeCheckT (error "no contract param") mempty $
        typeCheckList [Un.PrimEx instr] hst
      of
        Left err -> total $ show @Text err
        Right _ -> error "Typecheck unexpectedly succeded"
  ]
  where
    printStRef i = Un.EXT . Un.UPRINT $ Un.PrintComment [Right (Un.StackRef i)]
    stackEl = (sing @'T.TUnit, T.NStar, noAnn)
