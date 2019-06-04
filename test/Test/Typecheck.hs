{-# LANGUAGE ViewPatterns #-}

module Test.Typecheck
  ( unit_Good_contracts
  , unit_Bad_contracts
  , test_OriginatedContracts
  , test_srcPosition
  , unit_Unreachable_code
  , test_Roundtrip
  , test_StackRef
  , test_TCTypeError_display
  ) where

import Data.Default (def)
import qualified Data.Kind as Kind
import qualified Data.Map as M
import Data.Singletons (SingI(..))
import Data.Typeable (typeRep)
import Fmt (build, pretty)
import Test.Hspec (expectationFailure)
import Test.Hspec.Expectations (Expectation)
import Test.HUnit (Assertion, assertFailure, (@?=))
import Test.QuickCheck (Arbitrary, property, total, within)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)

import Michelson.ErrorPos (InstrCallStack(..), LetName(..), Pos(..), SrcPos(..), srcPos)
import Michelson.Runtime (prepareContract)
import Michelson.Test.Gen ()
import Michelson.Test.Import (ImportContractError(..), readContract)
import Michelson.TypeCheck
import qualified Michelson.Typed as T
import Michelson.Untyped (CT(..), T(..), Type(..), noAnn)
import qualified Michelson.Untyped as Un
import Tezos.Address (Address, unsafeParseAddress)
import Tezos.Core (Timestamp)
import Util.IO (readFileUtf8)

import Test.Util.Contracts (getIllTypedContracts, getWellTypedContracts)

unit_Good_contracts :: Assertion
unit_Good_contracts = mapM_ (\f -> checkFile [] True f (const pass)) =<< getWellTypedContracts

unit_Bad_contracts :: Assertion
unit_Bad_contracts = mapM_ (\f -> checkFile [] False f (const pass)) =<< getIllTypedContracts

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
    checkFile' a b f = checkFile a b f (const pass)
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
  [ testProperty "Verify instruction position in a typecheck error" $ do
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
  let file = "contracts/ill-typed/fail_before_nop.tz"
  let ics = InstrCallStack [] (srcPos 3 13)
  econtract <- readContract @'T.TUnit @'T.TUnit file <$> readFileUtf8 file
  econtract @?= Left (ICETypeCheck $ TCUnreachableCode ics (one $ Un.WithSrcEx ics $ Un.SeqEx []))

test_Roundtrip :: [TestTree]
test_Roundtrip =
  [ testGroup "Value"
    [ roundtripValue @Integer
    , roundtripValue @Timestamp
    ]
  ]
  where
  roundtripValue
    :: forall (a :: Kind.Type).
        ( Each [Typeable, SingI, T.HasNoOp] '[T.ToT a]
        , Typeable a, Arbitrary (T.Value $ T.ToT a)
        )
    => TestTree
  roundtripValue =
    testProperty (show $ typeRep (Proxy @a)) $
      property $ \(val :: T.Value (T.ToT a)) ->
        let uval = T.untypeValue val
            runTC = runTypeCheckTest . usingReaderT (def @InstrCallStack)
        in case runTC $ typeVerifyValue uval of
            Right got -> got @?= val
            Left err -> expectationFailure $
                        "Type check unexpectedly failed: " <> pretty err

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

test_TCTypeError_display :: [TestTree]
test_TCTypeError_display =
  -- One may say that it's madness to write tests on 'Buildable' instances,
  -- but IMO (martoon) it's worth resulting duplication because tests allow
  -- avoiding silly errors like lost spaces and ensuring general sanity
  -- of used way to display content.
  [ testCase "TypeEqError" $
      build (TypeEqError T.TUnit T.TKey)
      @?= "Types not equal: unit /= key"

  , testCase "StackEqError" $
      build (StackEqError [T.TUnit, T.Tc T.CBytes] [])
      @?= "Stacks not equal: [unit, bytes] /= []"

  , testCase "UnsupportedTypes" $
      build (UnsupportedTypes [T.TUnit])
      @?= "Unsupported types: [unit]"

  , testCase "Unknown type" $
      build (UnknownType T.TUnit)
      @?= "Unknown type `unit`"
  ]
