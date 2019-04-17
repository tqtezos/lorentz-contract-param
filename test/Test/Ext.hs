module Test.Ext
  ( typeCheckHandlerSpec
  , interpretHandlerSpec
  ) where

import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldSatisfy)

import Michelson.Interpret (InterpreterState(..), MorleyLogs(..), interpret)
import Michelson.Test (specWithTypedContract)
import Michelson.Test.Dummy (dummyContractEnv)
import Michelson.TypeCheck (HST(..), SomeHST(..), runTypeCheckT, typeCheckExt, typeCheckList)
import Michelson.Typed (CValue(..), extractNotes, fromUType, withSomeSingT)
import qualified Michelson.Typed as T
import Michelson.Types
import Michelson.Untyped
  (CT(..), ExpandedExtInstr, ExtInstrAbstract(..), StackTypePattern(..), T(..), Type(..), ann,
  noAnn)

interpretHandlerSpec :: Spec
interpretHandlerSpec = describe "interpretHandler PRINT/TEST_ASSERT tests" $
  specWithTypedContract "contracts/testassert_square.tz" $ \c -> do
    it "TEST_ASSERT assertion passed" $ do
      runTest True c 100 100
      runTest True c 1 1
    it "TEST_ASSERT assertion failed" $ do
      runTest False c 0 100
      runTest False c -1 -2
  where
    runTest corr contract x y = do
      let x' = T.VC $ CvInt x :: T.Value ('T.Tc 'T.CInt)
      let y' = T.VC $ CvInt y :: T.Value ('T.Tc 'T.CInt)
      let area' = T.VC $ CvInt $ x * y :: T.Value ('T.Tc 'T.CInt)
      let check (a, InterpreterState s _) =
            if corr then isRight a && s == MorleyLogs ["Area is " <> show area']
            else isLeft a && s == MorleyLogs ["Sides are " <> show x' <> " x " <> show y']
      interpret contract (T.VPair (x', y')) T.VUnit dummyContractEnv `shouldSatisfy` check

typeCheckHandlerSpec :: Spec
typeCheckHandlerSpec = describe "typeCheckExt STACKTYPE tests" $ do
  it "Correct test on [] pattern" $ runNopTest test1 True
  it "Correct test on [a, b] pattern" $ runNopTest test2 True
  it "Correct test on [a, b, ...] pattern" $ runNopTest test3 True
  it "Correct test on [a, b, ...] pattern and stack [a, b]" $ runNopTest test4 True

  it "Failed test on [] pattern and stack [a]" $ runNopTest test5 False
  it "Failed test on [a, b] pattern and stack [a, b, c]" $ runNopTest test6 False
  it "Failed test on [a, b] pattern and stack [a]" $ runNopTest test7 False
  it "Failed test on [a, b, ...] pattern and stack [a]" $ runNopTest test8 False
  it "Failed test on [a, b] pattern and stack [a, c]" $ runNopTest test9 False
  where
    p2 = StkCons (TyCon t1) (StkCons (TyCon t2) StkEmpty)
    p3 = StkCons (TyCon t1) (StkCons (TyCon t2) StkRest)

    test1 = (STACKTYPE StkEmpty, convertToHST [])
    test2 = (STACKTYPE p2, convertToHST [t1, t2])
    test3 = (STACKTYPE p3, convertToHST [t1, t2, t3])
    test4 = (STACKTYPE p3, convertToHST [t1, t2])

    test5 = (STACKTYPE StkEmpty, convertToHST [t1])
    test6 = (STACKTYPE p2, convertToHST [t1, t2, t3])
    test7 = (STACKTYPE p2, convertToHST [t1])
    test8 = (STACKTYPE p3, convertToHST [t1])
    test9 = (STACKTYPE p2, convertToHST [t1, t3])

    t1 = Type (TOption (ann "f") (Type TKey (ann "key"))) (ann "opt")
    t2 = Type (TPair (ann "f") (ann "s") (Type TUnit "x") (Type TSignature "s")) noAnn
    t3 = Type (Tc CInt) (ann "tint")

    convertToHST :: [Type] -> SomeHST
    convertToHST [] = SomeHST SNil
    convertToHST (t : ts) = withSomeSingT (fromUType t) $ \sing ->
      let nt = either (const $ error "unexpected trouble with extracting annotations") id (extractNotes t sing) in
      case convertToHST ts of
        SomeHST is -> SomeHST ((sing, nt, noAnn) ::& is)

    nh (ni, si) =
      runTypeCheckT (Type TKey noAnn) mempty $ typeCheckExt typeCheckList ni [] si

    runNopTest :: (ExpandedExtInstr, SomeHST) -> Bool -> Expectation
    runNopTest (ui, SomeHST hst) correct = case (nh (ui, hst), correct) of
      (Right _, False) -> expectationFailure $ "Test expected to fail but it passed"
      (Left e, True)   -> expectationFailure $ "Test expected to pass but it failed with error: " <> show e
      _                -> pass
