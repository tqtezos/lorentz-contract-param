module Test.Nop
  ( nopHandlerSpec
  ) where

import Test.Hspec (Expectation, Spec, describe, expectationFailure, it)

import Michelson.TypeCheck (HST(..), SomeHST(..))
import Michelson.Typed (extractNotes, fromMType, withSomeSingT)
import Michelson.Untyped (noAnn, Type (..), T (..), CT (..), ann)
import Morley.Nop (nopHandler)
import Morley.Types (NopInstr(..), StackTypePattern(..))

nopHandlerSpec :: Spec
nopHandlerSpec = describe "nopHandler STACKTYPE tests" $ do
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
    p2 = StkCons t1 (StkCons t2 StkEmpty)
    p3 = StkCons t1 (StkCons t2 StkRest)

    test1 = (STACKTYPE StkEmpty, convertToHST [])
    test2 = (STACKTYPE p2, convertToHST [t1, t2])
    test3 = (STACKTYPE p3, convertToHST [t1, t2, t3])
    test4 = (STACKTYPE p3, convertToHST [t1, t2])

    test5 = (STACKTYPE StkEmpty, convertToHST [t1])
    test6 = (STACKTYPE p2, convertToHST [t1, t2, t3])
    test7 = (STACKTYPE p2, convertToHST [t1])
    test8 = (STACKTYPE p3, convertToHST [t1])
    test9 = (STACKTYPE p2, convertToHST [t1, t3])

    t1 = Type (T_option (ann "f") (Type T_key (ann "key"))) (ann "opt")
    t2 = Type (T_pair (ann "f") (ann "s") (Type T_unit "x") (Type T_signature "s")) noAnn
    t3 = Type (T_comparable T_int) (ann "tint")

    convertToHST :: [Type] -> SomeHST
    convertToHST [] = SomeHST SNil
    convertToHST (t : ts) = withSomeSingT (fromMType t) $ \sing ->
      let nt = either (const $ error "unexpected trouble with extracting annotations") id (extractNotes t sing) in
      case convertToHST ts of
        SomeHST is -> SomeHST ((sing, nt, noAnn) ::& is)

    runNopTest :: (NopInstr, SomeHST) -> Bool -> Expectation
    runNopTest (ni, si) correct = case (nopHandler ni si, correct) of
      (Right _, False) -> expectationFailure $ "Test expected to fail but it passed"
      (Left e, True)   -> expectationFailure $ "Test expected to pass but it failed with error: " <> show e
      _                -> pass
