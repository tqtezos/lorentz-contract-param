module Test.Macro
  ( spec
  ) where

import Michelson.Untyped (UntypedValue)
import Morley.Macro
import Morley.Types
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Macros tests" $ do
  it "expand test" expandTest
  it "papair test" expandPapairTest
  it "unpapair test" expandUnpapairTest
  it "expandCadr test" expandCadrTest
  it "expandSetCadr test" expandSetCadrTest
  it "expandMapCadr test" expandMapCadrTest
  it "mapLeaves test" mapLeavesTest
  it "expandValue test" expandValueTest

expandPapairTest :: Expectation
expandPapairTest = do
  expandPapair pair n n `shouldBe` [PRIM $ PAIR n n n n]
  expandPapair (P leaf pair) n n `shouldBe`
    [PRIM $ DIP [MAC $ PAPAIR pair n n], PRIM $ PAIR n n n n]
  expandList [MAC $ PAPAIR (P pair leaf) n n] `shouldBe`
    [SEQ_EX [SEQ_EX [PRIM_EX $ PAIR n n n n], PRIM_EX $ PAIR n n n n]]
  expandList [MAC $ PAPAIR (P pair pair) n n] `shouldBe`
    [SEQ_EX [SEQ_EX [PRIM_EX (PAIR n n n n)],
             PRIM_EX (DIP [SEQ_EX [PRIM_EX (PAIR n n n n)]]),
             PRIM_EX (PAIR n n n n)]]
  where
    n = noAnn
    leaf = F (n, n)
    pair = P leaf leaf

expandUnpapairTest :: Expectation
expandUnpapairTest = do
  expandUnpapair pair `shouldBe`
    [PRIM $ DUP n, PRIM $ CAR n n, PRIM $ DIP [PRIM $ CDR n n]]
  expandList [MAC $ UNPAIR $ P leaf pair] `shouldBe`
    [SEQ_EX [PRIM_EX (DUP n),
             PRIM_EX (CAR n n),
             PRIM_EX (DIP [PRIM_EX (CDR n n),
                           SEQ_EX [PRIM_EX (DUP n),
                                   PRIM_EX (CAR n n),
                                   PRIM_EX (DIP [PRIM_EX (CDR n n)])]])]]
  expandList [MAC $ UNPAIR $ P pair leaf] `shouldBe`
    [SEQ_EX [PRIM_EX (DUP n),
             PRIM_EX (DIP [PRIM_EX (CDR n n)]),
             PRIM_EX (CAR n n),
             SEQ_EX [PRIM_EX (DUP n),
                     PRIM_EX (CAR n n),
                     PRIM_EX (DIP [PRIM_EX (CDR n n)])]]]
  expandList [MAC $ UNPAIR $ P pair pair] `shouldBe`
     [SEQ_EX $ one expandP ++ [PRIM_EX $ DIP $ one expandP] ++ one expandP]
  where
    expandP = SEQ_EX $ PRIM_EX <$> [DUP n, CAR n n, DIP [PRIM_EX $ CDR n n]]
    n = noAnn
    leaf = F (n, n)
    pair = P leaf leaf

expandCadrTest :: Expectation
expandCadrTest = do
  expandCadr ([A]) v f `shouldBe` [PRIM $ CAR v f]
  expandCadr ([D]) v f `shouldBe` [PRIM $ CDR v f]
  expandCadr (A:xs) v f `shouldBe` [PRIM $ CAR n n, MAC $ CADR xs v f]
  expandCadr (D:xs) v f `shouldBe` [PRIM $ CDR n n, MAC $ CADR xs v f]
  where
    v = ann "var"
    f = ann "field"
    n = noAnn
    xs = [A, D]

expandSetCadrTest :: Expectation
expandSetCadrTest = do
  expandSetCadr [A] v f `shouldBe` PRIM <$> [ DUP noAnn, CAR noAnn f, DROP
                                            , CDR (ann "%%") noAnn, SWAP, PAIR noAnn v f (ann "@")]
  expandSetCadr [D] v f `shouldBe` PRIM <$> [ DUP noAnn, CDR noAnn f, DROP
                                            , CAR (ann "%%") noAnn, PAIR noAnn v (ann "@") f]
  expandSetCadr (A:xs) v f `shouldBe`
    PRIM <$> [DUP noAnn, DIP [PRIM carN, MAC $ SET_CADR xs noAnn f], cdrN, SWAP, pairN]
  expandSetCadr (D:xs) v f `shouldBe`
    PRIM <$> [DUP noAnn, DIP [PRIM cdrN, MAC $ SET_CADR xs noAnn f], carN, pairN]
  where
    v = ann "var"
    f = ann "field"
    xs = [A, D]
    carN = CAR noAnn noAnn
    cdrN = CDR noAnn noAnn
    pairN = PAIR noAnn v noAnn noAnn

expandMapCadrTest :: Expectation
expandMapCadrTest = do
  expandMapCadr [A] v f ops `shouldBe`
    PRIM <$> [DUP noAnn, cdrN, DIP [PRIM $ CAR noAnn f, SEQ ops], SWAP, pairN]
  expandMapCadr [D] v f ops `shouldBe`
    concat [PRIM <$> [DUP noAnn, CDR noAnn f], [SEQ ops], PRIM <$> [SWAP, carN, pairN]]
  expandMapCadr (A:xs) v f ops `shouldBe`
    PRIM <$> [DUP noAnn, DIP [PRIM $ carN, MAC $ MAP_CADR xs noAnn f ops], cdrN, SWAP, pairN]
  expandMapCadr (D:xs) v f ops `shouldBe`
    PRIM <$> [DUP noAnn, DIP [PRIM $ cdrN, MAC $ MAP_CADR xs noAnn f ops], carN, pairN]
  where
    v = ann "var"
    f = ann "field"
    n = noAnn
    xs = [A, D]
    ops = [PRIM $ DUP n]
    carN = CAR noAnn noAnn
    cdrN = CDR noAnn noAnn
    pairN = PAIR noAnn v noAnn noAnn

mapLeavesTest :: Expectation
mapLeavesTest = do
  mapLeaves [(v, f), (v, f)] pair `shouldBe` P (F (v, f)) (F (v, f))
  mapLeaves annotations (P pair (F (n, n))) `shouldBe`
    P (P (leaf "var1" "field1") (leaf "var2" "field2")) (leaf "var3" "field3")
  mapLeaves annotations (P pair pair) `shouldBe`
    P (P (leaf "var1" "field1") (leaf "var2" "field2")) (P (leaf "var3" "field3") (F (n, n)))
  where
    annotations = zip (ann <$> ["var1", "var2", "var3"]) (ann <$> ["field1", "field2", "field3"])
    n = noAnn
    v = ann "var"
    f = ann "field"
    leaf v' f' = F (ann v', ann f')
    pair = P (F (n, n)) (F (n, n))

expandTest :: Expectation
expandTest = do
  expand diip `shouldBe` expandedDiip
  expand (PRIM $ IF [diip] [diip]) `shouldBe` (PRIM_EX $ IF [expandedDiip] [expandedDiip])
  expand (SEQ [diip, diip]) `shouldBe` (SEQ_EX $ [expandedDiip, expandedDiip])
  where
    diip :: ParsedOp
    diip = MAC (DIIP 2 [PRIM SWAP])
    expandedDiip :: ExpandedOp
    expandedDiip = SEQ_EX [PRIM_EX (DIP [SEQ_EX [PRIM_EX (DIP [PRIM_EX SWAP])]])]

expandValueTest :: Expectation
expandValueTest = do
  expandValue parsedPair `shouldBe` expandedPair
  expandValue parsedPapair `shouldBe` expandedPapair
  expandValue parsedLambdaWithMac `shouldBe` expandedLambdaWithMac
  where
    parsedPair :: Value ParsedOp
    parsedPair = ValuePair (ValueInt 5) (ValueInt 5)

    expandedPair :: UntypedValue
    expandedPair = ValuePair (ValueInt 5) (ValueInt 5)

    parsedPapair :: Value ParsedOp
    parsedPapair = ValuePair (ValuePair (ValueInt 5) (ValueInt 5)) (ValueInt 5)

    expandedPapair :: UntypedValue
    expandedPapair = ValuePair (ValuePair (ValueInt 5) (ValueInt 5)) (ValueInt 5)

    parsedLambdaWithMac :: Value ParsedOp
    parsedLambdaWithMac = ValueLambda $
      one (MAC (PAPAIR (P (F (noAnn, noAnn)) (P (F (noAnn, noAnn)) (F (noAnn, noAnn)))) noAnn noAnn))

    expandedLambdaWithMac :: UntypedValue
    expandedLambdaWithMac = ValueLambda . one $ SEQ_EX
      [ PRIM_EX $ DIP [SEQ_EX $ one $ PRIM_EX $ PAIR noAnn noAnn noAnn noAnn]
      , PRIM_EX $ PAIR noAnn noAnn noAnn noAnn
      ]
