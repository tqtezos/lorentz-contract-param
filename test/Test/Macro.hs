module Test.Macro
  ( spec
  ) where

import Michelson.Macro
import Michelson.Untyped (Value)
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
  expandPapair pair n n `shouldBe` [Prim $ PAIR n n n n]
  expandPapair (P leaf pair) n n `shouldBe`
    [Prim $ DIP [Mac $ PAPAIR pair n n], Prim $ PAIR n n n n]
  expandList [Mac $ PAPAIR (P pair leaf) n n] `shouldBe`
    [SeqEx [PrimEx $ PAIR n n n n, PrimEx $ PAIR n n n n]]
  expandList [Mac $ PAPAIR (P pair pair) n n] `shouldBe`
    [SeqEx [PrimEx (PAIR n n n n),
             PrimEx (DIP [PrimEx (PAIR n n n n)]),
             PrimEx (PAIR n n n n)]]
  where
    n = noAnn
    leaf = F (n, n)
    pair = P leaf leaf

expandUnpapairTest :: Expectation
expandUnpapairTest = do
  expandUnpapair pair `shouldBe`
    [Prim $ DUP n, Prim $ CAR n n, Prim $ DIP [Prim $ CDR n n]]
  expandList [Mac $ UNPAIR $ P leaf pair] `shouldBe`
    [SeqEx [PrimEx (DUP n),
             PrimEx (CAR n n),
             PrimEx (DIP [PrimEx (CDR n n),
                           SeqEx [PrimEx (DUP n),
                                   PrimEx (CAR n n),
                                   PrimEx (DIP [PrimEx (CDR n n)])]])]]
  expandList [Mac $ UNPAIR $ P pair leaf] `shouldBe`
    [SeqEx [PrimEx (DUP n),
             PrimEx (DIP [PrimEx (CDR n n)]),
             PrimEx (CAR n n),
             SeqEx [PrimEx (DUP n),
                     PrimEx (CAR n n),
                     PrimEx (DIP [PrimEx (CDR n n)])]]]
  expandList [Mac $ UNPAIR $ P pair pair] `shouldBe`
     [SeqEx $ one expandP ++ [PrimEx $ DIP $ one expandP] ++ one expandP]
  where
    expandP = SeqEx $ PrimEx <$> [DUP n, CAR n n, DIP [PrimEx $ CDR n n]]
    n = noAnn
    leaf = F (n, n)
    pair = P leaf leaf

expandCadrTest :: Expectation
expandCadrTest = do
  expandCadr ([A]) v f `shouldBe` [Prim $ CAR v f]
  expandCadr ([D]) v f `shouldBe` [Prim $ CDR v f]
  expandCadr (A:xs) v f `shouldBe` [Prim $ CAR n n, Mac $ CADR xs v f]
  expandCadr (D:xs) v f `shouldBe` [Prim $ CDR n n, Mac $ CADR xs v f]
  where
    v = ann "var"
    f = ann "field"
    n = noAnn
    xs = [A, D]

expandSetCadrTest :: Expectation
expandSetCadrTest = do
  expandSetCadr [A] v f `shouldBe` Prim <$> [ DUP noAnn, CAR noAnn f, DROP
                                            , CDR (ann "%%") noAnn, SWAP, PAIR noAnn v f (ann "@")]
  expandSetCadr [D] v f `shouldBe` Prim <$> [ DUP noAnn, CDR noAnn f, DROP
                                            , CAR (ann "%%") noAnn, PAIR noAnn v (ann "@") f]
  expandSetCadr (A:xs) v f `shouldBe`
    Prim <$> [DUP noAnn, DIP [Prim carN, Mac $ SET_CADR xs noAnn f], cdrN, SWAP, pairN]
  expandSetCadr (D:xs) v f `shouldBe`
    Prim <$> [DUP noAnn, DIP [Prim cdrN, Mac $ SET_CADR xs noAnn f], carN, pairN]
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
    Prim <$> [DUP noAnn, cdrN, DIP [Prim $ CAR noAnn f, Seq ops], SWAP, pairN]
  expandMapCadr [D] v f ops `shouldBe`
    concat [Prim <$> [DUP noAnn, CDR noAnn f], [Seq ops], Prim <$> [SWAP, carN, pairN]]
  expandMapCadr (A:xs) v f ops `shouldBe`
    Prim <$> [DUP noAnn, DIP [Prim $ carN, Mac $ MAP_CADR xs noAnn f ops], cdrN, SWAP, pairN]
  expandMapCadr (D:xs) v f ops `shouldBe`
    Prim <$> [DUP noAnn, DIP [Prim $ cdrN, Mac $ MAP_CADR xs noAnn f ops], carN, pairN]
  where
    v = ann "var"
    f = ann "field"
    n = noAnn
    xs = [A, D]
    ops = [Prim $ DUP n]
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
  expand (Prim $ IF [diip] [diip]) `shouldBe` (PrimEx $ IF [expandedDiip] [expandedDiip])
  expand (Seq [diip, diip]) `shouldBe` (SeqEx $ [expandedDiip, expandedDiip])
  where
    diip :: ParsedOp
    diip = Mac (DIIP 2 [Prim SWAP])
    expandedDiip :: ExpandedOp
    expandedDiip = SeqEx [PrimEx (DIP [SeqEx [PrimEx (DIP [PrimEx SWAP])]])]

expandValueTest :: Expectation
expandValueTest = do
  expandValue parsedPair `shouldBe` expandedPair
  expandValue parsedPapair `shouldBe` expandedPapair
  expandValue parsedLambdaWithMac `shouldBe` expandedLambdaWithMac
  where
    parsedPair :: Value' ParsedOp
    parsedPair = ValuePair (ValueInt 5) (ValueInt 5)

    expandedPair :: Value
    expandedPair = ValuePair (ValueInt 5) (ValueInt 5)

    parsedPapair :: Value' ParsedOp
    parsedPapair = ValuePair (ValuePair (ValueInt 5) (ValueInt 5)) (ValueInt 5)

    expandedPapair :: Value
    expandedPapair = ValuePair (ValuePair (ValueInt 5) (ValueInt 5)) (ValueInt 5)

    parsedLambdaWithMac :: Value' ParsedOp
    parsedLambdaWithMac = ValueLambda $
      one (Mac (PAPAIR (P (F (noAnn, noAnn)) (P (F (noAnn, noAnn)) (F (noAnn, noAnn)))) noAnn noAnn))

    expandedLambdaWithMac :: Value
    expandedLambdaWithMac = ValueLambda . one $ SeqEx
      [ PrimEx $ DIP [PrimEx $ PAIR noAnn noAnn noAnn noAnn]
      , PrimEx $ PAIR noAnn noAnn noAnn noAnn
      ]
