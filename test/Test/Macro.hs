module Test.Macro
  ( spec
  ) where

import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Morley.Macro
import Morley.Types

spec :: Spec
spec = describe "Macros tests" $ do
  it "expand test" expandTest
  it "expandFlat test" expandFlatTest
  it "papair test" expandPapairTest
  it "unpapair test" expandUnpapairTest
  it "expandCadr test" expandCadrTest
  it "expandSetCadr test" expandSetCadrTest
  it "expandMapCadr test" expandMapCadrTest
  it "mapLeaves test" mapLeavesTest
  it "flatten test" flattenTest

expandPapairTest :: Expectation
expandPapairTest = do
  expandPapair pair n n `shouldBe` [PRIM $ PAIR n n n n]
  expandPapair (P leaf pair) n n `shouldBe`
    [PRIM $ DIP [MAC $ PAPAIR pair n n], PRIM $ PAIR n n n n]
  expandPapair (P pair leaf) n n `shouldBe`
    [PRIM $ PAIR n n n n, MAC $ PAPAIR pair n n]
  expandPapair (P pair pair) n n `shouldBe`
    [MAC $ PAPAIR pair n n, MAC $ PAPAIR pair n n, PRIM $ PAIR n n n n]
  where
    n = Nothing
    leaf = F (n, n)
    pair = P leaf leaf

expandUnpapairTest :: Expectation
expandUnpapairTest = do
  expandUnpapair pair `shouldBe`
    [PRIM $ DUP n, PRIM $ CAR n n, PRIM $ DIP [PRIM $ CDR n n]]
  expandUnpapair (P leaf pair) `shouldBe`
    [MAC $ UNPAIR leaf, PRIM $ DIP [MAC $ UNPAIR pair]]
  expandUnpapair (P pair leaf) `shouldBe`
    [MAC $ UNPAIR leaf, MAC $ UNPAIR pair]
  expandUnpapair (P pair pair) `shouldBe`
    [MAC $ UNPAIR (P leaf leaf), MAC $ UNPAIR pair, MAC $ UNPAIR pair]
  where
    n = Nothing
    leaf = F (n, n)
    pair = P leaf leaf

expandCadrTest :: Expectation
expandCadrTest = do
  expandCadr (A:[]) v f `shouldBe` [PRIM $ CAR v f]
  expandCadr (D:[]) v f `shouldBe` [PRIM $ CDR v f]
  expandCadr (A:xs) v f `shouldBe` [PRIM $ CAR n n, MAC $ CADR xs v f]
  expandCadr (D:xs) v f `shouldBe` [PRIM $ CDR n n, MAC $ CADR xs v f]
  where
    v = Just "var"
    f = Just "field"
    n = Nothing
    xs = [A, D]

expandSetCadrTest :: Expectation
expandSetCadrTest = do
  expandSetCadr (A:[]) v f `shouldBe` PRIM <$> [CDR v f, SWAP, PAIR n n n n]
  expandSetCadr (D:[]) v f `shouldBe` PRIM <$> [CAR v f, PAIR n n n n]
  expandSetCadr (A:xs) v f `shouldBe`
    PRIM <$> [DUP n, DIP [PRIM $ CAR n n, MAC $ SET_CADR xs v f], CDR n n, SWAP, PAIR n n n n]
  expandSetCadr (D:xs) v f `shouldBe`
    PRIM <$> [DUP n, DIP [PRIM $ CDR n n, MAC $ SET_CADR xs v f], CDR n n, SWAP, PAIR n n n n]
  where
    v = Just "var"
    f = Just "field"
    n = Nothing
    xs = [A, D]

expandMapCadrTest :: Expectation
expandMapCadrTest = do
  expandMapCadr (A:[]) v f ops `shouldBe`
    PRIM <$> [DUP n, CDR n n, DIP [PRIM $ CAR v f, SEQ ops], SWAP, PAIR n n n n]
  expandMapCadr (D:[]) v f ops `shouldBe`
    [PRIM $ DUP n, PRIM $ CDR v f, SEQ ops, PRIM $ SWAP, PRIM $ CAR n n, PRIM $ PAIR n n n n]
  expandMapCadr (A:xs) v f ops `shouldBe`
    PRIM <$> [DUP n, DIP [PRIM $ CAR n n, MAC $ MAP_CADR xs v f ops], CDR n n, PAIR n n n n]
  expandMapCadr (D:xs) v f ops `shouldBe`
    PRIM <$> [DUP n, DIP [PRIM $ CDR n n, MAC $ MAP_CADR xs v f ops], CAR n n, PAIR n n n n]
  where
    v = Just "var"
    f = Just "field"
    n = Nothing
    xs = [A, D]
    ops = [PRIM $ DUP n]

mapLeavesTest :: Expectation
mapLeavesTest = do
  mapLeaves [(v, f), (v, f)] pair `shouldBe` P (F (v, f)) (F (v, f))
  mapLeaves annotations (P pair (F (n, n))) `shouldBe`
    P (P (leaf "var1" "field1") (leaf "var2" "field2")) (leaf "var3" "field3")
  mapLeaves annotations (P pair pair) `shouldBe`
    P (P (leaf "var1" "field1") (leaf "var2" "field2")) (P (leaf "var3" "field3") (F (n, n)))
  where
    annotations = zip (Just <$> ["var1", "var2", "var3"]) (Just <$> ["field1", "field2", "field3"])
    n = Nothing
    v = Just "var"
    f = Just "field"
    leaf v' f' = F (Just v', Just f')
    pair = P (F (n, n)) (F (n, n))

flattenTest :: Expectation
flattenTest = do
  flatten (SEQ_EX [PRIM_EX $ SWAP, PRIM_EX $ SWAP]) `shouldBe`
    [SWAP, SWAP]
  flatten (SEQ_EX [SEQ_EX [SEQ_EX [PRIM_EX $ SWAP], PRIM_EX $ SWAP], PRIM_EX $ SWAP]) `shouldBe`
    [SWAP, SWAP, SWAP]

expandFlatTest :: Expectation
expandFlatTest = do
  expandFlat [papair] `shouldBe` [DIP [Op $ PAIR n n n n], PAIR n n n n]
  expandFlat [diiiip] `shouldBe` [DIP [Op $ DIP [Op $ DIP [Op $ DIP[Op $ SWAP]]]]]
  where
    n = Nothing
    papair :: ParsedOp
    papair =
      MAC (PAPAIR (P (F (Nothing,Nothing)) (P (F (Nothing,Nothing)) (F (Nothing,Nothing)))) Nothing Nothing)
    diiiip :: ParsedOp
    diiiip = MAC (DIIP 4 [PRIM SWAP])

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
