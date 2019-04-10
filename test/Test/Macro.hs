module Test.Macro
  ( unit_PAPAIR
  , unit_UNPAIR
  , unit_CADR
  , unit_SET_CADR
  , unit_MAP_CADR
  , unit_mapLeaves
  , unit_expand
  , unit_expandValue
  ) where

import Data.Default (def)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)

import Michelson.ErrorPos (InstrCallStack(..), defExistingIcs, defMacroIcs, insideMacroPos)
import Michelson.Macro
import Michelson.Untyped (ExpandedOp(..), InstrAbstract(..), Value, Value'(..), ann, noAnn)
import Test.Hspec.Expectations (Expectation, shouldBe)
import Michelson.Untyped
  (ExpandedOp(..), InstrAbstract(..), Value, Value'(..), ann, noAnn)


unit_PAPAIR :: Expectation
unit_PAPAIR = do
  expandPapair def pair n n `shouldBe` [primEx $ PAIR n n n n]
  expandPapair def (P leaf pair) n n `shouldBe`
    [primEx $ DIP (expandMacro def $ PAPAIR pair n n), primEx $ PAIR n n n n]
  expandList [mac $ PAPAIR (P pair leaf) n n] `shouldBe`
    [SeqEx [primEx $ PAIR n n n n, primEx $ PAIR n n n n]]
  expandList [mac $ PAPAIR (P pair pair) n n] `shouldBe`
    [SeqEx [primEx (PAIR n n n n),
             primEx (DIP [primEx (PAIR n n n n)]),
             primEx (PAIR n n n n)]]
  where
    mac = flip Mac def
    primEx = flip PrimEx defMacroIcs
    n = noAnn
    leaf = F (n, n)
    pair = P leaf leaf

unit_UNPAIR :: Expectation
unit_UNPAIR = do
  expandUnpapair def pair `shouldBe`
    [primEx $ DUP n, primEx $ CAR n n, primEx $ DIP [primEx $ CDR n n]]
  expandList [mac $ UNPAIR $ P leaf pair] `shouldBe`
    [SeqEx [primEx (DUP n),
             primEx (CAR n n),
             primEx (DIP [primEx (CDR n n),
                          primEx (DUP n),
                          primEx (CAR n n),
                          primEx (DIP [primEx (CDR n n)])])]]
  expandList [mac $ UNPAIR $ P pair leaf] `shouldBe`
    [SeqEx [primEx (DUP n),
            primEx (DIP [primEx (CDR n n)]),
            primEx (CAR n n),
            primEx (DUP n),
            primEx (CAR n n),
            primEx (DIP [primEx (CDR n n)])]]
  expandList [mac $ UNPAIR $ P pair pair] `shouldBe`
     [SeqEx $ expandP ++ [primEx $ DIP expandP] ++ expandP]
  where
    mac = flip Mac def
    primEx = flip PrimEx defMacroIcs
    expandP = primEx <$> [DUP n, CAR n n, DIP [primEx $ CDR n n]]
    n = noAnn
    leaf = F (n, n)
    pair = P leaf leaf

unit_CADR :: Expectation
unit_CADR = do
  expandCadr def ([A]) v f `shouldBe` [primEx $ CAR v f]
  expandCadr def ([D]) v f `shouldBe` [primEx $ CDR v f]
  expandCadr def (A:xs) v f `shouldBe` primEx (CAR n n) : expandMacro def (CADR xs v f)
  expandCadr def (D:xs) v f `shouldBe` primEx (CDR n n) : expandMacro def (CADR xs v f)
  where
    primEx = flip PrimEx defMacroIcs
    v = ann "var"
    f = ann "field"
    n = noAnn
    xs = [A, D]

unit_SET_CADR :: Expectation
unit_SET_CADR = do
  expandSetCadr def [A] v f `shouldBe` primEx <$> [ DUP noAnn, CAR noAnn f, DROP
                                            , CDR (ann "%%") noAnn, SWAP, PAIR noAnn v f (ann "@")]
  expandSetCadr def [D] v f `shouldBe` primEx <$> [ DUP noAnn, CDR noAnn f, DROP
                                            , CAR (ann "%%") noAnn, PAIR noAnn v (ann "@") f]
  expandSetCadr def (A:xs) v f `shouldBe`
    primEx <$> [DUP noAnn, DIP (primEx carN : expandMacro def (SET_CADR xs noAnn f)), cdrN, SWAP, pairN]
  expandSetCadr def (D:xs) v f `shouldBe`
    primEx <$> [DUP noAnn, DIP (primEx cdrN : expandMacro def (SET_CADR xs noAnn f)), carN, pairN]
  where
    primEx = flip PrimEx defMacroIcs
    v = ann "var"
    f = ann "field"
    xs = [A, D]
    carN = CAR noAnn noAnn
    cdrN = CDR noAnn noAnn
    pairN = PAIR noAnn v noAnn noAnn

unit_MAP_CADR :: Expectation
unit_MAP_CADR = do
  expandMapCadr def [A] v f ops `shouldBe`
    primEx <$> [DUP noAnn, cdrN, DIP [primEx $ CAR noAnn f, SeqEx ops'], SWAP, pairN]
  expandMapCadr def [D] v f ops `shouldBe`
    concat [primEx <$> [DUP noAnn, CDR noAnn f], [SeqEx ops'], primEx <$> [SWAP, carN, pairN]]
  expandMapCadr def (A:xs) v f ops `shouldBe`
    primEx <$> [DUP noAnn, DIP (primEx carN : expandMacro def (MAP_CADR xs noAnn f ops)), cdrN, SWAP, pairN]
  expandMapCadr def (D:xs) v f ops `shouldBe`
    primEx <$> [DUP noAnn, DIP (primEx cdrN : expandMacro def (MAP_CADR xs noAnn f ops)), carN, pairN]
  where
    primEx = flip PrimEx defMacroIcs
    v = ann "var"
    f = ann "field"
    n = noAnn
    xs = [A, D]
    ops = [Prim (DUP n) def]
    ops' = [PrimEx (DUP n) defExistingIcs]
    carN = CAR noAnn noAnn
    cdrN = CDR noAnn noAnn
    pairN = PAIR noAnn v noAnn noAnn

unit_mapLeaves :: Expectation
unit_mapLeaves = do
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

unit_expand :: Expectation
unit_expand = do
  expand def diip `shouldBe` expandedDiip
  expand def (prim $ IF [diip] [diip]) `shouldBe` (primEx $ IF [expandedDiip] [expandedDiip])
  expand def (Seq [diip, diip]) `shouldBe` (SeqEx $ [expandedDiip, expandedDiip])
  where
    prim = flip Prim def
    primEx = flip PrimEx defExistingIcs
    primEx' = flip PrimEx (InstrCallStack [] (insideMacroPos 0 0))
    mac = flip Mac def
    diip :: ParsedOp
    diip = mac (DIIP 2 [prim SWAP])
    expandedDiip :: ExpandedOp
    expandedDiip = SeqEx [primEx' (DIP [SeqEx [primEx' (DIP [primEx SWAP])]])]

unit_expandValue :: Expectation
unit_expandValue = do
  expandValue parsedPair `shouldBe` expandedPair
  expandValue parsedPapair `shouldBe` expandedPapair
  expandValue parsedLambdaWithMac `shouldBe` expandedLambdaWithMac
  where
    mac = flip Mac def
    primEx = flip PrimEx defMacroIcs

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
      one (mac (PAPAIR (P (F (noAnn, noAnn)) (P (F (noAnn, noAnn)) (F (noAnn, noAnn)))) noAnn noAnn))

    expandedLambdaWithMac :: Value
    expandedLambdaWithMac = ValueLambda . one $ SeqEx
      [ primEx $ DIP [primEx $ PAIR noAnn noAnn noAnn noAnn]
      , primEx $ PAIR noAnn noAnn noAnn noAnn
      ]
