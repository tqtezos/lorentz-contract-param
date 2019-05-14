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

import Test.Hspec (Expectation, shouldBe)

import Michelson.ErrorPos (InstrCallStack(..), LetName(..), SrcPos, srcPos)
import Michelson.Macro
import Michelson.Untyped (ExpandedOp(..), InstrAbstract(..), Value, Value'(..), ann, noAnn)

defPos :: SrcPos
defPos = srcPos 1 1

defICS :: InstrCallStack
defICS = InstrCallStack [] defPos

unit_PAPAIR :: Expectation
unit_PAPAIR = do
  expandPapair defICS pair n n `shouldBe` [primEx $ PAIR n n n n]
  expandPapair defICS (P leaf pair) n n `shouldBe`
    [primEx $ DIP (expandMacro defICS $ PAPAIR pair n n), primEx $ PAIR n n n n]
  expandList [mac $ PAPAIR (P pair leaf) n n] `shouldBe`
    [WithSrcEx defICS $ SeqEx [primEx $ PAIR n n n n, primEx $ PAIR n n n n]]
  expandList [mac $ PAPAIR (P pair pair) n n] `shouldBe`
    [WithSrcEx defICS $ SeqEx [primEx (PAIR n n n n),
             primEx (DIP [primEx (PAIR n n n n)]),
             primEx (PAIR n n n n)]]
  where
    mac = flip Mac defPos
    primEx = PrimEx
    n = noAnn
    leaf = F (n, n)
    pair = P leaf leaf

unit_UNPAIR :: Expectation
unit_UNPAIR = do
  expandUnpapair defICS pair `shouldBe`
    [primEx $ DUP n, primEx $ CAR n n, primEx $ DIP [primEx $ CDR n n]]
  expandList [mac $ UNPAIR $ P leaf pair] `shouldBe`
    [WithSrcEx defICS $ SeqEx [primEx (DUP n),
             primEx (CAR n n),
             primEx (DIP [primEx (CDR n n),
                          primEx (DUP n),
                          primEx (CAR n n),
                          primEx (DIP [primEx (CDR n n)])])]]
  expandList [mac $ UNPAIR $ P pair leaf] `shouldBe`
    [WithSrcEx defICS $ SeqEx [primEx (DUP n),
            primEx (DIP [primEx (CDR n n)]),
            primEx (CAR n n),
            primEx (DUP n),
            primEx (CAR n n),
            primEx (DIP [primEx (CDR n n)])]]
  expandList [mac $ UNPAIR $ P pair pair] `shouldBe`
     [WithSrcEx defICS $ SeqEx $ expandP ++ [primEx $ DIP expandP] ++ expandP]
  where
    mac = flip Mac defPos
    primEx = PrimEx
    expandP = primEx <$> [DUP n, CAR n n, DIP [primEx $ CDR n n]]
    n = noAnn
    leaf = F (n, n)
    pair = P leaf leaf

unit_CADR :: Expectation
unit_CADR = do
  expandCadr defICS ([A]) v f `shouldBe` [primEx $ CAR v f]
  expandCadr defICS ([D]) v f `shouldBe` [primEx $ CDR v f]
  expandCadr defICS (A:xs) v f `shouldBe` primEx (CAR n n) : expandMacro defICS (CADR xs v f)
  expandCadr defICS (D:xs) v f `shouldBe` primEx (CDR n n) : expandMacro defICS (CADR xs v f)
  where
    primEx = PrimEx
    v = ann "var"
    f = ann "field"
    n = noAnn
    xs = [A, D]

unit_SET_CADR :: Expectation
unit_SET_CADR = do
  expandSetCadr defICS [A] v f `shouldBe` primEx <$> [ DUP noAnn, CAR noAnn f, DROP
                                            , CDR (ann "%%") noAnn, SWAP, PAIR noAnn v f (ann "@")]
  expandSetCadr defICS [D] v f `shouldBe` primEx <$> [ DUP noAnn, CDR noAnn f, DROP
                                            , CAR (ann "%%") noAnn, PAIR noAnn v (ann "@") f]
  expandSetCadr defICS (A:xs) v f `shouldBe`
    primEx <$> [DUP noAnn, DIP (primEx carN : expandMacro defICS (SET_CADR xs noAnn f)), cdrN, SWAP, pairN]
  expandSetCadr defICS (D:xs) v f `shouldBe`
    primEx <$> [DUP noAnn, DIP (primEx cdrN : expandMacro defICS (SET_CADR xs noAnn f)), carN, pairN]
  where
    primEx = PrimEx
    v = ann "var"
    f = ann "field"
    xs = [A, D]
    carN = CAR noAnn noAnn
    cdrN = CDR noAnn noAnn
    pairN = PAIR noAnn v noAnn noAnn

unit_MAP_CADR :: Expectation
unit_MAP_CADR = do
  expandMapCadr defICS [A] v f ops `shouldBe`
    primEx <$> [DUP noAnn, cdrN, DIP [primEx $ CAR noAnn f, SeqEx ops'], SWAP, pairN]
  expandMapCadr defICS [D] v f ops `shouldBe`
    concat [primEx <$> [DUP noAnn, CDR noAnn f], [SeqEx ops'], primEx <$> [SWAP, carN, pairN]]
  expandMapCadr defICS (A:xs) v f ops `shouldBe`
    primEx <$> [DUP noAnn, DIP (primEx carN : expandMacro defICS (MAP_CADR xs noAnn f ops)), cdrN, SWAP, pairN]
  expandMapCadr defICS (D:xs) v f ops `shouldBe`
    primEx <$> [DUP noAnn, DIP (primEx cdrN : expandMacro defICS (MAP_CADR xs noAnn f ops)), carN, pairN]
  where
    primEx = PrimEx
    v = ann "var"
    f = ann "field"
    n = noAnn
    xs = [A, D]
    ops = [Prim (DUP n) defPos]
    ops' = [WithSrcEx defICS $ PrimEx (DUP n)]
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
  expand [LetName "a"] diip `shouldBe` expandedDiip
  expand [LetName "a"] (prim $ IF [diip] [diip]) `shouldBe` (primEx $ IF [expandedDiip] [expandedDiip])
  expand [LetName "a"] (Seq [diip, diip] defPos) `shouldBe` (WithSrcEx aIcs $ SeqEx $ [expandedDiip, expandedDiip])
  where
    aIcs = InstrCallStack [LetName "a"] defPos
    prim = flip Prim defPos
    primEx = WithSrcEx aIcs . PrimEx
    primEx' = PrimEx
    mac = flip Mac defPos
    diip :: ParsedOp
    diip = mac (DIIP 2 [prim SWAP])
    expandedDiip :: ExpandedOp
    expandedDiip = WithSrcEx aIcs $ SeqEx [primEx' (DIP [primEx' (DIP [primEx SWAP])])]

unit_expandValue :: Expectation
unit_expandValue = do
  expandValue parsedPair `shouldBe` expandedPair
  expandValue parsedPapair `shouldBe` expandedPapair
  expandValue parsedLambdaWithMac `shouldBe` expandedLambdaWithMac
  where
    mac = flip Mac defPos
    primEx = PrimEx

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
    expandedLambdaWithMac = ValueLambda . one $ WithSrcEx defICS $ SeqEx
      [ primEx $ DIP [primEx $ PAIR noAnn noAnn noAnn noAnn]
      , primEx $ PAIR noAnn noAnn noAnn noAnn
      ]
