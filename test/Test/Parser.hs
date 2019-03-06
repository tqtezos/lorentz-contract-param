module Test.Parser
  ( spec
  ) where

import Test.Hspec (Expectation, Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (parse)

import Morley.Parser as P
import Morley.Types as M

import Test.Util.Contracts (getIllTypedContracts, getWellTypedContracts)

spec :: Spec
spec = describe "Parser tests" $ do
  it "Successfully parses contracts examples from contracts/" parseContractsTest
  it "Test stringLiteral" stringLiteralTest
  it "IF parsers test" ifParsersTest
  it "MAP parsers test" mapParsersTest
  it "PAIR parsers test" pairParsersTest
  it "pair type parser test" pairTypeParserTest
  it "or type parser test" orTypeParserTest
  it "lambda type parser test" lambdaTypeParserTest
  it "list type parser test" listTypeParserTest
  it "set type parser test" setTypeParserTest
  it "pair constructor test" pairTest
  it "value parser test" valueParserTest

parseContractsTest :: Expectation
parseContractsTest = do
  files <- mappend <$> getWellTypedContracts <*> getIllTypedContracts
  mapM_ checkFile files

checkFile :: FilePath -> Expectation
checkFile file = do
  code <- readFile file
  parse P.contract file code `shouldSatisfy` isRight

valueParserTest :: Expectation
valueParserTest = do
  parse P.value "" "{PUSH int 5;}" `shouldBe`
    (Right $ M.ValueLambda [M.PRIM (M.PUSH noAnn (M.Type (M.T_comparable M.T_int) noAnn) (M.ValueInt 5))])
  parse P.value "" "{1; 2}" `shouldBe`
    (Right $ M.ValueSeq [M.ValueInt 1, M.ValueInt 2])
  parse P.value "" "{Elt 1 2; Elt 3 4}" `shouldBe`
    (Right $ M.ValueMap [M.Elt (M.ValueInt 1) (M.ValueInt 2), M.Elt (M.ValueInt 3) (M.ValueInt 4)])

stringLiteralTest :: Expectation
stringLiteralTest = do
  parse P.stringLiteral "" "\"\"" `shouldSatisfy` isRight
  parse P.stringLiteral "" "\" \\t \\b \\n\\r  \"" `shouldSatisfy` isRight
  parse P.stringLiteral "" "\"abacaba \\t \n\n\r\"" `shouldSatisfy` isRight
  parse P.stringLiteral "" "\"abacaba \\t \n\n\r a\"" `shouldSatisfy` isLeft
  parse P.stringLiteral "" "\"abacaba \\t \\n\\n\\r" `shouldSatisfy` isLeft

ifParsersTest :: Expectation
ifParsersTest = do
  parse P.ops "" "{IF {} {};}" `shouldBe`
    (Prelude.Right [M.PRIM $ M.IF [] []])
  parse P.ops "" "{IFEQ {} {};}" `shouldBe`
    (Prelude.Right [M.MAC $ M.IFX (M.EQ noAnn) [] []])
  parse P.ops "" "{IFCMPEQ {} {};}" `shouldBe`
    (Prelude.Right [M.MAC $ M.IFCMP (M.EQ noAnn) noAnn [] []])

mapParsersTest :: Expectation
mapParsersTest = do
  parse P.ops "" "{MAP {};}" `shouldBe`
    (Prelude.Right [M.PRIM $ M.MAP noAnn []])
  parse P.ops "" "{MAP_CAR {};}" `shouldBe`
    (Prelude.Right [M.MAC $ M.MAP_CADR [M.A] noAnn noAnn []])

pairParsersTest :: Expectation
pairParsersTest = do
  parse P.ops "" "{PAIR;}" `shouldBe`
    Prelude.Right [M.PRIM $ PAIR noAnn noAnn noAnn noAnn]
  parse P.ops "" "{PAIR %a;}" `shouldBe`
    Prelude.Right [MAC $ PAPAIR (P (F (noAnn, M.ann "a")) (F (noAnn,noAnn))) noAnn noAnn]
  parse P.ops "" "{PAPAIR;}" `shouldBe`
    Prelude.Right
      [MAC $
        PAPAIR (P (F (noAnn,noAnn)) (P (F (noAnn,noAnn)) (F (noAnn,noAnn))))
          noAnn noAnn
      ]

pairTypeParserTest :: Expectation
pairTypeParserTest = do
  parse P.type_ "" "pair unit unit" `shouldBe` Right unitPair
  parse P.type_ "" "(unit, unit)" `shouldBe` Right unitPair
  where
    unitPair :: M.Type
    unitPair =
      M.Type (M.T_pair noAnn noAnn (M.Type M.T_unit noAnn) (M.Type M.T_unit noAnn)) noAnn

orTypeParserTest :: Expectation
orTypeParserTest = do
  parse P.type_ "" "or unit unit" `shouldBe` Right unitOr
  parse P.type_ "" "(unit | unit)" `shouldBe` Right unitOr
  where
    unitOr :: M.Type
    unitOr =
      M.Type (M.T_or noAnn noAnn (M.Type M.T_unit noAnn) (M.Type M.T_unit noAnn)) noAnn

lambdaTypeParserTest :: Expectation
lambdaTypeParserTest = do
  parse P.type_ "" "lambda unit unit" `shouldBe` Right lambdaUnitUnit
  parse P.type_ "" "\\unit -> unit" `shouldBe` Right lambdaUnitUnit
  where
    lambdaUnitUnit :: M.Type
    lambdaUnitUnit =
      M.Type (M.T_lambda (M.Type M.T_unit noAnn) (M.Type M.T_unit noAnn)) noAnn

listTypeParserTest :: Expectation
listTypeParserTest = do
  parse P.type_ "" "list unit" `shouldBe` Right unitList
  parse P.type_ "" "[unit]" `shouldBe` Right unitList
  where
    unitList :: M.Type
    unitList =
      M.Type (M.T_list (M.Type M.T_unit noAnn)) noAnn

setTypeParserTest :: Expectation
setTypeParserTest = do
  parse P.type_ "" "set int" `shouldBe` Right intSet
  parse P.type_ "" "{int}" `shouldBe` Right intSet
  where
    intSet :: M.Type
    intSet =
      M.Type (M.T_set (M.Comparable M.T_int noAnn)) noAnn

pairTest :: Expectation
pairTest = do
  parse P.value "" "Pair Unit Unit" `shouldBe` Right unitPair
  parse P.value "" "(Unit, Unit)" `shouldBe` Right unitPair
  where
    unitPair :: M.Value M.ParsedOp
    unitPair = M.ValuePair M.ValueUnit M.ValueUnit
