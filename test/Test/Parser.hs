module Test.Parser
  ( spec
  ) where

import qualified Data.List.NonEmpty as NE
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (ErrorFancy(ErrorCustom), ParseError(FancyError), bundleErrors)

import Michelson.Parser as P
import Michelson.Types as Mo
import Michelson.Untyped

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
  it "printComment parser test" printCommentParserTest
  it "parserException test" parserExceptionTest

parseContractsTest :: Expectation
parseContractsTest = do
  files <- mappend <$> getWellTypedContracts <*> getIllTypedContracts
  mapM_ checkFile files

checkFile :: FilePath -> Expectation
checkFile file = do
  code <- readFile file
  parse P.program file code `shouldSatisfy` isRight

valueParserTest :: Expectation
valueParserTest = do
  P.parseNoEnv P.value "" "{}" `shouldBe`
    (Right Mo.ValueNil)
  P.parseNoEnv P.value "" "{PUSH int 5;}" `shouldBe`
    (Right . Mo.ValueLambda $ NE.fromList
      [Mo.Prim (Mo.PUSH noAnn (Mo.Type (Mo.Tc Mo.CInt) noAnn) (Mo.ValueInt 5))]
    )
  P.parseNoEnv P.value "" "{1; 2}" `shouldBe`
    (Right . Mo.ValueSeq $ NE.fromList
      [Mo.ValueInt 1, Mo.ValueInt 2]
    )
  P.parseNoEnv P.value "" "{Elt 1 2; Elt 3 4}" `shouldBe`
    (Right . Mo.ValueMap $ NE.fromList
      [Mo.Elt (Mo.ValueInt 1) (Mo.ValueInt 2), Mo.Elt (Mo.ValueInt 3) (Mo.ValueInt 4)]
    )

stringLiteralTest :: Expectation
stringLiteralTest = do
  P.parseNoEnv P.stringLiteral "" "\"\"" `shouldSatisfy` isRight
  P.parseNoEnv P.stringLiteral "" "\" \\t \\b \\n\\r  \"" `shouldSatisfy` isRight
  P.parseNoEnv P.stringLiteral "" "\"abacaba \\t \n\n\r\"" `shouldSatisfy` isRight
  P.parseNoEnv P.stringLiteral "" "\"abacaba \\t \n\n\r a\"" `shouldSatisfy` isLeft
  P.parseNoEnv P.stringLiteral "" "\"abacaba \\t \\n\\n\\r" `shouldSatisfy` isLeft

ifParsersTest :: Expectation
ifParsersTest = do
  P.parseNoEnv P.codeEntry "" "{IF {} {};}" `shouldBe`
    (Prelude.Right [Mo.Prim $ Mo.IF [] []])
  P.parseNoEnv P.codeEntry "" "{IFEQ {} {};}" `shouldBe`
    (Prelude.Right [Mo.Mac $ Mo.IFX (Mo.EQ noAnn) [] []])
  P.parseNoEnv P.codeEntry "" "{IFCMPEQ {} {};}" `shouldBe`
    (Prelude.Right [Mo.Mac $ Mo.IFCMP (Mo.EQ noAnn) noAnn [] []])

mapParsersTest :: Expectation
mapParsersTest = do
  parseNoEnv P.codeEntry "" "{MAP {};}" `shouldBe`
    (Prelude.Right [Mo.Prim $ Mo.MAP noAnn []])
  parseNoEnv P.codeEntry "" "{MAP_CAR {};}" `shouldBe`
    (Prelude.Right [Mo.Mac $ Mo.MAP_CADR [Mo.A] noAnn noAnn []])

pairParsersTest :: Expectation
pairParsersTest = do
  P.parseNoEnv P.codeEntry "" "{PAIR;}" `shouldBe`
    Prelude.Right [Mo.Prim $ PAIR noAnn noAnn noAnn noAnn]
  P.parseNoEnv P.codeEntry "" "{PAIR %a;}" `shouldBe`
    Prelude.Right [Mac $ PAPAIR (P (F (noAnn, Mo.ann "a")) (F (noAnn,noAnn))) noAnn noAnn]
  P.parseNoEnv P.codeEntry "" "{PAPAIR;}" `shouldBe`
    Prelude.Right
      [Mac $
        PAPAIR (P (F (noAnn,noAnn)) (P (F (noAnn,noAnn)) (F (noAnn,noAnn))))
          noAnn noAnn
      ]

pairTypeParserTest :: Expectation
pairTypeParserTest = do
  P.parseNoEnv P.type_ "" "pair unit unit" `shouldBe` Right unitPair
  P.parseNoEnv P.type_ "" "(unit, unit)" `shouldBe` Right unitPair
  where
    unitPair :: Mo.Type
    unitPair =
      Mo.Type (Mo.TPair noAnn noAnn (Mo.Type Mo.TUnit noAnn) (Mo.Type Mo.TUnit noAnn)) noAnn

orTypeParserTest :: Expectation
orTypeParserTest = do
  P.parseNoEnv P.type_ "" "or unit unit" `shouldBe` Right unitOr
  P.parseNoEnv P.type_ "" "(unit | unit)" `shouldBe` Right unitOr
  where
    unitOr :: Mo.Type
    unitOr =
      Mo.Type (Mo.TOr noAnn noAnn (Mo.Type Mo.TUnit noAnn) (Mo.Type Mo.TUnit noAnn)) noAnn

lambdaTypeParserTest :: Expectation
lambdaTypeParserTest = do
  P.parseNoEnv P.type_ "" "lambda unit unit" `shouldBe` Right lambdaUnitUnit
  P.parseNoEnv P.type_ "" "\\unit -> unit" `shouldBe` Right lambdaUnitUnit
  where
    lambdaUnitUnit :: Mo.Type
    lambdaUnitUnit =
      Mo.Type (Mo.TLambda (Mo.Type Mo.TUnit noAnn) (Mo.Type Mo.TUnit noAnn)) noAnn

listTypeParserTest :: Expectation
listTypeParserTest = do
  P.parseNoEnv P.type_ "" "list unit" `shouldBe` Right unitList
  P.parseNoEnv P.type_ "" "[unit]" `shouldBe` Right unitList
  where
    unitList :: Mo.Type
    unitList =
      Mo.Type (Mo.TList (Mo.Type Mo.TUnit noAnn)) noAnn

setTypeParserTest :: Expectation
setTypeParserTest = do
  P.parseNoEnv P.type_ "" "set int" `shouldBe` Right intSet
  P.parseNoEnv P.type_ "" "{int}" `shouldBe` Right intSet
  where
    intSet :: Mo.Type
    intSet =
      Mo.Type (Mo.TSet (Mo.Comparable Mo.CInt noAnn)) noAnn

pairTest :: Expectation
pairTest = do
  P.parseNoEnv P.value "" "Pair Unit Unit" `shouldBe` Right unitPair
  P.parseNoEnv P.value "" "(Unit, Unit)" `shouldBe` Right unitPair
  where
    unitPair :: Mo.Value' Mo.ParsedOp
    unitPair = Mo.ValuePair Mo.ValueUnit Mo.ValueUnit

printCommentParserTest :: Expectation
printCommentParserTest = do
  P.parseNoEnv P.printComment "" "\"Sides are %[0] x %[1]\"" `shouldBe`
    Right (PrintComment [Left "Sides are ", Right (StackRef 0), Left " x ", Right (StackRef 1)])
  P.parseNoEnv P.printComment "" "\"%[0] x\"" `shouldBe`
    Right (PrintComment [Right (StackRef 0), Left " x"])
  P.parseNoEnv P.printComment "" "\"%[0]x%[1]\"" `shouldBe`
    Right (PrintComment [Right (StackRef 0), Left "x", Right (StackRef 1)])
  P.parseNoEnv P.printComment "" "\"%[0]%[1]\"" `shouldBe`
    Right (PrintComment [Right (StackRef 0), Right (StackRef 1)])
  P.parseNoEnv P.printComment "" "\"xxx\"" `shouldBe`
    Right (PrintComment [Left "xxx"])
  P.parseNoEnv P.printComment "" "\"\"" `shouldBe`
    Right (PrintComment [])

parserExceptionTest :: Expectation
parserExceptionTest = do
  handleCustomError "0x000" P.value OddNumberBytesException
  handleCustomError "Right 0x000" P.value OddNumberBytesException
  handleCustomError "\"abacaba \\t \n\n\r a\"" P.value UnexpectedLineBreak
  handleCustomError "kek" P.type_ UnknownTypeException
  where
    handleCustomError
      :: Text -> Parser a -> CustomParserException -> Expectation
    handleCustomError text parser customException =
      case P.parseNoEnv parser "" text of
        Right _ -> expectationFailure "expecting parser to fail"
        Left bundle -> case toList $ bundleErrors bundle of
          [FancyError _ errorSet] -> case toList errorSet of
            [(ErrorCustom e)] -> e `shouldBe` customException
            _ -> expectationFailure "expecting single ErrorCustom"
          _ -> expectationFailure "expecting single ErrorCustom"
