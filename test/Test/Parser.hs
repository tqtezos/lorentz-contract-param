module Test.Parser
  ( unit_Parse_contracts
  , unit_Value
  , unit_string_literal
  , unit_IF
  , unit_MAP
  , unit_PAIR
  , unit_pair_type
  , unit_or_type
  , unit_lambda_type
  , unit_list_type
  , unit_set_type
  , unit_explicitType
  , unit_Pair_constructor
  , unit_PrintComment
  , unit_ParserException
  , unit_letType
  ) where

import qualified Data.List.NonEmpty as NE
import Test.Hspec.Expectations (Expectation, expectationFailure, shouldBe, shouldSatisfy)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (ErrorFancy(ErrorCustom), ParseError(FancyError), bundleErrors)

import Michelson.ErrorPos (srcPos)
import Michelson.Macro as Mo
import Michelson.Parser as P
import Michelson.Untyped as Mo
import Util.IO

import Test.Util.Contracts (getIllTypedContracts, getWellTypedContracts)

unit_Parse_contracts :: Expectation
unit_Parse_contracts = do
  files <- mappend <$> getWellTypedContracts <*> getIllTypedContracts
  mapM_ checkFile files
  where
    checkFile :: FilePath -> Expectation
    checkFile file = do
      code <- readFileUtf8 file
      parse P.program file code `shouldSatisfy` isRight

unit_Value :: Expectation
unit_Value = do
  P.parseNoEnv P.value "" "{}" `shouldBe`
    Right Mo.ValueNil
  P.parseNoEnv P.value "" "{PUSH int 5;}" `shouldBe`
    (Right . ValueLambda $ NE.fromList
      [Mo.Prim (Mo.PUSH noAnn (Mo.Type (Mo.Tc Mo.CInt) noAnn) (Mo.ValueInt 5)) (srcPos 0 1)]
    )
  P.parseNoEnv P.value "" "{1; 2}" `shouldBe`
    (Right . Mo.ValueSeq $ NE.fromList
      [Mo.ValueInt 1, Mo.ValueInt 2]
    )
  P.parseNoEnv P.value "" "{Elt 1 2; Elt 3 4}" `shouldBe`
    (Right . Mo.ValueMap $ NE.fromList
      [Mo.Elt (Mo.ValueInt 1) (Mo.ValueInt 2), Mo.Elt (Mo.ValueInt 3) (Mo.ValueInt 4)]
    )

unit_string_literal :: Expectation
unit_string_literal = do
  P.parseNoEnv P.stringLiteral "" "\"\"" `shouldSatisfy` isRight
  P.parseNoEnv P.stringLiteral "" "\" \\n  \"" `shouldSatisfy` isRight
  P.parseNoEnv P.stringLiteral "" "\"abacaba \\t \n\n\r a\"" `shouldSatisfy` isLeft
  P.parseNoEnv P.stringLiteral "" "\"abacaba \\t \\n\\n\\r" `shouldSatisfy` isLeft

unit_IF :: Expectation
unit_IF = do
  P.parseNoEnv P.codeEntry "" "{IF {} {};}" `shouldBe`
    Prelude.Right [Mo.Prim (Mo.IF [] []) (srcPos 0 1)]
  P.parseNoEnv P.codeEntry "" "{IFEQ {} {};}" `shouldBe`
    Prelude.Right [Mo.Mac (Mo.IFX (Mo.EQ noAnn) [] []) (srcPos 0 1)]
  P.parseNoEnv P.codeEntry "" "{IFCMPEQ {} {};}" `shouldBe`
    Prelude.Right [Mo.Mac (Mo.IFCMP (Mo.EQ noAnn) noAnn [] []) (srcPos 0 1)]

unit_MAP :: Expectation
unit_MAP = do
  parseNoEnv P.codeEntry "" "{MAP {};}" `shouldBe`
    Prelude.Right [Mo.Prim (Mo.MAP noAnn []) (srcPos 0 1)]
  parseNoEnv P.codeEntry "" "{MAP_CAR {};}" `shouldBe`
    Prelude.Right [Mo.Mac (Mo.MAP_CADR [Mo.A] noAnn noAnn []) (srcPos 0 1)]

unit_PAIR :: Expectation
unit_PAIR = do
  P.parseNoEnv P.codeEntry "" "{PAIR;}" `shouldBe`
    Prelude.Right [Mo.Prim (PAIR noAnn noAnn noAnn noAnn) (srcPos 0 1)]
  P.parseNoEnv P.codeEntry "" "{PAIR %a;}" `shouldBe`
    Prelude.Right [Mac (PAPAIR (P (F (noAnn, Mo.ann "a")) (F (noAnn,noAnn))) noAnn noAnn) (srcPos 0 1)]
  P.parseNoEnv P.codeEntry "" "{PAPAIR;}" `shouldBe`
    Prelude.Right
      [flip Mac (srcPos 0 1) $
        PAPAIR (P (F (noAnn,noAnn)) (P (F (noAnn,noAnn)) (F (noAnn,noAnn))))
          noAnn noAnn
      ]

unit_pair_type :: Expectation
unit_pair_type = do
  P.parseNoEnv P.type_ "" "pair unit unit" `shouldBe` Right unitPair
  P.parseNoEnv P.type_ "" "(unit, unit)" `shouldBe` Right unitPair
  P.parseNoEnv P.type_ "" "(Parameter, (int, (Storage, bool)))"
    `shouldSatisfy` isRight
  P.parseNoEnv P.type_ "" "(Parameter, Parameter, Storage, bool)"
    `shouldSatisfy` isRight
  where
    unitPair :: Mo.Type
    unitPair =
      Mo.Type (Mo.TPair noAnn noAnn (Mo.Type Mo.TUnit noAnn) (Mo.Type Mo.TUnit noAnn)) noAnn

unit_or_type :: Expectation
unit_or_type = do
  P.parseNoEnv P.type_ "" "or unit unit" `shouldBe` Right unitOr
  P.parseNoEnv P.type_ "" "(unit | unit)" `shouldBe` Right unitOr
  P.parseNoEnv P.type_ "" "Parameter | (int | (Storage | bool)))"
    `shouldSatisfy` isRight
  where
    unitOr :: Mo.Type
    unitOr =
      Mo.Type (Mo.TOr noAnn noAnn (Mo.Type Mo.TUnit noAnn) (Mo.Type Mo.TUnit noAnn)) noAnn

unit_lambda_type :: Expectation
unit_lambda_type = do
  P.parseNoEnv P.type_ "" "lambda unit unit" `shouldBe` Right lambdaUnitUnit
  P.parseNoEnv P.type_ "" "\\unit -> unit" `shouldBe` Right lambdaUnitUnit
  P.parseNoEnv P.type_ "" "lambda int (Storage, int)" `shouldSatisfy` isRight
  where
    lambdaUnitUnit :: Mo.Type
    lambdaUnitUnit =
      Mo.Type (Mo.TLambda (Mo.Type Mo.TUnit noAnn) (Mo.Type Mo.TUnit noAnn)) noAnn

unit_list_type :: Expectation
unit_list_type = do
  P.parseNoEnv P.type_ "" "list unit" `shouldBe` Right unitList
  P.parseNoEnv P.type_ "" "[unit]" `shouldBe` Right unitList
  P.parseNoEnv P.type_ "" "[(Parameter, Storage)]" `shouldSatisfy` isRight
  where
    unitList :: Mo.Type
    unitList =
      Mo.Type (Mo.TList (Mo.Type Mo.TUnit noAnn)) noAnn

unit_set_type :: Expectation
unit_set_type = do
  P.parseNoEnv P.type_ "" "set int" `shouldBe` Right intSet
  P.parseNoEnv P.type_ "" "{int}" `shouldBe` Right intSet
  where
    intSet :: Mo.Type
    intSet =
      Mo.Type (Mo.TSet (Mo.Comparable Mo.CInt noAnn)) noAnn

unit_explicitType :: Expectation
unit_explicitType = do
  P.parseNoEnv P.explicitType "" "Parameter" `shouldSatisfy` isLeft
  P.parseNoEnv P.explicitType "" "Storage" `shouldSatisfy` isLeft
  P.parseNoEnv P.explicitType "" "List Parameter" `shouldSatisfy` isLeft
  P.parseNoEnv P.explicitType "" "Void int Parameter" `shouldSatisfy` isLeft
  P.parseNoEnv P.explicitType "" "(Parameter, (int, (bool, Storage)))"
    `shouldSatisfy` isLeft
  P.parseNoEnv P.explicitType "" "int"
    `shouldBe` (Right $ Mo.Type (Mo.Tc Mo.CInt) noAnn)

unit_Pair_constructor :: Expectation
unit_Pair_constructor = do
  P.parseNoEnv P.value "" "Pair Unit Unit" `shouldBe` Right unitPair
  P.parseNoEnv P.value "" "(Unit, Unit)" `shouldBe` Right unitPair
  where
    unitPair :: Mo.Value' Mo.ParsedOp
    unitPair = Mo.ValuePair Mo.ValueUnit Mo.ValueUnit

unit_PrintComment :: Expectation
unit_PrintComment = do
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

unit_ParserException :: Expectation
unit_ParserException = do
  handleCustomError "0x000" P.value OddNumberBytesException
  handleCustomError "Right 0x000" P.value OddNumberBytesException
  handleCustomError "kek" P.type_ UnknownTypeException
  handleCustomError "\"aaa\\r\"" P.stringLiteral
    (StringLiteralException (InvalidEscapeSequence 'r'))
  handleCustomError "\"aaa\\b\"" P.stringLiteral
    (StringLiteralException (InvalidEscapeSequence 'b'))
  handleCustomError "\"aaa\\t\"" P.stringLiteral
    (StringLiteralException (InvalidEscapeSequence 't'))
  handleCustomError "\"aaa\n\"" P.stringLiteral
    (StringLiteralException (InvalidChar '\n'))
  handleCustomError "\"aaa\r\"" P.stringLiteral
    (StringLiteralException (InvalidChar '\r'))
  where
    handleCustomError
      :: HasCallStack => Text -> Parser a -> CustomParserException -> Expectation
    handleCustomError text parser customException =
      case P.parseNoEnv parser "" text of
        Right _ -> expectationFailure "expecting parser to fail"
        Left bundle -> case toList $ bundleErrors bundle of
          [FancyError _ errorSet] -> case toList errorSet of
            [(ErrorCustom e)] -> e `shouldBe` customException
            _ -> expectationFailure "expecting single ErrorCustom"
          _ -> expectationFailure "expecting single ErrorCustom"

unit_letType :: Expectation
unit_letType = do
  P.parseNoEnv P.letType "" "type kek = int" `shouldSatisfy` isRight
  P.parseNoEnv P.letType "" "type Parameter = int" `shouldSatisfy` isLeft
  P.parseNoEnv P.letType "" "type Storage = int" `shouldSatisfy` isLeft
