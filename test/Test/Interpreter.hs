module Test.Interpreter
  ( spec
  ) where

import qualified Data.Map as M
import Data.Typeable ((:~:)(..), eqT)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)
import Test.HUnit (assertFailure)
import Text.Megaparsec (parse)

import Michelson.Interpret
import Michelson.TypeCheck (ContractInp, ContractOut, SomeContract(..), typeCheckContract)
import Michelson.Typed
  (CT(..), CVal(..), Instr(..), T(..), Val(..))
import qualified Michelson.Typed as Typed
import qualified Michelson.Typed.Value as V
import Michelson.Untyped (Contract(..), Value(..), unOp)
import Morley.Macro (expandFlattenContract)
import Morley.Nop (nopHandler)
import Morley.Parser (contract)
import Tezos.Core (Mutez(..), Timestamp(..))
import Tezos.Crypto (Address(..))

spec :: Spec
spec = describe "Advanced type interpreter tests" $ do
    it "Interpreter correctly executes contract \"contracts/basic1.tz\"" $
      testRun "contracts/basic1.tz"
    it "Yet another interpreter test with ContractEnv" interpreterTest
testRun :: FilePath -> Expectation
testRun file = do
  c' <- assertEither "Parse error" $
                  parse contract file <$> readFile file
  let c = expandFlattenContract $ Contract (para c') (stor c') (code c')
  case typeCheckContract nopHandler (fmap unOp c) of
    Right (SomeContract (instr :: Instr cp (ContractInp cp st)
                                                     (ContractOut st)) _ _) -> do
      case interpret @cp @st nopHandler stubContractEnv instr of
        Left m -> expectationFailure $ "run error: " <> show m
        Right (res :: Val (Instr cp) ('T_pair ('T_list 'T_operation) st)) ->
          case (eqT @cp @'T_unit, eqT @st @('T_list ('T_c 'T_int))) of
            (Just Refl, Just Refl) -> do
              let (r, s) = contractRes res
              trToList r `shouldBe` [123, 100]
              length s `shouldBe` 0
            (Nothing, _) -> expectationFailure "Unexpected param type"
            _ -> expectationFailure "Unexpected storage type"
    Left _ -> expectationFailure "Illtyped contract"
  where
    assertEither text action =
      either (\e -> assertFailure $ text <> ": " <> show e) pure =<< action
    trToList :: Val (Instr 'T_unit) ('T_list ('T_c 'T_int)) -> [Integer]
    trToList (VList l) = map (\(VC (CvInt i)) -> i) l
    contractRes
      :: (V.Val (Instr cp) ('T_pair ('T_list 'T_operation) st))
      -> (V.Val (Instr cp) st, [V.Operation (Instr cp)])
    contractRes (V.VPair ((V.VList ops), r)) = (r, (\(V.VOp op) -> op) <$> ops)
    stubContractEnv :: ContractEnv nop
    stubContractEnv = ContractEnv
      { ceNow = Timestamp 0
      , ceMaxSteps = fromInteger 0
      , ceBalance = Mutez 0
      , ceStorage = ValueSeq [ValueInt 100, ValueInt 10, ValueInt 1]
      , ceContracts = M.empty
      , ceParameter = ValueUnit
      , ceSource = Address ""
      , ceSender = Address ""
      , ceAmount = Mutez 0
      }

interpreterTest :: Expectation
interpreterTest = do
  case parse contract "" add1tz of
    Left e -> error $ show e
    Right cont ->
      case typeCheckContract nopHandler ((fmap unOp . expandFlattenContract) cont) of
        Right (SomeContract (instr :: Instr cp (ContractInp cp st)
                                                         (ContractOut st)) _ _) -> do
          interpret @cp @st nopHandler add1tzStubEnv instr
            `shouldSatisfy` isRight
        Left _ -> expectationFailure "Illtyped contract"
  case parse contract "" someContract of
    Left e -> error $ show e
    Right cont ->
      case typeCheckContract nopHandler ((fmap unOp . expandFlattenContract) cont) of
        Right (SomeContract (instr :: Instr cp (ContractInp cp st)
                                                         (ContractOut st)) _ _) -> do
          interpret @cp @st nopHandler someContractStubEnv instr
            `shouldSatisfy` isRight
        Left _ -> expectationFailure "Illtyped contract"
  case parse contract "" someContract of
    Left e -> error $ show e
    Right cont ->
      case typeCheckContract nopHandler ((fmap unOp . expandFlattenContract) cont) of
        Right (SomeContract (instr :: Instr cp (ContractInp cp st)
                                                         (ContractOut st)) _ _) -> do
          interpret @cp @st nopHandler add1tzStubEnv instr
            `shouldSatisfy` isLeft
        Left _ -> expectationFailure "Illtyped contract"
  where
    add1tzStubEnv :: ContractEnv nop
    add1tzStubEnv = ContractEnv
      { ceNow = Timestamp 0
      , ceMaxSteps = fromInteger 0
      , ceBalance = Mutez 0
      , ceStorage = ValueInt 1
      , ceContracts = M.empty
      , ceParameter = ValueInt 1
      , ceSource = Address ""
      , ceSender = Address ""
      , ceAmount = Mutez 0
      }

    add1tz :: Text
    add1tz =
      "parameter int;"
      <> "storage int;"
      <> "code {CAR;"
      <> "PUSH int 1;"
      <> "ADD;"
      <> "NIL operation;"
      <> "PAIR}"

    someContractStubEnv :: ContractEnv nop
    someContractStubEnv = ContractEnv
      { ceNow = Timestamp 0
      , ceMaxSteps = fromInteger 0
      , ceBalance = Mutez 0
      , ceStorage = ValueSeq [ValueInt 1]
      , ceContracts = M.empty
      , ceParameter = ValueUnit
      , ceSource = Address ""
      , ceSender = Address ""
      , ceAmount = Mutez 0
      }
    someContract :: Text
    someContract = "parameter unit; storage (list (int :t)); "
      <> "code { CDR; PUSH int 0; SWAP; ITER { ADD; }; NIL operation;"
      <> "PUSH int 12; DIP { SWAP; }; ADD; PUSH (int :t) 100;"
      <> "NIL int; SWAP; CONS; SWAP; CONS; SWAP; PAIR; };"

--------------------
-- Examples
--------------------

-- | @myInstr@ is an equivalent to Michelson code:
--
--    PUSH int 223;
--    SOME;
--    IF_NONE { DUP; } { SWAP; };
--    ADD;
--    PUSH nat 12
--    ADD;
_myInstr :: Typed.Instr cp ('T_c 'T_int : s) ('T_c 'T_int : s)
_myInstr =
  Typed.PUSH (Typed.VC $ Typed.CvInt 223) Typed.#
  Typed.SOME Typed.#
  Typed.IF_NONE Typed.DUP Typed.SWAP Typed.#
  Typed.ADD Typed.#
  Typed.PUSH (Typed.VC $ Typed.CvNat 12) Typed.#
  Typed.ADD

-- | @myInstr2@ can not be represented in Michelson
-- syntax as Michelson has no way to directly push value
-- of type "option int"
_myInstr2 :: Typed.Instr cp a ('T_option ('T_c 'T_int) : a)
_myInstr2 =
  Typed.PUSH (Typed.VOption $ Just $ Typed.VC $ Typed.CvInt 223) Typed.#
  Typed.Nop
