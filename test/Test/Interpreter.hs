module Test.Interpreter
  ( spec
  ) where

import Data.Singletons (SingI)
import Fmt (pretty)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, label, (.&&.), (===))

import Michelson.Interpret
  (ContractEnv(..), ContractReturn, MichelsonFailed(..), RemainingSteps, interpret)
import Michelson.Test (ContractPropValidator, contractProp, specWithTypedContract)
import Michelson.Test.Dummy (dummyContractEnv)
import Michelson.Test.Util (failedProp)
import Michelson.Typed (CT(..), CValue(..), Instr(..), T(..), ToT, fromVal, toVal, ( # ))
import qualified Michelson.Typed as T
import Morley.Types (ExtInstr(..), MorleyLogs, PrintComment(..), mkStackRef)
import Test.Interpreter.A1.Feather (featherSpec)
import Test.Interpreter.Auction (auctionSpec)
import Test.Interpreter.CallSelf (selfCallerSpec)
import Test.Interpreter.Compare (compareSpec)
import Test.Interpreter.Conditionals (conditionalsSpec)
import Test.Interpreter.ContractOp (contractOpSpec)
import Test.Interpreter.EnvironmentSpec (environmentSpec)
import Test.Interpreter.StringCaller (stringCallerSpec)

spec :: Spec
spec = describe "Advanced type interpreter tests" $ do
  let contractResShouldBe (res, _) expected =
        case res of
          Left err -> expectationFailure $ "Unexpected failure: " <> pretty err
          Right (_ops, v) -> v `shouldBe` expected

  specWithTypedContract "contracts/basic5.tz" $ \contract ->
    it "Basic test" $
      interpret contract T.VUnit (toVal [1 :: Integer]) dummyContractEnv
        `contractResShouldBe` (toVal [13 :: Integer, 100])

  specWithTypedContract "contracts/increment.tz" $ \contract ->
    it "Basic test" $
      interpret contract T.VUnit (toVal @Integer 23) dummyContractEnv
        `contractResShouldBe` (toVal @Integer 24)

  specWithTypedContract "contracts/fail.tz" $ \contract ->
    it "Fail test" $
      interpret contract T.VUnit T.VUnit dummyContractEnv
        `shouldSatisfy` (isLeft . fst)

  specWithTypedContract "contracts/mutez_add_overflow.tz" $ \contract ->
    it "Mutez add overflow test" $
      interpret contract T.VUnit T.VUnit dummyContractEnv
        `shouldSatisfy` (isLeft . fst)

  specWithTypedContract "contracts/mutez_sub_underflow.tz" $ \contract ->
    it "Mutez sub underflow test" $
      interpret contract T.VUnit T.VUnit dummyContractEnv
        `shouldSatisfy` (isLeft . fst)

  specWithTypedContract "contracts/basic1.tz" $ \contract -> do
    prop "Random check" $ \input ->
      contractProp @_ @[Integer] contract (validateBasic1 input)
      dummyContractEnv () input

  specWithTypedContract "contracts/lsl.tz" $ \contract -> do
    it "LSL shouldn't overflow test" $
      interpret contract (toVal @Natural 5) (toVal @Natural 2) dummyContractEnv
        `contractResShouldBe` (toVal @Natural 20)
    it "LSL should overflow test" $
      interpret contract (toVal @Natural 5) (toVal @Natural 257) dummyContractEnv
        `shouldSatisfy` (isLeft . fst)

  specWithTypedContract "contracts/lsr.tz" $ \contract -> do
    it "LSR shouldn't underflow test" $
      interpret contract (toVal @Natural 30) (toVal @Natural 3) dummyContractEnv
        `contractResShouldBe` (toVal @Natural 3)
    it "LSR should underflow test" $
      interpret contract (toVal @Natural 1000) (toVal @Natural 257) dummyContractEnv
        `shouldSatisfy` (isLeft . fst)

  describe "FAILWITH" $ do
    specWithTypedContract "contracts/failwith_message.tz" $ \contract ->
      it "Failwith message test" $ do
        let msg = "An error occurred." :: Text
        contractProp contract (validateMichelsonFailsWith msg) dummyContractEnv msg ()

    specWithTypedContract "contracts/failwith_message2.tz" $ \contract -> do
        it "Conditional failwith message test" $ do
          let msg = "An error occurred." :: Text
          contractProp contract (validateMichelsonFailsWith msg) dummyContractEnv (True, msg) ()

        it "Conditional success test" $ do
          let param = (False, "Err" :: Text)
          contractProp contract validateSuccess dummyContractEnv param ()

  auctionSpec
  compareSpec
  conditionalsSpec
  stringCallerSpec
  selfCallerSpec
  environmentSpec
  contractOpSpec
  featherSpec

  specWithTypedContract "contracts/steps_to_quota_test1.tz" $ \contract -> do
    it "Amount of steps should reduce" $ do
      validateStepsToQuotaTest
        (interpret contract T.VUnit (T.VC (CvNat 0)) dummyContractEnv) 4

  specWithTypedContract "contracts/steps_to_quota_test2.tz" $ \contract -> do
    it "Amount of steps should reduce" $ do
      validateStepsToQuotaTest
        (interpret contract T.VUnit (T.VC (CvNat 0)) dummyContractEnv) 8

  specWithTypedContract "contracts/gas_exhaustion.tz" $ \contract -> do
    it "Contract should fail due to gas exhaustion" $ do
      let dummyStr = toVal @Text "x"
      case fst $ interpret contract dummyStr dummyStr dummyContractEnv of
        Right _ -> expectationFailure "expecting contract to fail"
        Left MichelsonGasExhaustion -> pass
        Left _ -> expectationFailure "expecting another failure reason"

  specWithTypedContract "contracts/add1_list.tz" $ \contract -> do
    let
      validate ::
        [Integer] -> ContractPropValidator (ToT [Integer]) Property
      validate param (res, _) =
        case res of
          Left failed -> failedProp $
            "add1_list unexpectedly failed: " <> pretty failed
          Right (fromVal . snd -> finalStorage) ->
            map succ param === finalStorage
    prop "Random check" $ \param ->
      contractProp contract (validate param) dummyContractEnv param param

  it "mkStackRef does not segfault" $ do
    let printI = Ext $ PRINT (PrintComment $ [Right $ mkStackRef @1])
    let contract = DROP # PUSH (toVal ()) # DUP # printI # DROP # NIL # PAIR
    contractProp contract (isRight . fst) dummyContractEnv () ()

validateSuccess :: ContractPropValidator t Expectation
validateSuccess (res, _) = res `shouldSatisfy` isRight

validateBasic1
  :: [Integer] -> ContractPropValidator ('TList ('Tc 'CInt)) Property
validateBasic1 input (Right (ops, res), _) =
    (fromVal res === [sum input + 12, 100])
    .&&.
    (label "returned no ops" $ null ops)
validateBasic1 _ (Left e, _) = failedProp $ show e

validateStepsToQuotaTest ::
     ContractReturn MorleyLogs ('Tc 'CNat) -> RemainingSteps -> Expectation
validateStepsToQuotaTest res numOfSteps =
  case fst res of
    Right ([], T.VC (CvNat x)) ->
      (fromInteger . toInteger) x `shouldBe` ceMaxSteps dummyContractEnv - numOfSteps
    _ -> expectationFailure "unexpected contract result"

validateMichelsonFailsWith
  :: (T.ToVal v, Typeable (ToT v), SingI (ToT v))
  => v -> ContractPropValidator st Expectation
validateMichelsonFailsWith v (res, _) = res `shouldBe` Left (MichelsonFailedWith $ toVal v)

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
_myInstr :: Typeable s => Instr ('Tc 'CInt : s) ('Tc 'CInt : s)
_myInstr =
  PUSH (T.VC $ CvInt 223) #
  SOME #
  IF_NONE DUP SWAP #
  ADD #
  PUSH (T.VC $ CvNat 12) #
  ADD

_myInstr2 :: Instr a ('TOption ('Tc 'CInt) : a)
_myInstr2 =
  PUSH (T.VOption $ Just $ T.VC $ CvInt 223) #
  Nop
