module Test.Interpreter
  ( spec
  ) where

import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, label, (.&&.), (===))

import Michelson.Interpret (ContractEnv(..), ContractReturn, MichelsonFailed(..), RemainingSteps)
import Michelson.Typed (CT(..), CVal(..), Instr(..), T(..), Val(..), ( # ))
import Morley.Ext (interpretMorley)
import Morley.Test (ContractPropValidator, contractProp, specWithTypedContract)
import Morley.Types (MorleyLogs)
import Test.Interpreter.Auction (auctionSpec)
import Test.Interpreter.CallSelf (selfCallerSpec)
import Test.Interpreter.Compare (compareSpec)
import Test.Interpreter.Conditionals (conditionalsSpec)
import Test.Interpreter.StringCaller (stringCallerSpec)
import Test.Util.Interpreter (dummyContractEnv)

spec :: Spec
spec = describe "Advanced type interpreter tests" $ do
  specWithTypedContract "contracts/basic5.tz" $ \contract ->
    it "Basic test" $
      interpretMorley contract VUnit (VList [ VC $ CvInt 1 ]) dummyContractEnv
        `shouldSatisfy` (isRight . fst)

  specWithTypedContract "contracts/increment.tz" $ \contract ->
    it "Basic test" $
      interpretMorley contract VUnit (VC $ CvInt 23) dummyContractEnv
        `shouldSatisfy` (isRight . fst)

  specWithTypedContract "contracts/fail.tz" $ \contract ->
    it "Fail test" $
      interpretMorley contract VUnit VUnit dummyContractEnv
        `shouldSatisfy` (isLeft . fst)

  specWithTypedContract "contracts/mutez_add_overflow.tz" $ \contract ->
    it "Mutez add overflow test" $
      interpretMorley contract VUnit VUnit dummyContractEnv
        `shouldSatisfy` (isLeft . fst)

  specWithTypedContract "contracts/mutez_sub_underflow.tz" $ \contract ->
    it "Mutez sub underflow test" $
      interpretMorley contract VUnit VUnit dummyContractEnv
        `shouldSatisfy` (isLeft . fst)

  specWithTypedContract "contracts/basic1.tz" $ \contract -> do
    prop "Random check" $
      contractProp contract validateBasic1 dummyContractEnv

  auctionSpec
  compareSpec
  conditionalsSpec
  stringCallerSpec
  selfCallerSpec

  specWithTypedContract "contracts/steps_to_quota_test1.tz" $ \contract -> do
    it "Amount of steps should reduce" $ do
      validateStepsToQuotaTest
        (interpretMorley contract VUnit (VC (CvNat 0)) dummyContractEnv) 4

  specWithTypedContract "contracts/steps_to_quota_test2.tz" $ \contract -> do
    it "Amount of steps should reduce" $ do
      validateStepsToQuotaTest
        (interpretMorley contract VUnit (VC (CvNat 0)) dummyContractEnv) 8

  specWithTypedContract "contracts/gas_exhaustion.tz" $ \contract -> do
    it "Contract should fail due to gas exhaustion" $ do
      case fst $ interpretMorley contract (VC (CvString "x")) (VC (CvString "x")) dummyContractEnv of
        Right _ -> expectationFailure "expecting contract to fail"
        Left MichelsonGasExhaustion -> pass
        Left _ -> expectationFailure "expecting another failure reason"

validateBasic1
  :: ContractPropValidator 'T_unit ('T_list ('T_c 'T_int)) Property
validateBasic1 _env _param input (Right (ops, res), _) =
    (trToList res === [calcSum input + 12, 100])
    .&&.
    (label "returned no ops" $ null ops)
  where
    calcSum :: Val instr ('T_list ('T_c 'T_int)) -> Integer
    calcSum (VList l) = sum $ map (\(VC (CvInt i)) -> i) l

    trToList :: Val instr ('T_list ('T_c 'T_int)) -> [Integer]
    trToList (VList l) = map (\(VC (CvInt i)) -> i) l

validateBasic1 _ _ _ (Left e, _) = error $ show e

validateStepsToQuotaTest ::
     ContractReturn MorleyLogs ('T_c 'T_nat) -> RemainingSteps -> Expectation
validateStepsToQuotaTest res numOfSteps =
  case fst res of
    Right ([], VC (CvNat x)) ->
      (fromInteger . toInteger) x `shouldBe` ceMaxSteps dummyContractEnv - numOfSteps
    _ -> expectationFailure "unexpected contract result"

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
_myInstr :: Typeable s => Instr ('T_c 'T_int : s) ('T_c 'T_int : s)
_myInstr =
  PUSH (VC $ CvInt 223) #
  SOME #
  IF_NONE DUP SWAP #
  ADD #
  PUSH (VC $ CvNat 12) #
  ADD

_myInstr2 :: Typeable a => Instr a ('T_option ('T_c 'T_int) : a)
_myInstr2 =
  PUSH (VOption $ Just $ VC $ CvInt 223) #
  Nop
