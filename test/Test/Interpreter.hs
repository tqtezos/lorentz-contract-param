module Test.Interpreter
  ( spec
  ) where

import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, label, (.&&.), (===))

import Michelson.Interpret (interpret)
import Michelson.Typed (CT(..), CVal(..), Instr(..), T(..), Val(..), ( # ))
import Morley.Test (ContractPropValidator, contractProp, specWithContract)
import Test.Morley.Runtime (dummyContractEnv)

spec :: Spec
spec = describe "Advanced type interpreter tests" $ do
    specWithContract "contracts/basic5.tz" $ \contract ->
      it "Basic test" $
        interpret contract VUnit (VList [ VC $ CvInt 1 ]) dummyContractEnv
          `shouldSatisfy` isRight
    specWithContract "contracts/increment.tz" $ \contract ->
      it "Basic test" $
        interpret contract VUnit (VC $ CvInt 23) dummyContractEnv
          `shouldSatisfy` isRight
    specWithContract "contracts/basic1.tz" $ \contract -> do
      prop "Random check" $
        contractProp contract validateBasic1 dummyContractEnv

validateBasic1
  :: ContractPropValidator 'T_unit ('T_list ('T_c 'T_int)) Property
validateBasic1 _env _param input (Right (ops, res)) =
    (trToList res === [calcSum input + 12, 100])
    .&&.
    (label "returned no ops" $ null ops)
  where
    calcSum :: Val instr ('T_list ('T_c 'T_int)) -> Integer
    calcSum (VList l) = sum $ map (\(VC (CvInt i)) -> i) l

    trToList :: Val instr ('T_list ('T_c 'T_int)) -> [Integer]
    trToList (VList l) = map (\(VC (CvInt i)) -> i) l

validateBasic1 _ _ _ (Left e) = error $ show e

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
_myInstr :: Instr cp ('T_c 'T_int : s) ('T_c 'T_int : s)
_myInstr =
  PUSH (VC $ CvInt 223) #
  SOME #
  IF_NONE DUP SWAP #
  ADD #
  PUSH (VC $ CvNat 12) #
  ADD

_myInstr2 :: Instr cp a ('T_option ('T_c 'T_int) : a)
_myInstr2 =
  PUSH (VOption $ Just $ VC $ CvInt 223) #
  Nop
