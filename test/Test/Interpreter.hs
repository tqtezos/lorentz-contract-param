module Test.Interpreter
  ( spec
  ) where

import Data.Vinyl (Rec(..))
import Data.Typeable ((:~:)(..), eqT)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, shouldBe, it)
import Text.Megaparsec (parse)
import Test.HUnit (assertFailure)

import Michelson.Types (Contract(..), Op (..))
import Morley.Macro (expandFlat)
import Morley.Parser (contract)
import qualified Michelson.Advanced as A
import Michelson.Advanced (CT(..), T(..))

spec :: Spec
spec = describe "Advanced type interpreter tests" $ do
    it "Interpreter correctly executes contract \"contracts/basic1.tz\"" $
      testRun "contracts/basic1.tz" trInitSt A.VUnit $ \(res, ops) -> do
        trToList res `shouldBe` [123, 100]
        case ops of
          [] -> pass
          _ -> expectationFailure "Non-empty operation list returned"
  where
    trInitSt = A.VList ((A.VC . A.CvInt) <$> [100, 10, 1])

    trToList :: A.Val (A.Instr 'T_unit) ('T_list ('T_c 'T_int)) -> [Integer]
    trToList (A.VList l) = map (\(A.VC (A.CvInt i)) -> i) l

testRun
  :: forall cp st .
    (Typeable cp, Typeable st)
  => FilePath
  -> A.Val (A.Instr cp) st
  -> A.Val (A.Instr cp) cp
  -> ((A.Val (A.Instr cp) st, [A.Operation (A.Instr cp)]) -> Expectation)
  -> Expectation
testRun file initSt initParam checkRes = do
  c' <- assertEither "Parse error" $
                  parse contract file <$> readFile file
  A.SomeContract (instr :: A.Instr cp' (A.ContractInp cp' st')
                                                  (A.ContractOut st')) _ _
    <- assertEither "Type check error" $ pure $ A.typeCheckContract $
        unOp <$> Contract (para c') (stor c') (expandFlat $ code c')
  case (eqT @cp @cp', eqT @st @st') of
    (Just Refl, Just Refl) -> do
      let initValue = A.VPair (initParam, initSt) :& RNil
      case A.doInstr (A.run instr initValue) A.ContractEnv of
        Left m -> expectationFailure $ "run error: " <> show m
        Right r -> checkRes $ contractRes r
    (Nothing, _) -> expectationFailure "Unexpected param type"
    _ -> expectationFailure "Unexpected storage type"
  where
    assertEither text action =
      either (\e -> assertFailure $ text <> ": " <> show e) pure =<< action

    contractRes
      :: Rec (A.Val (A.Instr cp)) (A.ContractOut st)
      -> (A.Val (A.Instr cp) st, [A.Operation (A.Instr cp)])
    contractRes (A.VPair (A.VList ops, r) :& RNil) =
      (r, (\(A.VOp op) -> op) <$> ops)

