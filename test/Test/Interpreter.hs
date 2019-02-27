module Test.Interpreter
  ( spec
  ) where

import Data.Typeable ((:~:)(..), eqT)
import Data.Vinyl (Rec(..))
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe)
import Test.HUnit (assertFailure)
import Text.Megaparsec (parse)

import Michelson.Typed (CT(..), T(..), Instr (..))
import Michelson.Interpret
import Test.Morley.Runtime (dummyContractEnv)
import qualified Michelson.TypeCheck as T
import Michelson.Types (Contract(..), Op(..))
import qualified Michelson.Typed.Value as V
import Morley.Macro (expandFlat)
import Morley.Parser (contract)

spec :: Spec
spec = describe "Advanced type interpreter tests" $ do
    it "Interpreter correctly executes contract \"contracts/basic1.tz\"" $
      testRun "contracts/basic1.tz" trInitSt V.VUnit $ \(res, ops) -> do
        trToList res `shouldBe` [123, 100]
        case ops of
          [] -> pass
          _ -> expectationFailure "Non-empty operation list returned"
  where
    trInitSt = V.VList ((V.VC . V.CvInt) <$> [100, 10, 1])

    trToList :: V.Val (Instr 'T_unit) ('T_list ('T_c 'T_int)) -> [Integer]
    trToList (V.VList l) = map (\(V.VC (V.CvInt i)) -> i) l

testRun
  :: forall cp st .
    (Typeable cp, Typeable st)
  => FilePath
  -> V.Val (Instr cp) st
  -> V.Val (Instr cp) cp
  -> ((V.Val (Instr cp) st, [V.Operation (Instr cp)]) -> Expectation)
  -> Expectation
testRun file initSt initParam checkRes = do
  c' <- assertEither "Parse error" $
                  parse contract file <$> readFile file
  T.SomeContract (instr :: Instr cp' (T.ContractInp cp' st')
                                                  (T.ContractOut st')) _ _
    <- assertEither "Type check error" $ pure $ T.typeCheckContract $
        unOp <$> Contract (para c') (stor c') (expandFlat $ code c')
  case (eqT @cp @cp', eqT @st @st') of
    (Just Refl, Just Refl) -> do
      let initValue = V.VPair (initParam, initSt) :& RNil
      case doInstr (run instr initValue) dummyContractEnv of
        Left m -> expectationFailure $ "run error: " <> show m
        Right r -> checkRes $ contractRes r
    (Nothing, _) -> expectationFailure "Unexpected param type"
    _ -> expectationFailure "Unexpected storage type"
  where
    assertEither text action =
      either (\e -> assertFailure $ text <> ": " <> show e) pure =<< action

    contractRes
      :: Rec (V.Val (Instr cp)) (T.ContractOut st)
      -> (V.Val (Instr cp) st, [V.Operation (Instr cp)])
    contractRes (V.VPair (V.VList ops, r) :& RNil) =
      (r, (\(V.VOp op) -> op) <$> ops)

