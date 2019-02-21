module Test.Typecheck
  ( simpleSpec
  , advancedSpec
  ) where

import Data.Vinyl (Rec(..))
import Data.Typeable ((:~:)(..), eqT)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, shouldBe, it)
import Text.Megaparsec (parse)
import Test.HUnit (assertFailure)

import qualified Michelson.Typecheck as S
import Michelson.Types (Contract(..), Op (..))
import Morley.Macro (expandFlat)
import Morley.Parser (contract)
import qualified Advanced as A
import Advanced (CT(..), T(..))

import Test.Util.Contracts (getIllTypedContracts, getWellTypedContracts)

simpleSpec :: Spec
simpleSpec = describe "Simple type typechecker tests" $ specImpl doTC
  where
    doTC = either (Left . displayException) pure . S.typecheckContract

advancedSpec :: Spec
advancedSpec = describe "Advanced type typechecker tests" $ do
    specImpl doTC
    it "Interpreter correctly executes contract \"contracts/basic1.tz\"" $
      testRun "contracts/basic1.tz" trInitSt A.VUnit $ \(res, ops) -> do
        trToList res `shouldBe` [123, 100]
        case ops of
          [] -> pass
          _ -> expectationFailure "Non-empty operation list returned"
  where
    doTC = either (Left . displayException) (\_ -> pure ()) .
            A.typeCheckContract . fmap unOp
    trInitSt = A.VList ((A.VC . A.CvInt) <$> [100, 10, 1])

    trToList :: A.Val A.Operation 'T_unit ('T_list ('T_c 'T_int)) -> [Integer]
    trToList (A.VList l) = map (\(A.VC (A.CvInt i)) -> i) l

specImpl :: TypeCheckFun -> Spec
specImpl doTC = do
    it "Successfully typechecks contracts examples from contracts/" $
      goodContractsTest doTC
    it "Reports errors on contracts examples from contracts/ill-typed" $
      badContractsTest doTC

type TypeCheckFun = Contract Op -> Either String ()

goodContractsTest :: TypeCheckFun -> Expectation
goodContractsTest doTC = mapM_ (checkFile doTC True) =<< getWellTypedContracts

badContractsTest :: TypeCheckFun -> Expectation
badContractsTest doTC = mapM_ (checkFile doTC False) =<< getIllTypedContracts

testRun
  :: forall cp st .
    (Typeable cp, Typeable st)
  => FilePath
  -> A.Val A.Operation cp st
  -> A.Val A.Operation cp cp
  -> ((A.Val A.Operation cp st, [A.Operation]) -> Expectation)
  -> Expectation
testRun file initSt initParam checkRes = do
  c' <- assertEither "Parse error" $
                  parse contract file <$> readFile file
  A.SomeContract (instr :: A.Instr A.Operation cp' (A.ContractInp cp' st')
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
      :: Rec (A.Val A.Operation cp) (A.ContractOut st)
      -> (A.Val A.Operation cp st, [A.Operation])
    contractRes (A.VPair (A.VList ops, r) :& RNil) =
      (r, (\(A.VOp op) -> op) <$> ops)

checkFile :: TypeCheckFun -> Bool -> FilePath -> Expectation
checkFile doTypeCheck wellTyped file = do
  cd <- readFile file
  case parse contract file cd of
    Right c' -> do
      let c = Contract (para c') (stor c') (expandFlat $ code c')
      case doTypeCheck c of
        Left err
          | wellTyped ->
            expectationFailure $
            "Typechecker unexpectedly failed on " <>
            show file <> ": " <> err
          | otherwise ->
            pass
        Right _
          | not wellTyped ->
            expectationFailure $
            "Typechecker unexpectedly considered " <> show file <> "well-typed."
          | otherwise ->
            pass
    Left e -> expectationFailure $ "Parser error: " <> show e
