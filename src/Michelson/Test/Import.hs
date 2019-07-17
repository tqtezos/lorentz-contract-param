-- | Functions to import contracts to be used in tests.

module Michelson.Test.Import
  (
    -- * Read, parse, typecheck
    readContract
  , importContract
  , importUntypedContract
  , ImportContractError (..)

    -- * Tasty helpers
  , testTreesWithContract
  , testTreesWithContractL
  , testTreesWithTypedContract
  , concatTestTrees

    -- * HSpec helpers
  , specWithContract
  , specWithContractL
  , specWithTypedContract
  , specWithUntypedContract
  ) where

import Control.Exception (IOException)
import Data.Singletons (SingI, demote)
import Data.Typeable ((:~:)(..), eqT)
import Fmt (Buildable(build), pretty, (+|), (|+))
import Test.Hspec (Spec, describe, expectationFailure, it, runIO)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertFailure, testCase)

import qualified Lorentz as L
import Michelson.Parser.Error (ParserException(..))
import Michelson.Runtime (parseExpandContract, prepareContract)
import Michelson.TypeCheck (SomeContract(..), TCError, typeCheckContract)
import Michelson.Typed (Contract, ToT, toUType)
import qualified Michelson.Untyped as U
import Util.IO

----------------------------------------------------------------------------
-- tasty helpers
----------------------------------------------------------------------------

-- | Import contract and use to create test trees. Both versions of contract are
-- passed to the callback function (untyped and typed).
--
-- If contract's import fails, a tree with single failing test will be generated
-- (so test tree will likely be generated unexceptionally, but a failing
-- result will notify about problem).
testTreesWithContract
  :: (Each [Typeable, SingI] [cp, st], HasCallStack)
  => FilePath -> ((U.Contract, Contract cp st) -> IO [TestTree]) -> IO [TestTree]
testTreesWithContract = testTreesWithContractImpl importContract

-- | Like 'testTreesWithContract' but for Lorentz types.
testTreesWithContractL
  :: (Each [Typeable, SingI] [ToT cp, ToT st], HasCallStack)
  => FilePath -> ((U.Contract, L.Contract cp st) -> IO [TestTree]) -> IO [TestTree]
testTreesWithContractL file testImpl = testTreesWithContract file (testImpl . second L.I)

-- | Like 'testTreesWithContract' but supplies only typed contract.
testTreesWithTypedContract
  :: (Each [Typeable, SingI] [cp, st], HasCallStack)
  => FilePath -> (Contract cp st -> IO [TestTree]) -> IO [TestTree]
testTreesWithTypedContract =
  testTreesWithContractImpl (fmap snd . importContract)

testTreesWithContractImpl
  :: HasCallStack
  => (FilePath -> IO contract)
  -> FilePath
  -> (contract -> IO [TestTree])
  -> IO [TestTree]
testTreesWithContractImpl doImport file testImpl =
  saferImport doImport file >>= \case
    Left err -> pure [testCase ("Import contract " <> file) $ assertFailure err]
    Right contract -> testImpl contract

-- A helper function which allows you to use multiple
-- 'testTreesWithTypedContract' in a single top-level test with type
-- 'IO [TestTree]'.
concatTestTrees :: [IO [TestTree]] -> IO [TestTree]
concatTestTrees = fmap concat . sequence

----------------------------------------------------------------------------
-- hspec helpers
----------------------------------------------------------------------------

-- | Import contract and use it in the spec. Both versions of contract are
-- passed to the callback function (untyped and typed).
--
-- If contract's import fails, a spec with single failing expectation
-- will be generated (so tests will likely run unexceptionally, but a failing
-- result will notify about problem).
specWithContract
  :: (Each [Typeable, SingI] [cp, st], HasCallStack)
  => FilePath -> ((U.Contract, Contract cp st) -> Spec) -> Spec
specWithContract = specWithContractImpl importContract

-- | Like 'specWithContract', but for Lorentz types.
specWithContractL
  :: (Each [Typeable, SingI] [ToT cp, ToT st], HasCallStack)
  => FilePath -> ((U.Contract, L.Contract cp st) -> Spec) -> Spec
specWithContractL file mkSpec = specWithContract file (mkSpec . second L.I)

-- | A version of 'specWithContract' which passes only the typed
-- representation of the contract.
specWithTypedContract
  :: (Each [Typeable, SingI] [cp, st], HasCallStack)
  => FilePath -> (Contract cp st -> Spec) -> Spec
specWithTypedContract = specWithContractImpl (fmap snd . importContract)

specWithUntypedContract :: FilePath -> (U.Contract -> Spec) -> Spec
specWithUntypedContract = specWithContractImpl importUntypedContract

specWithContractImpl
  :: HasCallStack
  => (FilePath -> IO contract) -> FilePath -> (contract -> Spec) -> Spec
specWithContractImpl doImport file execSpec =
  either errorSpec (describe ("Test contract " <> file) . execSpec)
    =<< runIO (saferImport doImport file)
  where
    errorSpec = it ("Import contract " <> file) . expectationFailure

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- Catch some errors during contract import, we don't want the whole
-- test suite to crash if something like that happens.
saferImport :: (FilePath -> IO contract) -> FilePath -> IO (Either String contract)
saferImport doImport file =
  ((Right <$> doImport file)
  `catch` \(e :: ImportContractError) -> pure $ Left $ displayException e)
  `catch` \(e :: IOException) -> pure $ Left $ displayException e

----------------------------------------------------------------------------
-- Reading, parsing, typechecking
----------------------------------------------------------------------------

readContract
  :: forall cp st .
    Each [Typeable, SingI] [cp, st]
  => FilePath
  -> Text
  -> Either ImportContractError (U.Contract, Contract cp st)
readContract filePath txt = do
  contract <- first ICEParse $ parseExpandContract (Just filePath) txt
  SomeContract (instr :: Contract cp' st') _ _
    <- first ICETypeCheck $ typeCheckContract mempty contract
  case (eqT @cp @cp', eqT @st @st') of
    (Just Refl, Just Refl) -> pure (contract, instr)
    (Nothing, _) -> Left $
      ICEUnexpectedParamType (U.para contract) (toUType $ demote @cp)
    _ -> Left (ICEUnexpectedStorageType (U.stor contract) (toUType $ demote @st))

-- | Import contract from a given file path.
--
-- This function reads file, parses and type checks a contract.
-- Within the typechecking we assume that no contracts are originated,
-- otherwise a type checking error will be caused.
--
-- This function may throw 'IOException' and 'ImportContractError'.
importContract
  :: forall cp st .
     Each [Typeable, SingI] [cp, st]
  => FilePath -> IO (U.Contract, Contract cp st)
importContract file = either throwM pure =<< readContract file <$> readFileUtf8 file

importUntypedContract :: FilePath -> IO U.Contract
importUntypedContract = prepareContract . Just

-- | Error type for 'importContract' function.
data ImportContractError
  = ICEUnexpectedParamType !U.Type !U.Type
  | ICEUnexpectedStorageType !U.Type !U.Type
  | ICEParse !ParserException
  | ICETypeCheck !TCError
  deriving (Show, Eq)

instance Buildable ImportContractError where
  build =
    \case
      ICEUnexpectedParamType actual expected ->
        "Unexpected parameter type: " +| actual |+
        ", expected: " +| expected |+ ""
      ICEUnexpectedStorageType actual expected ->
        "Unexpected storage type: " +| actual |+
        ", expected: " +| expected |+ ""
      ICEParse e -> "Failed to parse the contract: " +| e |+ ""
      ICETypeCheck e -> "The contract is ill-typed: " +| e |+ ""

instance Exception ImportContractError where
  displayException = pretty
