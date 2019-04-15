-- | Functions to import contracts to be used in tests.

module Michelson.Test.Import
  ( readContract
  , specWithContract
  , specWithTypedContract
  , specWithUntypedContract
  , importContract
  , importUntypedContract
  , ImportContractError (..)
  ) where

import Control.Exception (IOException)
import Data.Typeable ((:~:)(..), TypeRep, eqT, typeRep)
import Fmt (Buildable(build), pretty, (+|), (|+), (||+))
import Test.Hspec (Spec, describe, expectationFailure, it, runIO)

import Michelson.Runtime (parseExpandContract, prepareContract)
import Michelson.TypeCheck (SomeContract(..), TCError)
import Michelson.Typed (Contract)
import qualified Michelson.Untyped as U
import Morley.Ext (typeCheckMorleyContract)
import Morley.Types (ParserException(..))

-- | Import contract and use it in the spec. Both versions of contract are
-- passed to the callback function (untyped and typed).
--
-- If contract's import failed, a spec with single failing expectation
-- will be generated (so tests will run unexceptionally, but a failing
-- result will notify about problem).
specWithContract
  :: (Typeable cp, Typeable st)
  => FilePath -> ((U.Contract, Contract cp st) -> Spec) -> Spec
specWithContract = specWithContractImpl importContract

-- | A version of 'specWithContract' which passes only the typed
-- representation of the contract.
specWithTypedContract
  :: (Typeable cp, Typeable st, HasCallStack)
  => FilePath -> (Contract cp st -> Spec) -> Spec
specWithTypedContract = specWithContractImpl (fmap snd . importContract)

specWithUntypedContract :: FilePath -> (U.Contract -> Spec) -> Spec
specWithUntypedContract = specWithContractImpl importUntypedContract

specWithContractImpl
  :: HasCallStack
  => (FilePath -> IO contract) -> FilePath -> (contract -> Spec) -> Spec
specWithContractImpl doImport file execSpec =
  either errorSpec (describe ("Test contract " <> file) . execSpec)
    =<< runIO
          ( (Right <$> doImport file)
            `catch` (\(e :: ImportContractError) -> pure $ Left $ displayException e)
            `catch` \(e :: IOException) -> pure $ Left $ displayException e )
  where
    errorSpec = it ("Import contract " <> file) . expectationFailure

readContract
  :: forall cp st .
    (Typeable cp, Typeable st)
  => FilePath
  -> Text
  -> Either ImportContractError (U.Contract, Contract cp st)
readContract filePath txt = do
  contract <- first ICEParse $ parseExpandContract (Just filePath) txt
  SomeContract (instr :: Contract cp' st') _ _
    <- first ICETypeCheck $ typeCheckMorleyContract mempty contract
  case (eqT @cp @cp', eqT @st @st') of
    (Just Refl, Just Refl) -> pure (contract, instr)
    (Nothing, _) -> Left $
      ICEUnexpectedParamType (U.para contract) (typeRep (Proxy @cp))
    _ -> Left (ICEUnexpectedStorageType (U.stor contract) (typeRep (Proxy @st)))

-- | Import contract from a given file path.
--
-- This function reads file, parses and type checks a contract.
-- Within the typechecking we assume that no contracts are originated,
-- otherwise a type checking error will be caused.
--
-- This function may throw 'IOException' and 'ImportContractError'.
importContract
  :: forall cp st .
    (Typeable cp, Typeable st)
  => FilePath -> IO (U.Contract, Contract cp st)
importContract file = either throwM pure =<< readContract file <$> readFile file

importUntypedContract :: FilePath -> IO U.Contract
importUntypedContract = prepareContract . Just

-- | Error type for 'importContract' function.
data ImportContractError
  = ICEUnexpectedParamType !U.Type !TypeRep
  | ICEUnexpectedStorageType !U.Type !TypeRep
  | ICEParse !ParserException
  | ICETypeCheck !TCError
  deriving (Show, Eq)

instance Buildable ImportContractError where
  build =
    \case
      ICEUnexpectedParamType actual expected ->
        "Unexpected parameter type: " +| actual |+
        ", expected: " +| expected ||+ ""
      ICEUnexpectedStorageType actual expected ->
        "Unexpected storage type: " +| actual |+
        ", expected: " +| expected ||+ ""
      ICEParse e -> "Failed to parse the contract: " +| e |+ ""
      ICETypeCheck e -> "The contract is ill-typed: " +| e |+ ""

instance Exception ImportContractError where
  displayException = pretty
