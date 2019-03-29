-- | Functions to import contracts to be used in tests.

module Morley.Test.Import
  ( specWithContract
  , specWithTypedContract
  , importContract
  , ImportContractError (..)
  ) where

import Control.Exception (IOException, mapException)
import Data.Typeable ((:~:)(..), TypeRep, eqT, typeRep)
import Fmt (Buildable(build), pretty, (+|), (|+), (||+))
import Test.Hspec (Spec, describe, expectationFailure, it, runIO)

import Michelson.TypeCheck (SomeContract(..), TCError)
import Michelson.Typed (Contract)
import qualified Michelson.Untyped as U
import Morley.Ext (typeCheckMorleyContract)
import Morley.Runtime (prepareContract)
import Morley.Types (ParserException(..))

-- | Import contract and use it in the spec. Both versions of contract are
-- passed to the callback function (untyped and typed).
--
-- If contract's import failed, a spec with single failing expectation
-- will be generated (so tests will run unexceptionally, but a failing
-- result will notify about problem).
specWithContract
  :: (Typeable cp, Typeable st)
  => FilePath -> ((U.UntypedContract, Contract cp st) -> Spec) -> Spec
specWithContract file execSpec =
  either errorSpec (describe ("Test contract " <> file) . execSpec)
    =<< runIO
          ( (Right <$> importContract file)
            `catch` (\(e :: ImportContractError) -> pure $ Left $ displayException e)
            `catch` \(e :: IOException) -> pure $ Left $ displayException e )
  where
    errorSpec = it ("Type check contract " <> file) . expectationFailure

-- | A version of 'specWithContract' which passes only the typed
-- representation of the contract.
specWithTypedContract
  :: (Typeable cp, Typeable st)
  => FilePath -> (Contract cp st -> Spec) -> Spec
specWithTypedContract file execSpec = specWithContract file (execSpec . snd)

-- | Import contract from a given file path.
--
-- This function reads file, parses and type checks contract.
--
-- This function may throw 'IOException' and 'ImportContractError'.
importContract
  :: forall cp st .
    (Typeable cp, Typeable st)
  => FilePath -> IO (U.UntypedContract, Contract cp st)
importContract file = do
  contract <- mapException ICEParse $ prepareContract (Just file)
  SomeContract (instr :: Contract cp' st') _ _
    <- assertEither ICETypeCheck $ pure $ typeCheckMorleyContract contract
  case (eqT @cp @cp', eqT @st @st') of
    (Just Refl, Just Refl) -> pure (contract, instr)
    (Nothing, _) -> throwM $
      ICEUnexpectedParamType (U.para contract) (typeRep (Proxy @cp))
    _ -> throwM (ICEUnexpectedStorageType (U.stor contract) (typeRep (Proxy @st)))
  where
    assertEither err action = either (throwM . err) pure =<< action

-- | Error type for 'importContract' function.
data ImportContractError
  = ICEUnexpectedParamType !U.Type !TypeRep
  | ICEUnexpectedStorageType !U.Type !TypeRep
  | ICEParse !ParserException
  | ICETypeCheck !TCError
  deriving Show

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
