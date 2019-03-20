{-# OPTIONS_GHC -Wno-orphans #-}

-- | Module, containing some utilities for testing Michelson contracts using
-- Haskell testing frameworks (hspec and QuickCheck in particular).
module Morley.Test
  ( ContractReturn
  , ContractPropValidator
  , contractProp
  , specWithContract
  , specWithTypedContract
  , importContract
  , ImportContractError (..)

  , minTimestamp
  , maxTimestamp
  , midTimestamp
  ) where

import Control.Exception (IOException)
import Data.Time.Calendar (Day, addDays, diffDays)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Typeable ((:~:)(..), TypeRep, eqT, typeRep)
import Fmt (Buildable(build), pretty, (+|), (|+), (||+))
import Test.Hspec (Spec, describe, expectationFailure, it, runIO)
import Test.QuickCheck (Arbitrary(..), choose)
import Text.Megaparsec (parse)

import Michelson.Interpret (ContractEnv, ContractReturn)
import Michelson.TypeCheck (SomeContract(..), TCError)
import Michelson.Typed (CT(..), CVal(..), Contract, Instr, T(..), Val(..))
import qualified Michelson.Untyped as U
import Morley.Aliases (UntypedContract)
import Morley.Ext (interpretMorley, typeCheckMorleyContract)
import Morley.Macro (expandFlattenContract)
import qualified Morley.Parser as Mo
import Morley.Types (MorleyLogs, ParserException(..))
import Tezos.Core
  (Mutez(..), Timestamp, timestampFromSeconds, timestampFromUTCTime, timestampToSeconds,
  unsafeMkMutez)

-- | Type for contract execution validation.
--
-- It's a function which is supplied with:
--
--  * contract's environment
--  * initial state
--  * contract's parameter
--  * contract execution output (failure or new storage with operation list)
--
-- Function returns a property which type is designated by type variable @prop@
-- and might be 'Test.QuickCheck.Property' or 'Test.Hspec.Expectation'
-- or anything else relevant.
type ContractPropValidator cp st prop =
     ContractEnv
  -> Val Instr cp
  -> Val Instr st
  -> ContractReturn MorleyLogs st
  -> prop

-- | Contract's property tester against given input.
-- Takes contract environment, initial storage and parameter,
-- interprets contract on this input and invokes validation function.
contractProp
  :: (Typeable cp, Typeable st)
  => Contract cp st
  -> ContractPropValidator cp st prop
  -> ContractEnv
  -> Val Instr cp
  -> Val Instr st
  -> prop
contractProp instr check env param initSt =
  check env param initSt $ interpretMorley instr param initSt env

-- | Import contract and use it in the spec. Both versions of contract are
-- passed to the callback function (untyped and typed).
--
-- If contract's import failed, a spec with single failing expectation
-- will be generated (so tests will run unexceptionally, but a failing
-- result will notify about problem).
specWithContract
  :: (Typeable cp, Typeable st)
  => FilePath -> ((UntypedContract, Contract cp st) -> Spec) -> Spec
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
  => FilePath -> IO (UntypedContract, Contract cp st)
importContract file = do
  contract <- assertEither (ICEParse . ParserException) $
                  parse Mo.program file <$> readFile file
  let expandedAndFlattened = expandFlattenContract contract
  SomeContract (instr :: Contract cp' st') _ _
    <- assertEither ICETypeCheck $ pure $ typeCheckMorleyContract $
        U.unOp <$> expandedAndFlattened
  case (eqT @cp @cp', eqT @st @st') of
    (Just Refl, Just Refl) -> pure (expandedAndFlattened, instr)
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

instance Arbitrary (CVal 'T_key_hash) where
  arbitrary = CvKeyHash <$> arbitrary
instance Arbitrary (CVal 'T_mutez) where
  arbitrary = CvMutez <$> arbitrary
instance Arbitrary (CVal 'T_int) where
  arbitrary = CvInt <$> arbitrary
instance Arbitrary (CVal a) => Arbitrary (Val instr ('T_c a)) where
  arbitrary = VC <$> arbitrary
instance Arbitrary (Val instr a) => Arbitrary (Val instr ('T_list a)) where
  arbitrary = VList <$> arbitrary
instance Arbitrary (Val instr 'T_unit) where
  arbitrary = pure VUnit
instance (Arbitrary (Val instr a), Arbitrary (Val instr b))
    => Arbitrary (Val instr ('T_pair a b)) where
  arbitrary = VPair ... (,) <$> arbitrary <*> arbitrary

minDay :: Day
minDay = fromMaybe (error "failed to parse day 2008-11-01") $
            parseTimeM True defaultTimeLocale "%Y-%-m-%-d" "2008-11-01"

maxDay :: Day
maxDay = fromMaybe (error "failed to parse day 2024-11-01") $
            parseTimeM True defaultTimeLocale "%Y-%-m-%-d" "2024-11-01"

minSec :: Integer
minSec = 0

maxSec :: Integer
maxSec = 86399

-- | Minimal (earliest) timestamp used for @Arbitrary (CVal 'T_timestamp)@
minTimestamp :: Timestamp
minTimestamp = timestampFromUTCTime $ UTCTime minDay (fromInteger minSec)

-- | Maximal (latest) timestamp used for @Arbitrary (CVal 'T_timestamp)@
maxTimestamp :: Timestamp
maxTimestamp = timestampFromUTCTime $ UTCTime maxDay (fromInteger maxSec)

-- | Median of 'minTimestamp' and 'maxTimestamp'.
-- Useful for testing (exactly half of generated dates will be before and after
-- this date).
midTimestamp :: Timestamp
midTimestamp = timestampFromUTCTime $
  UTCTime ( ((maxDay `diffDays` minDay) `div` 2) `addDays` minDay)
          (fromInteger $ (maxSec - minSec) `div` 2)

instance Arbitrary (CVal 'T_timestamp) where
  arbitrary =
    CvTimestamp . timestampFromSeconds @Int <$>
    choose (timestampToSeconds minTimestamp, timestampToSeconds maxTimestamp)

instance Arbitrary Mutez where
  arbitrary = unsafeMkMutez <$> choose (unMutez minBound, unMutez maxBound)
