{-# OPTIONS_GHC -Wno-orphans #-}

-- | Module, containing some utilities for testing Michelson contracts using
-- Haskell testing frameworks (hspec and QuickCheck in particular).
module Morley.Test
  ( ContractReturn
  , ContractPropValidator
  , contractProp
  , specWithContract
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
import Data.Typeable ((:~:)(..), eqT)
import Test.Hspec (Spec, describe, expectationFailure, it, runIO)
import Test.QuickCheck (Arbitrary(..), choose)
import Text.Megaparsec (parse)

import Michelson.Interpret (ContractEnv, ContractReturn, interpret)
import Michelson.TypeCheck (SomeContract(..), TCError)
import Michelson.Typed (CT(..), CVal(..), Contract, Instr, T(..), Val(..))
import qualified Michelson.Untyped as U
import Morley.Macro (expandFlattenContract)
import Morley.Nop (typeCheckMorleyContract)
import qualified Morley.Parser as Mo
import Morley.Types (NopInstr)
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
  ContractEnv NopInstr
      -> Val Instr cp
      -> Val Instr st
      -> ContractReturn st
      -> prop

-- | Contract's property tester against given input.
-- Takes contract environment, initial storage and parameter,
-- interprets contract on this input and invokes validation function.
contractProp
  :: Contract cp st
  -> ContractPropValidator cp st prop
  -> ContractEnv NopInstr
  -> Val Instr cp
  -> Val Instr st
  -> prop
contractProp instr check env param initSt =
  check env param initSt $ interpret instr param initSt env

-- | Import contract and use it in the spec.
--
-- If contract's import failed, a spec with single failing expectation
-- will be generated (so tests will run unexceptionally, but a failing
-- result will notify about problem).
specWithContract
  :: (Typeable cp, Typeable st)
  => FilePath -> (Contract cp st -> Spec) -> Spec
specWithContract file execSpec =
  either errorSpec (describe ("Test contract " <> file) . execSpec)
    =<< runIO
          ( (Right <$> importContract file)
            `catch` (\(e :: ImportContractError) -> pure $ Left $ show e)
            `catch` \(e :: IOException) -> pure $ Left $ show e )
  where
    errorSpec = it ("Type check contract " <> file) . expectationFailure

-- | Import contract from a given file path.
--
-- This function reads file, parses and type checks contract.
--
-- This function may throw 'IOException' and 'ImportContractError'.
importContract
  :: forall cp st .
    (Typeable cp, Typeable st)
  => FilePath -> IO (Contract cp st)
importContract file = do
  contract <- assertEither (ICEParse . show) $
                  parse Mo.program file <$> readFile file
  SomeContract (instr :: Contract cp' st') _ _
    <- assertEither ICETypeCheck $ pure $ typeCheckMorleyContract $
        U.unOp <$> expandFlattenContract contract
  case (eqT @cp @cp', eqT @st @st') of
    (Just Refl, Just Refl) -> pure instr
    (Nothing, _) -> throwM ICEUnexpectedParamType
    _ -> throwM ICEUnexpectedStorageType
  where
    assertEither err action = either (throwM . err) pure =<< action

-- | Error type for 'importContract' function.
data ImportContractError
  = ICEUnexpectedParamType
  | ICEUnexpectedStorageType
  | ICEParse Text
  | ICETypeCheck (TCError NopInstr)
  deriving Show

instance Exception ImportContractError

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
