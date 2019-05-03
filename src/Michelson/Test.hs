-- | Module containing some utilities for testing Michelson contracts using
-- Haskell testing frameworks (hspec and QuickCheck in particular).
-- It's Morley testing EDSL.

module Michelson.Test
  ( -- * Importing a contract
    specWithContract
  , specWithTypedContract
  , specWithUntypedContract
  , importUntypedContract

  -- * Unit testing
  , ContractReturn
  , ContractPropValidator
  , contractProp
  , contractPropVal
  , contractRepeatedProp
  , contractRepeatedPropVal

  -- * Integrational testing
  -- ** Testing engine
  , IntegrationalValidator
  , SuccessValidator
  , IntegrationalScenario
  , IntegrationalScenarioM
  , integrationalTestExpectation
  , integrationalTestProperty
  , originate
  , transfer
  , validate
  , setMaxSteps
  , setNow

  -- ** Validators
  , composeValidators
  , composeValidatorsList
  , expectAnySuccess
  , expectStorageUpdate
  , expectStorageUpdateConst
  , expectBalance
  , expectStorageConst
  , expectGasExhaustion
  , expectMichelsonFailed

  -- ** Various
  , TxData (..)
  , genesisAddress

  -- * General utilities
  , failedProp
  , succeededProp
  , qcIsLeft
  , qcIsRight

  -- * Dummy values
  , dummyContractEnv

  -- * Arbitrary data
  , minTimestamp
  , maxTimestamp
  , midTimestamp
  ) where

import Michelson.Test.Dummy
import Michelson.Test.Gen
import Michelson.Test.Import
import Michelson.Test.Integrational
import Michelson.Test.Unit
import Michelson.Test.Util
