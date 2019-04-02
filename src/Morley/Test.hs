-- | Module containing some utilities for testing Michelson contracts using
-- Haskell testing frameworks (hspec and QuickCheck in particular).
-- It's Morley testing EDSL.

module Morley.Test
  ( -- * Importing a contract
    specWithContract
  , specWithTypedContract
  , specWithUntypedContract

  -- * Unit testing
  , ContractReturn
  , ContractPropValidator
  , contractProp
  , contractPropVal

  -- * Integrational testing
  -- ** Testing engine
  , IntegrationalValidator
  , SuccessValidator
  , IntegrationalScenario
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

import Morley.Test.Dummy
import Morley.Test.Gen
import Morley.Test.Import
import Morley.Test.Integrational
import Morley.Test.Unit
import Morley.Test.Util
