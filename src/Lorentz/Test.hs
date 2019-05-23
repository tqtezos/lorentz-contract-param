module Lorentz.Test
  ( -- * Importing a contract
    specWithContract
  , specWithTypedContract
  , specWithUntypedContract

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
  , lOriginate
  , lOriginateEmpty
  , lTransfer
  , lCall
  , validate
  , setMaxSteps
  , setNow

  -- ** Validators
  , composeValidators
  , composeValidatorsList
  , expectAnySuccess
  , lExpectStorageUpdate
  , lExpectMichelsonFailed
  , lExpectFailWith

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
import Michelson.Test.Unit
import Michelson.Test.Util

import Lorentz.Test.Integrational as Exports
