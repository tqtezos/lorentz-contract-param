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
  , ValidationError (..)
  , integrationalTestExpectation
  , integrationalTestProperty
  , lOriginate
  , lOriginateEmpty
  , lTransfer
  , lCall
  , validate
  , setMaxSteps
  , setNow
  , withSender

  -- ** Validators
  , composeValidators
  , composeValidatorsList
  , expectAnySuccess
  , lExpectStorageUpdate
  , lExpectBalance
  , lExpectStorageConst
  , lExpectMichelsonFailed
  , lExpectFailWith
  , lExpectUserError
  , lExpectConsumerStorage
  , lExpectViewConsumerStorage

  -- ** Various
  , TxData (..)
  , genesisAddresses
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
