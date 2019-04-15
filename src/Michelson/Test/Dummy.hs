-- | Dummy data to be used in tests where it's not essential.

module Michelson.Test.Dummy
  ( dummyNow
  , dummyMaxSteps
  , dummyContractEnv
  , dummyOrigination
  ) where

import Michelson.Interpret (ContractEnv(..), RemainingSteps)
import Michelson.Runtime.GState (genesisAddress, genesisKeyHash)
import Michelson.Untyped
import Tezos.Core (Timestamp(..), unsafeMkMutez)

-- | Dummy timestamp, can be used to specify current `NOW` value or
-- maybe something else.
dummyNow :: Timestamp
dummyNow = Timestamp 100

-- | Dummy value for maximal number of steps a contract can
-- make. Intentionally quite large, because most likely if you use
-- dummy value you don't want the interpreter to stop due to gas
-- exhaustion. On the other hand, it probably still prevents the
-- interpreter from working for eternity.
dummyMaxSteps :: RemainingSteps
dummyMaxSteps = 100500

-- | Dummy 'ContractEnv' with some reasonable hardcoded values. You
-- can override values you are interested in using record update
-- syntax.
dummyContractEnv :: ContractEnv
dummyContractEnv = ContractEnv
  { ceNow = dummyNow
  , ceMaxSteps = dummyMaxSteps
  , ceBalance = unsafeMkMutez 100
  , ceContracts = mempty
  , ceSelf = genesisAddress
  , ceSource = genesisAddress
  , ceSender = genesisAddress
  , ceAmount = unsafeMkMutez 100
  }

-- | 'OriginationOperation' with most data hardcoded to some
-- reasonable values. Contract and initial values must be passed
-- explicitly, because otherwise it hardly makes sense.
dummyOrigination ::
     Value
  -> Contract
  -> OriginationOperation
dummyOrigination storage contract = OriginationOperation
  { ooManager = genesisKeyHash
  , ooDelegate = Nothing
  , ooSpendable = False
  , ooDelegatable = False
  , ooBalance = unsafeMkMutez 100
  , ooStorage = storage
  , ooContract = contract
  }
