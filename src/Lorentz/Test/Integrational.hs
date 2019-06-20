-- | Mirrors 'Michelson.Test.Integrational' module in a Lorentz way.
module Lorentz.Test.Integrational
  (
    -- * Re-exports
    TxData (..)
  , genesisAddresses
  , genesisAddress
  -- * More genesis addresses which can be used in tests
  , genesisAddress1
  , genesisAddress2
  , genesisAddress3
  , genesisAddress4
  , genesisAddress5
  , genesisAddress6

    -- * Testing engine
  , I.IntegrationalValidator
  , SuccessValidator
  , IntegrationalScenarioM
  , I.IntegrationalScenario
  , I.ValidationError (..)
  , I.integrationalTestExpectation
  , I.integrationalTestProperty
  , lOriginate
  , lOriginateEmpty
  , lTransfer
  , lCall
  , I.validate
  , I.setMaxSteps
  , I.setNow
  , I.withSender
  , I.branchout
  , (I.?-)

  -- * Validators
  , I.composeValidators
  , I.composeValidatorsList
  , I.expectAnySuccess
  , lExpectStorageUpdate
  , lExpectBalance
  , lExpectStorageConst
  , lExpectMichelsonFailed
  , lExpectFailWith
  , lExpectError
  , lExpectConsumerStorage
  , lExpectViewConsumerStorage
  ) where

import Data.Default (Default(..))
import Data.Singletons (SingI(..))
import Data.Typeable (gcast)
import Fmt (Buildable, listF, (+|), (|+))
import Named ((:!), arg)

import qualified Lorentz as L
import Michelson.Interpret (InterpretUntypedError(..), MichelsonFailed(..))
import Michelson.Runtime
import Michelson.Runtime.GState
import Michelson.Test.Integrational (IntegrationalScenarioM, SuccessValidator)
import qualified Michelson.Test.Integrational as I
import Michelson.TypeCheck (typeVerifyValue)
import qualified Michelson.Typed as T
import Tezos.Address
import Tezos.Core
import Util.Named ((.!))

----------------------------------------------------------------------------
-- Interface
----------------------------------------------------------------------------

-- TODO: how to call they normally? :thinking:
-- Preserving just the same names like @transfer@ or @originate@
-- looks very bad because no one will import this or
-- 'Michelson.Test.Integrational' module qualified
-- and thus finding which exact function is used would become too painful.

-- | Like 'originate', but for typed contract and value.
tOriginate
  :: (SingI cp, SingI st, T.HasNoOp st)
  => T.Contract cp st -> Text -> T.Value st -> Mutez -> IntegrationalScenarioM Address
tOriginate contract name value balance =
  I.originate (T.convertContract contract) name (T.untypeValue value) balance

-- | Like 'originate', but for Lorentz contracts.
lOriginate
  :: ( SingI (T.ToT cp), SingI (T.ToT st), T.HasNoOp (T.ToT st)
     , T.IsoValue st
     )
  => L.Contract cp st
  -> Text
  -> st
  -> Mutez
  -> IntegrationalScenarioM (T.ContractAddr cp)
lOriginate contract name value balance =
  T.ContractAddr <$>
  tOriginate (L.compileLorentz contract) name (T.toVal value) balance

-- | Originate a contract with empty balance and default storage.
lOriginateEmpty
  :: ( SingI (T.ToT cp), SingI (T.ToT st), T.HasNoOp (T.ToT st)
     , T.IsoValue st, Default st
     )
  => L.Contract cp st
  -> Text
  -> IntegrationalScenarioM (T.ContractAddr cp)
lOriginateEmpty contract name = lOriginate contract name def (unsafeMkMutez 0)

-- | Similar to 'transfer', for typed values.
tTransfer
  :: (SingI cp, T.HasNoOp cp)
  => "from" :! Address
  -> "to" :! Address
  -> Mutez
  -> T.Value cp
  -> IntegrationalScenarioM ()
tTransfer (arg #from -> from) (arg #to -> to) money param =
  let txData = TxData
        { tdSenderAddress = from
        , tdParameter = T.untypeValue param
        , tdAmount = money
        }
  in I.transfer txData to

-- | Similar to 'transfer', for Lorentz values.
lTransfer
  :: (SingI (T.ToT cp), T.HasNoOp (T.ToT cp), T.IsoValue cp)
  => "from" :! Address
  -> "to" :! T.ContractAddr cp
  -> Mutez
  -> cp
  -> IntegrationalScenarioM ()
lTransfer from (arg #to -> T.ContractAddr to) money param =
  tTransfer from (#to .! to) money (T.toVal param)

-- | Call a contract without caring about source address and money.
lCall
  :: (SingI (T.ToT cp), T.HasNoOp (T.ToT cp), T.IsoValue cp)
  => T.ContractAddr cp -> cp -> IntegrationalScenarioM ()
lCall contract param =
  lTransfer (#from .! genesisAddress) (#to .! contract)
    (unsafeMkMutez 1000) param

----------------------------------------------------------------------------
-- Validators to be used within 'IntegrationalValidator'
----------------------------------------------------------------------------

-- | Similar to 'expectStorageUpdate', for Lorentz values.
lExpectStorageUpdate
  :: ( T.IsoValue st, Each [Typeable, SingI, T.HasNoOp] '[T.ToT st]
     , HasCallStack
     )
  => T.ContractAddr cp -> (st -> Either I.ValidationError ()) -> SuccessValidator
lExpectStorageUpdate (T.ContractAddr addr) predicate =
  I.expectStorageUpdate addr $ \got -> do
    val <- first I.UnexpectedTypeCheckError $ typeCheck got
    predicate $ T.fromVal val
  where
    typeCheck uval =
      evaluatingState initSt . runExceptT $
      usingReaderT def $
      typeVerifyValue uval
    initSt = error "Typechecker state unavailable"

-- | Like 'expectBalance', for Lorentz values.
lExpectBalance :: T.ContractAddr cp -> Mutez -> SuccessValidator
lExpectBalance (T.ContractAddr addr) money = I.expectBalance addr money

-- | Similar to 'expectStorageConst', for Lorentz values.
lExpectStorageConst
  :: (T.IsoValue st, Each '[SingI, T.HasNoOp] '[T.ToT st])
  => T.ContractAddr cp -> st -> SuccessValidator
lExpectStorageConst (T.ContractAddr addr) expected =
  I.expectStorageConst addr (T.untypeValue $ T.toVal expected)

-- | Expect that interpretation of contract with given address ended
-- with [FAILED].
lExpectMichelsonFailed
  :: (MichelsonFailed -> Bool) -> T.ContractAddr cp -> InterpreterError -> Bool
lExpectMichelsonFailed predicate (T.ContractAddr addr) =
  I.expectMichelsonFailed predicate addr

-- | Expect contract to fail with "FAILWITH" instruction and provided value
-- to match against the given predicate.
lExpectFailWith
  :: forall e.
      (Typeable (T.ToT e), T.IsoValue e)
  => (e -> Bool) -> InterpreterError -> Bool
lExpectFailWith predicate =
  \case
    IEInterpreterFailed _ (RuntimeFailure (MichelsonFailedWith err, _)) ->
        case gcast err of
          Just errT -> predicate $ T.fromVal @e errT
          Nothing -> False
    _ -> False

-- | Expect contract to fail with given 'LorentzUserError' error.
lExpectError
  :: forall e.
      (L.IsError e)
  => (e -> Bool) -> InterpreterError -> Bool
lExpectError predicate =
  \case
    IEInterpreterFailed _ (RuntimeFailure (MichelsonFailedWith err, _)) ->
        case L.errorFromVal err of
          Right err' -> predicate err'
          Left _ -> False
    _ -> False

-- | Version of 'lExpectStorageUpdate' specialized to "consumer" contract
-- (see 'Lorentz.Contracts.Consumer.contractConsumer').
lExpectConsumerStorage
  :: (st ~ [cp], T.IsoValue st, Each [Typeable, SingI, T.HasNoOp] '[T.ToT st])
  => T.ContractAddr cp -> (st -> Either I.ValidationError ()) -> SuccessValidator
lExpectConsumerStorage = lExpectStorageUpdate

-- | Assuming that "consumer" contract receives a value from 'View', expect
-- this view return value to be the given one.
--
-- Despite consumer stores parameters it was called with in reversed order,
-- this function cares about it, so you should provide a list of expected values
-- in the same order in which the corresponding events were happenning.
lExpectViewConsumerStorage
  :: ( st ~ [cp], cp ~ (arg, Maybe res)
     , Eq res, Buildable res
     , T.IsoValue st, Each [Typeable, SingI, T.HasNoOp] '[T.ToT st]
     )
  => T.ContractAddr cp -> [res] -> SuccessValidator
lExpectViewConsumerStorage addr expected =
  lExpectConsumerStorage addr (extractJusts >=> matchExpected . reverse)
  where
    extractJusts = mapM $ \case
      (_, Just got) -> pure got
      (_, Nothing) -> mkError "Consumer got empty value unexpectedly"
    mkError = Left . I.CustomError
    matchExpected got
      | got == expected = pass
      | otherwise = mkError $ "Expected " +| listF expected |+
                              ", but got " +| listF got |+ ""
