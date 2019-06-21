module Test.Lorentz.Contracts.UpgradableCounter
  ( spec_UpgradableCounter
  ) where

import Lorentz (View(..), MText, mt)

import Test.Hspec (Spec, describe, it)

import Lorentz.Constraints
import Lorentz.Contracts.Consumer (contractConsumer)
import Lorentz.Contracts.UpgradableCounter
import qualified Lorentz.Contracts.UpgradableCounter.V1 as V1
import qualified Lorentz.Contracts.UpgradableCounter.V2 as V2
import Lorentz.Test
import Lorentz.Value
import Michelson.Interpret.Pack (packValue')
import qualified Michelson.Typed as T
import Util.Instances ()


originateUpgradableCounter
  :: IntegrationalScenarioM (ContractAddr Parameter)
originateUpgradableCounter =
  lOriginate upgradableCounterContract "UpgradableCounter"
    emptyMigration (toMutez 1000)

originateUpgradableCounterV1
  :: IntegrationalScenarioM (ContractAddr Parameter)
originateUpgradableCounterV1 = do
  contract <- originateUpgradableCounter
  lCall contract $ Upgrade (V1.migrate, V1.counterContract)
  return contract

uCall
  :: forall a.
     (KnownValue a, IsoValue a, T.HasNoOp (ToT a), T.HasNoBigMap (ToT a))
  => ContractAddr Parameter -> MText -> a
  -> IntegrationalScenarioM ()
uCall contract method arg = do
  lCall contract $ Run (method, packValue' $ toVal arg)

getCounterValueV1
  :: ContractAddr Parameter
  -> ContractAddr Natural
  -> IntegrationalScenarioM ()
getCounterValueV1 contract consumer = do
  uCall contract [mt|GetCounterValue|] $ View () consumer

getCounterValueV2
  :: ContractAddr Parameter
  -> ContractAddr Integer
  -> IntegrationalScenarioM ()
getCounterValueV2 contract consumer = do
  uCall contract [mt|GetCounterValue|] $ View () consumer

spec_UpgradableCounter :: Spec
spec_UpgradableCounter = do
  describe "v1" $ do
    it "Initially contains zero" $ do
      integrationalTestProperty $ do
        contract <- originateUpgradableCounterV1
        consumer <- lOriginateEmpty contractConsumer "consumer"
        getCounterValueV1 contract consumer
        validate . Right $
          lExpectViewConsumerStorage consumer [0]

    it "Updates counter after each operation" $ do
      integrationalTestProperty $ do
        contract <- originateUpgradableCounterV1
        consumer <- lOriginateEmpty contractConsumer "consumer"

        uCall contract [mt|Add|] (2 :: Natural)
        getCounterValueV1 contract consumer

        uCall contract [mt|Mul|] (3 :: Natural)
        getCounterValueV1 contract consumer

        validate . Right $
          lExpectViewConsumerStorage consumer [2, 6]

  describe "v2" $ do
    it "Preserves the counter after the upgrade" $ do
      integrationalTestProperty $ do
        contract <- originateUpgradableCounterV1
        consumer <- lOriginateEmpty contractConsumer "consumer"

        uCall contract [mt|Add|] (42 :: Natural)
        lCall contract $ Upgrade (V2.migrate, V2.counterContract)
        getCounterValueV2 contract consumer

        validate . Right $
          lExpectViewConsumerStorage consumer [42]

    it "Exposes new methods" $ do
      integrationalTestProperty $ do
        contract <- originateUpgradableCounterV1
        consumer <- lOriginateEmpty contractConsumer "consumer"

        lCall contract $ Upgrade (V2.migrate, V2.counterContract)
        uCall contract [mt|Inc|] ()
        uCall contract [mt|Inc|] ()
        getCounterValueV2 contract consumer
        uCall contract [mt|Dec|] ()
        getCounterValueV2 contract consumer

        validate . Right $
          lExpectViewConsumerStorage consumer [2, 1]

    it "Allows to decrement below zero" $ do
      integrationalTestProperty $ do
        contract <- originateUpgradableCounterV1
        consumer <- lOriginateEmpty contractConsumer "consumer"

        lCall contract $ Upgrade (V2.migrate, V2.counterContract)
        uCall contract [mt|Dec|] ()
        getCounterValueV2 contract consumer
        uCall contract [mt|Dec|] ()
        getCounterValueV2 contract consumer
        uCall contract [mt|Dec|] ()
        getCounterValueV2 contract consumer

        validate . Right $
          lExpectViewConsumerStorage consumer [-1, -2, -3]
