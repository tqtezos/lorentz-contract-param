-- | Tests for managed ledger implementations (all of them).

module Test.Lorentz.Contracts.ManagedLedger
  ( spec_ManagedLedger
  , test_ManagedLedgerAthens
  ) where

-- Import Prelude to make intero calm
import Prelude

import qualified Data.Map as Map
import Data.Singletons (SingI)
import Test.Hspec (Spec, describe, it)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

import Lorentz (View(..))
import qualified Lorentz as L
import Lorentz.Contracts.Consumer
import qualified Lorentz.Contracts.ManagedLedger.Athens as Athens
import qualified Lorentz.Contracts.ManagedLedger.Babylon as Babylon
import qualified Lorentz.Contracts.ManagedLedger.Proxy as Proxy
import qualified Lorentz.Contracts.ManagedLedger.Types as Types
import Lorentz.Test
import Lorentz.Value
import qualified Michelson.Typed as T
import Tezos.Address (Address)
import Util.Named ((.!))

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

wallet1, wallet2, wallet3, admin, admin2 :: Address
wallet1 = genesisAddress1
wallet2 = genesisAddress2
wallet3 = genesisAddress3
admin = genesisAddress4
admin2 = genesisAddress5

type GoodParam cp = (SingI (T.ToT cp), T.HasNoOp (T.ToT cp), T.IsoValue cp)
type GoodStorage st = GoodParam st

-- | Originate one of the managed ledger contracts with a storage
-- returned created by given function which takes admin address and
-- initial balances.
-- If initial balance is 0, ledger is empty.
originateManagedLedger
  :: forall param storage.
     (GoodParam param, GoodStorage storage)
  => (Address -> Map Address Natural -> storage)
  -> L.Contract param storage
  -> Natural
  -> IntegrationalScenarioM (ContractAddr param)
originateManagedLedger mkStorage contract initMoney =
  lOriginate contract "Managed ledger"
    (mkStorage admin balances) (toMutez 1000)
  where
    balances
      | initMoney == 0 = mempty
      | otherwise = someBalances

    someBalances :: Map Address Natural
    someBalances = Map.fromList
      [ (wallet1, initMoney)
      , (wallet2, initMoney)
      ]

-- | This data type allows is an abstraction layer for all contracts
-- which implement approvable ledger interface. Since
-- 'Proxy.SaneParameter' contains exactly FA1.2 methods we require
-- parameter to be constructible from it.
data ApprovableLedger param = ApprovableLedger
  { alOriginate :: Natural -> IntegrationalScenarioM (ContractAddr param)
  -- ^ The argument denotes initial balances (can be 0).
  , alMkParam :: Proxy.SaneParameter -> param
  }

alBabylon :: ApprovableLedger Babylon.Parameter
alBabylon = ApprovableLedger
  { alOriginate =
    originateManagedLedger Babylon.mkStorage Babylon.managedLedgerContract
  , alMkParam = \case
      Proxy.STransfer tp -> Babylon.Transfer tp
      Proxy.SApprove tp -> Babylon.Approve tp
      Proxy.SGetAllowance tp -> Babylon.GetAllowance tp
      Proxy.SGetBalance tp -> Babylon.GetBalance tp
      Proxy.SGetTotalSupply tp -> Babylon.GetTotalSupply tp
  }

alAthens :: ApprovableLedger Athens.Parameter
alAthens = ApprovableLedger
  { alOriginate =
    originateManagedLedger Athens.mkStorage Athens.managedLedgerAthensContract
  , alMkParam = \case
      Proxy.STransfer tp -> Athens.Transfer tp
      Proxy.SApprove arg -> Athens.Approve arg
      Proxy.SGetAllowance arg -> Athens.GetAllowance arg
      Proxy.SGetBalance arg -> Athens.GetBalance arg
      Proxy.SGetTotalSupply arg -> Athens.GetTotalSupply arg
  }

alProxy :: ApprovableLedger Proxy.Parameter
alProxy = ApprovableLedger
  { alOriginate = \initMoney -> do
      athensAddr <-
        originateManagedLedger Athens.mkStorage Athens.managedLedgerAthensContract initMoney
      proxyAddr <-
        lOriginate Proxy.managedLedgerProxyContract "Proxy" athensAddr (toMutez 1000)
      proxyAddr <$ withSender admin
        (lCall athensAddr (Athens.SetProxy $ T.unContractAddress proxyAddr))
  , alMkParam = Proxy.fromSaneParameter
  }

-- | This data type allows is an abstraction layer for all contracts
-- which implement managed ledger interface. Since
-- 'Babylon.Parameter' contains exactly managed ledger methods we require
-- parameter to be constructible from it.
data ManagedLedger param = ManagedLedger
  { mlApprovable :: ApprovableLedger param
  -- ^ Managed ledger always implements approvable ledger.
  , mlMkParam :: Babylon.Parameter -> param
  }

mlBabylon :: ManagedLedger Babylon.Parameter
mlBabylon = ManagedLedger
  { mlApprovable = alBabylon
  , mlMkParam = id
  }

mlAthens :: ManagedLedger Athens.Parameter
mlAthens = ManagedLedger
  { mlApprovable = alAthens
  , mlMkParam = \case
      Babylon.Transfer tp -> Athens.Transfer tp
      Babylon.Approve arg -> Athens.Approve arg
      Babylon.GetAllowance arg -> Athens.GetAllowance arg
      Babylon.GetBalance arg -> Athens.GetBalance arg
      Babylon.GetTotalSupply arg -> Athens.GetTotalSupply arg
      Babylon.SetPause arg -> Athens.SetPause arg
      Babylon.SetAdministrator arg -> Athens.SetAdministrator arg
      Babylon.GetAdministrator arg -> Athens.GetAdministrator arg
      Babylon.Mint arg -> Athens.Mint arg
      Babylon.Burn arg -> Athens.Burn arg
  }

spec_ManagedLedger :: Spec
spec_ManagedLedger = do
  describe "Babylon" $ do
    approvableLedgerSpec alBabylon
    managedLedgerSpec mlBabylon

  describe "Athens" $ do
    approvableLedgerSpec alAthens
    managedLedgerSpec mlAthens

  describe "Proxy" $ do
    approvableLedgerSpec alProxy

-- | A set of tests for all contracts which implement approvable
-- ledger interface.
approvableLedgerSpec ::
  forall param. GoodParam param => ApprovableLedger param -> Spec
approvableLedgerSpec ApprovableLedger {..} = do
  let
    lCallApprovable ::
      ContractAddr param -> Proxy.SaneParameter -> IntegrationalScenarioM ()
    lCallApprovable contractAddr = lCall contractAddr . alMkParam

  it "Balance is initially empty" $
    integrationalTestExpectation $ do
      al <- alOriginate 0
      consumer <- lOriginateEmpty contractConsumer "consumer"

      lCallApprovable al $ Proxy.SGetBalance (View wallet1 consumer)

      validate . Right $
        lExpectViewConsumerStorage consumer [0]

  describe "Transfers authorized by users" $ do
    it "Can transfer own money" $
      integrationalTestExpectation $ do
        ml <- alOriginate 10
        consumer <- lOriginateEmpty contractConsumer "consumer"

        withSender wallet1 $ do
          lCallApprovable ml $
            Proxy.STransfer
            ( #from .! wallet1
            , #to .! wallet2
            , #value .! 5
            )

        lCallApprovable ml $ Proxy.SGetBalance (View wallet2 consumer)

        validate . Right $
          lExpectViewConsumerStorage consumer [15]

    it "Cannot transfer foreign money" $
      integrationalTestExpectation $ do
        ml <- alOriginate 10

        withSender wallet2 $ do
          lCallApprovable ml $
            Proxy.STransfer
            ( #from .! wallet1
            , #to .! wallet2
            , #value .! 5
            )

        validate . Left $
          lExpectError (== Types.NotEnoughAllowance (#required .! 5, #present .! 0))

    it "Cannot transfer too much money" $
      integrationalTestExpectation $ do
        ml <- alOriginate 10

        withSender wallet1 $ do
          lCallApprovable ml $
            Proxy.STransfer
            ( #from .! wallet1
            , #to .! wallet2
            , #value .! 11
            )

        validate . Left $
          lExpectError (== Types.NotEnoughBalance (#required .! 11, #present .! 10))

    it "Transferring 0 tokens does not create entry in the ledger" $
      integrationalTestExpectation $ do
        ml <- alOriginate 10

        validate . Right $ expectAnySuccess
        withSender wallet1 $ do
          lCallApprovable ml $
            Proxy.STransfer
            ( #from .! wallet1
            , #to .! wallet3
            , #value .! 0
            )

        validate . Right $ expectNoStorageUpdates

  describe "Allowance" $ do
    it "Can get allowance" $
      integrationalTestExpectation $ do
        ml <- alOriginate 10
        consumer <- lOriginateEmpty contractConsumer "consumer"

        withSender wallet1 $ do
          lCallApprovable ml $
            Proxy.SApprove
            ( #spender .! wallet2
            , #value .! 5
            )

        lCallApprovable ml $ Proxy.SGetAllowance
          (View (#owner .! wallet1, #spender .! wallet2) consumer)

        withSender wallet2 $ do
          lCallApprovable ml $
            Proxy.STransfer
            ( #from .! wallet1
            , #to .! wallet2
            , #value .! 5
            )

        lCallApprovable ml $ Proxy.SGetAllowance
          (View (#owner .! wallet1, #spender .! wallet2) consumer)

        validate . Right $
          lExpectViewConsumerStorage consumer [5, 0]

    it "Can spend foreign money with allowance" $
      integrationalTestExpectation $ do
        ml <- alOriginate 10
        consumer <- lOriginateEmpty contractConsumer "consumer"

        withSender wallet1 $ do
          lCallApprovable ml $
            Proxy.SApprove
            ( #spender .! wallet2
            , #value .! 5
            )
        withSender wallet2 $ do
          lCallApprovable ml $
            Proxy.STransfer
            ( #from .! wallet1
            , #to .! wallet2
            , #value .! 5
            )

        lCallApprovable ml $ Proxy.SGetBalance (View wallet2 consumer)

        validate . Right $
          lExpectViewConsumerStorage consumer [15]

    it "Cannot spend more than allowed amount of money" $
      integrationalTestExpectation $ do
        ml <- alOriginate 10

        withSender wallet1 $ do
          lCallApprovable ml $
            Proxy.SApprove
            ( #spender .! wallet2
            , #value .! 5
            )
        withSender wallet2 . replicateM_ 2 $ do
          lCallApprovable ml $
            Proxy.STransfer
            ( #from .! wallet1
            , #to .! wallet2
            , #value .! 3
            )

        validate . Left $
          lExpectError (== Types.NotEnoughAllowance (#required .! 3, #present .! 2))

    it "Can close allowance suggestion" $
      integrationalTestExpectation $ do
        ml <- alOriginate 10

        withSender wallet1 $ do
          lCallApprovable ml $
            Proxy.SApprove
            ( #spender .! wallet2
            , #value .! 5
            )
          lCallApprovable ml $
            Proxy.SApprove
            ( #spender .! wallet2
            , #value .! 0
            )
        withSender wallet2 $ do
          lCallApprovable ml $
            Proxy.STransfer
            ( #from .! wallet1
            , #to .! wallet2
            , #value .! 1
            )

        validate . Left $
          lExpectError (== Types.NotEnoughAllowance (#required .! 1, #present .! 0))

    it "Can transfer approved money to a foreign address" $
      integrationalTestExpectation $ do
        ml <- alOriginate 10
        consumer <- lOriginateEmpty contractConsumer "consumer"

        withSender wallet1 $ do
          lCallApprovable ml $
            Proxy.SApprove
            ( #spender .! wallet2
            , #value .! 5
            )
        withSender wallet2 $ do
          lCallApprovable ml $
            Proxy.STransfer
            ( #from .! wallet1
            , #to .! wallet3
            , #value .! 1
            )

        lCallApprovable ml $ Proxy.SGetBalance (View wallet1 consumer)
        lCallApprovable ml $ Proxy.SGetBalance (View wallet2 consumer)
        lCallApprovable ml $ Proxy.SGetBalance (View wallet3 consumer)

        validate . Right $
          lExpectViewConsumerStorage consumer [9, 10, 1]

    it "Cannot set allowance from non-zero to non-zero" $
      integrationalTestExpectation $ do
        ml <- alOriginate 10

        withSender wallet1 $ do
          lCallApprovable ml $
            Proxy.SApprove
            ( #spender .! wallet2
            , #value .! 5
            )
          lCallApprovable ml $
            Proxy.SApprove
            ( #spender .! wallet2
            , #value .! 3
            )

        validate . Left $
          lExpectError (== Types.UnsafeAllowanceChange 5)

-- | A set of tests for all contracts which implement managed ledger.
managedLedgerSpec ::
  forall param. GoodParam param => ManagedLedger param -> Spec
managedLedgerSpec ManagedLedger { mlApprovable = ApprovableLedger {..}, ..} = do
  let
    lCallManaged ::
      ContractAddr param -> Babylon.Parameter -> IntegrationalScenarioM ()
    lCallManaged contractAddr = lCall contractAddr . mlMkParam

  describe "Transfers authorized by admin" $ do
    it "Can mint tokens" $
      integrationalTestExpectation $ do
        ml <- alOriginate 0
        consumer <- lOriginateEmpty contractConsumer "consumer"

        withSender admin . lCallManaged ml $
          Babylon.Mint
          ( #to .! wallet1
          , #value .! 10
          )

        lCallManaged ml $ Babylon.GetBalance (View wallet1 consumer)

        validate . Right $
          lExpectViewConsumerStorage consumer [10]

    it "Can not transfer not own tokens without approval" $
      integrationalTestExpectation $ do
        ml <- alOriginate 0

        withSender admin $ do
          lCallManaged ml $
            Babylon.Mint
            ( #to .! wallet1
            , #value .! 10
            )
          lCallManaged ml $
            Babylon.Transfer
            ( #from .! wallet1
            , #to .! wallet2
            , #value .! 8
            )

        validate . Left $
          lExpectError (== Types.NotEnoughAllowance (#required .! 8, #present .! 0))

    it "Can burn money" $
      integrationalTestExpectation $ do
        ml <- alOriginate 0
        consumer <- lOriginateEmpty contractConsumer "consumer"

        withSender admin $
          lCallManaged ml $
            Babylon.Mint
            ( #to .! wallet1
            , #value .! 10
            )

        withSender wallet1 $
          lCallManaged ml $
            Babylon.Transfer
            ( #from .! wallet1
            , #to .! wallet2
            , #value .! 10
            )

        withSender admin $
          lCallManaged ml $
            Babylon.Burn
            ( #from .! wallet2
            , #value .! 9
            )

        lCallManaged ml $ Babylon.GetBalance (View wallet1 consumer)
        lCallManaged ml $ Babylon.GetBalance (View wallet2 consumer)

        validate . Right $
          lExpectViewConsumerStorage consumer [0, 1]

  describe "setPause" $ do
    it "Pause prohibits transfers" $
      integrationalTestExpectation $ do
        ml <- alOriginate 10

        withSender admin $ do
          lCallManaged ml $ Babylon.SetPause True
        withSender wallet1 $ do
          lCallManaged ml $
            Babylon.Transfer
            ( #from .! wallet1
            , #to .! wallet2
            , #value .! 3
            )

        validate . Left $
          lExpectError (== Types.OperationsArePaused)

    it "Pause prohibits approvals" $
      integrationalTestExpectation $ do
        ml <- alOriginate 10

        withSender admin $ do
          lCallManaged ml $ Babylon.SetPause True
        withSender wallet1 $ do
          lCallManaged ml $
            Babylon.Approve
            ( #spender .! wallet2
            , #value .! 3
            )

        validate . Left $
          lExpectError (== Types.OperationsArePaused)

    it "Admin cannot do transfers even during pause" $
      integrationalTestExpectation $ do
        ml <- alOriginate 10

        withSender admin $ do
          lCallManaged ml $ Babylon.SetPause True
        withSender admin $ do
          lCallManaged ml $
            Babylon.Transfer
            ( #from .! wallet1
            , #to .! wallet2
            , #value .! 3
            )

        validate . Left $
          lExpectError (== Types.OperationsArePaused)

  describe "setAdministrator" $ do
    it "Admin can delegate his rights" $
      integrationalTestExpectation $ do
        ml <- alOriginate 10

        withSender admin $ do
          lCallManaged ml $ Babylon.SetAdministrator admin2
        withSender admin2 $ do
          lCallManaged ml $ Babylon.SetPause True

        validate . Right $ expectAnySuccess

    it "Only admin can set a new admin" $
      integrationalTestExpectation $ do
        ml <- alOriginate 10

        withSender wallet1 $ do
          lCallManaged ml $ Babylon.SetAdministrator wallet2

        validate . Left $
          lExpectError (== Types.SenderIsNotAdmin)

  describe "Total supply" $ do
    it "Is correct after multiple transfers" $
      integrationalTestExpectation $ do
        ml <- alOriginate 0
        consumer <- lOriginateEmpty contractConsumer "consumer"

        withSender admin $ do
          lCallManaged ml $
            Babylon.Mint
            ( #to .! wallet1
            , #value .! 20
            )
          lCallManaged ml $
            Babylon.Mint
            ( #to .! wallet2
            , #value .! 5
            )
          lCallManaged ml $
            Babylon.Burn
            ( #from .! wallet2
            , #value .! 1
            )

        withSender wallet1 $
          lCallManaged ml $
            Babylon.Transfer
            ( #from .! wallet1
            , #to .! wallet2
            , #value .! 10
            )

        lCallManaged ml $ Babylon.GetTotalSupply (View () consumer)

        validate . Right $
          lExpectViewConsumerStorage consumer [24]

-- | Tests specific to 'ManagedLedgerAthens'.
test_ManagedLedgerAthens :: [TestTree]
test_ManagedLedgerAthens =
  [ testCase "Arbitrary user can not set proxy" $
    integrationalTestExpectation $ do
      ml <- originateManagedLedger
        Athens.mkStorage Athens.managedLedgerAthensContract 0

      withSender wallet1 $
        lCall ml (Athens.SetProxy $ wallet1)

      validate . Left $
        lExpectError (== Athens.NotAllowedToSetProxy)

  , testCase "Proxy can not be set twice" $
    integrationalTestExpectation $ do
      ml <- originateManagedLedger
        Athens.mkStorage Athens.managedLedgerAthensContract 0

      withSender admin $ do
        lCall ml (Athens.SetProxy $ wallet1)
        lCall ml (Athens.SetProxy $ wallet1)

      validate . Left $
        lExpectError (== Athens.ProxyAlreadySet)
  ]
