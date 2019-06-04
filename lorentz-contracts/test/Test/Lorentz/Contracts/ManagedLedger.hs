-- To make multi genesis addresses bindings work, we don't need
-- high quality code in tests anyway.
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Module, containing spec to FA1.4.

module Test.Lorentz.Contracts.ManagedLedger
  ( spec_ManagedLedger
  ) where

-- Import Prelude to make intero calm
import Prelude

import qualified Data.Map as Map
import Fmt ((+||), (||+))
import Test.Hspec (Spec, describe, it)

import Lorentz (View(..))
import Lorentz.Contracts.Consumer
import Lorentz.Contracts.ManagedLedger
import Lorentz.Test
import Lorentz.Value
import Tezos.Address (Address)
import Util.Instances ()
import Util.Named ((.!))

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

wallet1, wallet2, wallet3, manager, manager2 :: Address
wallet1 : wallet2 : wallet3
  : manager : manager2 : _ = toList genesisAddresses

-- | Originate the contract we are currently testing with empty storage.
originateEmptyManagedLedger :: IntegrationalScenarioM (ContractAddr Parameter)
originateEmptyManagedLedger =
  lOriginate contract_Managed_ledger "Managed ledger"
    (mkStorage manager) (toMutez 1000)

-- | Originate the contract we are currently testing with given amount of money
-- on some accounts.
originateManagedLedger
  :: Natural
  -> IntegrationalScenarioM (ContractAddr Parameter)
originateManagedLedger initMoney =
  lOriginate contract_Managed_ledger "Managed ledger"
    (mkStorage manager)
    { ledger = BigMap $ Map.fromList
        [ (wallet1, (#balance .! initMoney, #approvals .! mempty))
        , (wallet2, (#balance .! initMoney, #approvals .! mempty))
        ]
    }
    (toMutez 1000)

spec_ManagedLedger :: Spec
spec_ManagedLedger = do
  describe "Transfers authorized by manager" $ do
    it "Can mint money" $
      integrationalTestProperty $ do
        erc20 <- originateEmptyManagedLedger
        consumer <- lOriginateEmpty contractConsumer "consumer"

        withSender manager . lCall erc20 $
          Transfer
          ( #from .! manager
          , #to .! wallet1
          , #deltaVal .! 10
          )

        lCall erc20 $ GetBalance (View wallet1 consumer)

        validate . Right $
          lExpectViewConsumerStorage consumer [10]

    it "Balance is initially empty" $
      integrationalTestProperty $ do
        erc20 <- originateEmptyManagedLedger
        consumer <- lOriginateEmpty contractConsumer "consumer"

        lCall erc20 $ GetBalance (View wallet1 consumer)

        validate . Right $
          lExpectViewConsumerStorage consumer [0]

    it "Can transfer money between two mortals" $
      integrationalTestProperty $ do
        erc20 <- originateEmptyManagedLedger
        consumer <- lOriginateEmpty contractConsumer "consumer"

        withSender manager $ do
          lCall erc20 $
            Transfer
            ( #from .! manager
            , #to .! wallet1
            , #deltaVal .! 10
            )
          lCall erc20 $
            Transfer
            ( #from .! wallet1
            , #to .! wallet2
            , #deltaVal .! 8
            )

        lCall erc20 $ GetBalance (View wallet1 consumer)
        lCall erc20 $ GetBalance (View wallet2 consumer)

        validate . Right $
          lExpectViewConsumerStorage consumer [2, 8]

    it "Can burn money" $
      integrationalTestProperty $ do
        erc20 <- originateEmptyManagedLedger
        consumer <- lOriginateEmpty contractConsumer "consumer"

        withSender manager $ do
          lCall erc20 $
            Transfer
            ( #from .! manager
            , #to .! wallet1
            , #deltaVal .! 10
            )
          lCall erc20 $
            Transfer
            ( #from .! wallet1
            , #to .! wallet2
            , #deltaVal .! 10
            )
          lCall erc20 $
            Transfer
            ( #from .! wallet2
            , #to .! manager
            , #deltaVal .! 9
            )

        lCall erc20 $ GetBalance (View wallet1 consumer)
        lCall erc20 $ GetBalance (View wallet2 consumer)
        lCall erc20 $ GetBalance (View manager consumer)

        validate . Right $
          lExpectViewConsumerStorage consumer [0, 1, 0]

  describe "Transfers authorized by users" $ do
    it "Can transfer own money" $
      integrationalTestProperty $ do
        erc20 <- originateManagedLedger 10
        consumer <- lOriginateEmpty contractConsumer "consumer"

        withSender wallet1 $ do
          lCall erc20 $
            Transfer
            ( #from .! wallet1
            , #to .! wallet2
            , #deltaVal .! 5
            )

        lCall erc20 $ GetBalance (View wallet2 consumer)

        validate . Right $
          lExpectViewConsumerStorage consumer [15]

    it "Cannot transfer foreign money" $
      integrationalTestProperty $ do
        erc20 <- originateManagedLedger 10

        withSender wallet2 $ do
          lCall erc20 $
            Transfer
            ( #from .! wallet1
            , #to .! wallet2
            , #deltaVal .! 5
            )

        validate . Left $
          lExpectUserError (== NotEnoughAllowance (#required .! 5, #present .! 0))

    it "Cannot transfer too much money" $
      integrationalTestProperty $ do
        erc20 <- originateManagedLedger 10

        withSender wallet1 $ do
          lCall erc20 $
            Transfer
            ( #from .! wallet1
            , #to .! wallet2
            , #deltaVal .! 11
            )

        validate . Left $
          lExpectUserError (== NotEnoughBalance (#required .! 11, #present .! 10))

    it "Transferring 0 tokens does not create entry in the ledger" $
      integrationalTestProperty $ do
        erc20 <- originateManagedLedger 10

        withSender wallet1 $ do
          lCall erc20 $
            Transfer
            ( #from .! wallet1
            , #to .! wallet3
            , #deltaVal .! 0
            )

        validate . Right $
          lExpectStorageUpdate erc20 $ \(st :: Storage) ->
            case Map.lookup wallet3 $ unBigMap (ledger st) of
              Nothing -> pass
              Just lv -> Left . CustomError $
                "Expected no value in ledger for wallet3 \
                \but got " +|| lv ||+ ""

  describe "Allowance" $ do
    it "Can get allowance" $
      integrationalTestProperty $ do
        erc20 <- originateManagedLedger 10
        consumer <- lOriginateEmpty contractConsumer "consumer"

        withSender wallet1 $ do
          lCall erc20 $
            Approve
            ( #to .! wallet2
            , #val .! 5
            )

        lCall erc20 $ GetAllowance
          (View (#from .! wallet1, #to .! wallet2) consumer)

        withSender wallet2 $ do
          lCall erc20 $
            Transfer
            ( #from .! wallet1
            , #to .! wallet2
            , #deltaVal .! 5
            )

        lCall erc20 $ GetAllowance
          (View (#from .! wallet1, #to .! wallet2) consumer)

        validate . Right $
          lExpectViewConsumerStorage consumer [5, 0]

    it "Can spend foreign money with allowance" $
      integrationalTestProperty $ do
        erc20 <- originateManagedLedger 10
        consumer <- lOriginateEmpty contractConsumer "consumer"

        withSender wallet1 $ do
          lCall erc20 $
            Approve
            ( #to .! wallet2
            , #val .! 5
            )
        withSender wallet2 $ do
          lCall erc20 $
            Transfer
            ( #from .! wallet1
            , #to .! wallet2
            , #deltaVal .! 5
            )

        lCall erc20 $ GetBalance (View wallet2 consumer)

        validate . Right $
          lExpectViewConsumerStorage consumer [15]

    it "Cannot spend more than allowed amount of money" $
      integrationalTestProperty $ do
        erc20 <- originateManagedLedger 10

        withSender wallet1 $ do
          lCall erc20 $
            Approve
            ( #to .! wallet2
            , #val .! 5
            )
        withSender wallet2 . replicateM_ 2 $ do
          lCall erc20 $
            Transfer
            ( #from .! wallet1
            , #to .! wallet2
            , #deltaVal .! 3
            )

        validate . Left $
          lExpectUserError (== NotEnoughAllowance (#required .! 3, #present .! 2))

    -- TODO: do we need this behavior?
    -- it "Approving funds flow from manager is not possible" $

    it "Can close allowance suggestion" $
      integrationalTestProperty $ do
        erc20 <- originateManagedLedger 10

        withSender wallet1 $ do
          lCall erc20 $
            Approve
            ( #to .! wallet2
            , #val .! 5
            )
          lCall erc20 $
            Approve
            ( #to .! wallet2
            , #val .! 0
            )
        withSender wallet2 $ do
          lCall erc20 $
            Transfer
            ( #from .! wallet1
            , #to .! wallet2
            , #deltaVal .! 1
            )

        validate . Left $
          lExpectUserError (== NotEnoughAllowance (#required .! 1, #present .! 0))

    it "Can transfer approved money to a foreign address" $
      integrationalTestProperty $ do
        erc20 <- originateManagedLedger 10
        consumer <- lOriginateEmpty contractConsumer "consumer"

        withSender wallet1 $ do
          lCall erc20 $
            Approve
            ( #to .! wallet2
            , #val .! 5
            )
        withSender wallet2 $ do
          lCall erc20 $
            Transfer
            ( #from .! wallet1
            , #to .! wallet3
            , #deltaVal .! 1
            )

        lCall erc20 $ GetBalance (View wallet1 consumer)
        lCall erc20 $ GetBalance (View wallet2 consumer)
        lCall erc20 $ GetBalance (View wallet3 consumer)

        validate . Right $
          lExpectViewConsumerStorage consumer [9, 10, 1]

    it "Cannot set allowance from non-zero to non-zero" $
      integrationalTestProperty $ do
        erc20 <- originateManagedLedger 10

        withSender wallet1 $ do
          lCall erc20 $
            Approve
            ( #to .! wallet2
            , #val .! 5
            )
          lCall erc20 $
            Approve
            ( #to .! wallet2
            , #val .! 3
            )

        validate . Left $
          lExpectUserError (== UnsafeAllowanceChange 5)

  describe "setPause" $ do
    it "Pause prohibits transfers" $
      integrationalTestProperty $ do
        erc20 <- originateManagedLedger 10

        withSender manager $ do
          lCall erc20 $ SetPause True
        withSender wallet1 $ do
          lCall erc20 $
            Transfer
            ( #from .! wallet1
            , #to .! wallet2
            , #deltaVal .! 3
            )

        validate . Left $
          lExpectUserError (== OperationsArePaused)

    it "Pause prohibits approvals" $
      integrationalTestProperty $ do
        erc20 <- originateManagedLedger 10

        withSender manager $ do
          lCall erc20 $ SetPause True
        withSender wallet1 $ do
          lCall erc20 $
            Approve
            ( #to .! wallet2
            , #val .! 3
            )

        validate . Left $
          lExpectUserError (== OperationsArePaused)

    it "Manager cannot do transfers even during pause" $
      integrationalTestProperty $ do
        erc20 <- originateManagedLedger 10

        withSender manager $ do
          lCall erc20 $ SetPause True
        withSender manager $ do
          lCall erc20 $
            Transfer
            ( #from .! wallet1
            , #to .! wallet2
            , #deltaVal .! 3
            )

        validate . Left $
          lExpectUserError (== OperationsArePaused)

  describe "setManager" $ do
    it "Manager can delegate his rights" $
      integrationalTestProperty $ do
        erc20 <- originateManagedLedger 10

        withSender manager $ do
          lCall erc20 $ SetManager manager2
        withSender manager2 $ do
          lCall erc20 $ SetPause True

        validate . Right $ expectAnySuccess

    it "Only manager can set a new manager" $
      integrationalTestProperty $ do
        erc20 <- originateManagedLedger 10

        withSender wallet1 $ do
          lCall erc20 $ SetManager wallet2

        validate . Left $
          lExpectUserError (== InitiatorIsNotManager)

    it "Cannot set manager to an address from ledger" $
      integrationalTestProperty $ do
        erc20 <- originateManagedLedger 10

        withSender manager $ do
          lCall erc20 $ SetManager wallet2

        validate . Left $
          lExpectUserError (== ManagerAddressWouldShadow)

  describe "Total supply" $ do
    it "Is correct after multiple transfers" $
      integrationalTestProperty $ do
        erc20 <- originateEmptyManagedLedger
        consumer <- lOriginateEmpty contractConsumer "consumer"

        withSender manager $ do
          lCall erc20 $
            Transfer
            ( #from .! manager
            , #to .! wallet1
            , #deltaVal .! 20
            )
          lCall erc20 $
            Transfer
            ( #from .! manager
            , #to .! wallet2
            , #deltaVal .! 5
            )
          lCall erc20 $
            Transfer
            ( #from .! wallet1
            , #to .! wallet2
            , #deltaVal .! 10
            )
          lCall erc20 $
            Transfer
            ( #from .! wallet2
            , #to .! manager
            , #deltaVal .! 1
            )

        lCall erc20 $ GetTotalSupply (View () consumer)

        validate . Right $
          lExpectViewConsumerStorage consumer [24]
