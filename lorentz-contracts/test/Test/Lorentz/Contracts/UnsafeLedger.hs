module Test.Lorentz.Contracts.UnsafeLedger
  ( test_UnsafeLedger
  ) where

import qualified Data.Map as M
import Test.Hspec.Expectations (Expectation, shouldSatisfy)
import Test.HUnit (Assertion, assertFailure, (@?=))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Lorentz (Address, View(..), compileLorentzContract)
import Lorentz.Contracts.UnsafeLedger
import Michelson.Interpret (ceAmount)
import Michelson.Runtime.GState (genesisAddress)
import Michelson.Test (concatTestTrees, contractProp, dummyContractEnv, testTreesWithTypedContract)
import Michelson.Test.Unit
import qualified Michelson.Typed as T
import Tezos.Address
import Tezos.Core (Mutez)

-- | All tests for UnsafeLedger (FA1).
test_UnsafeLedger :: IO [TestTree]
test_UnsafeLedger = concatTestTrees
  [ one . testGroup "Morley version" <$>
    testTreesWithTypedContract "../contracts/FA1/FA1.mtz" testImpl
  , one . testGroup "Lorentz version" <$>
    testImpl (compileLorentzContract unsafeLedgerContract)
  ]
  where
    testImpl fa1 = pure
      [ testGroup "Transfer" $ do
          transfer fa1

      , testGroup "GetTotalSupply" $ do
          getTotalSupply fa1

      , testGroup "GetBalance" $ do
          getBalance fa1
      ]

type TStorage = T.ToT Storage
type TContract = T.Contract (T.ToT Parameter) TStorage

source, foo, bar, feather :: Address
source  = genesisAddress
foo     = mkContractAddressRaw "foo"
bar     = mkContractAddressRaw "bar"
feather = mkContractAddressRaw "feather"

transferredAmount :: Mutez
transferredAmount = ceAmount dummyContractEnv

storage :: Storage
storage = Storage
  { ledger = T.BigMap $ M.fromList
    [ (bar,    300)
    , (source, 200)
    ]
  , totalSupply = 500
  }

-- | A contract specification factory.
makeTestCase
  :: TContract
  -> String
  -> ContractPropValidator TStorage Assertion
  -> Parameter
  -> TestTree
makeTestCase contract name validator param =
  testCase name $ do
    contractProp contract validator dummyContractEnv param storage

-- | Testing various properties of `GetBalance`.
getBalance
  :: TContract
  -> [TestTree]
getBalance contract =
  [ makeTestCase'
      "just gets balance for recorded address"
      gettingExistingBalance
      $ GetBalance (View bar (T.ContractAddr feather))

  , makeTestCase'
      "just gets balance for missing address"
      gettingNonExistingBalance
      $ GetBalance (View foo (T.ContractAddr feather))
  ]
  where
    makeTestCase' = makeTestCase contract

-- | Testing properties of `GetBalance` when dest address is present.
gettingExistingBalance
  :: ContractPropValidator TStorage Assertion
gettingExistingBalance (Left _, _) = do
  assertFailure "shouldn't end with an error"
gettingExistingBalance (Right (ops, store), _) = do
  store @?= T.toVal storage
  ops   @?=
    [ T.OpTransferTokens $
        T.TransferTokens
          { ttContractParameter = T.toVal @Natural 300
          , ttAmount   = transferredAmount
          , ttContract = T.VContract feather
        }
    ]

-- | Testing properties of `GetBalance` when dest address is NOT present.
gettingNonExistingBalance
  :: ContractPropValidator TStorage Assertion
gettingNonExistingBalance (Left _, _) = do
  assertFailure "shouldn't end with an error"
gettingNonExistingBalance (Right (ops, store), _) = do
  store @?= T.toVal storage
  ops @?=
    [ T.OpTransferTokens $
        T.TransferTokens
          { ttContractParameter = T.toVal @Natural 0
          , ttAmount   = transferredAmount
          , ttContract = T.VContract feather
          }
    ]

-- | Testing properties of `GetTotalSupply`.
getTotalSupply :: TContract -> [TestTree]
getTotalSupply contract =
  [ makeTestCase contract "getting" gettingTotalSupply
      $ GetTotalSupply (View () (T.ContractAddr feather))
  ]

-- | Checking that `GetTotalSupply` doesn't change the storage
--   and returns total supply to the provided address.
gettingTotalSupply
  :: ContractPropValidator TStorage Assertion
gettingTotalSupply (Left _, _) = do
  assertFailure "shouldn't end with an error"
gettingTotalSupply (Right (ops, store), _) = do
  store @?= T.toVal storage
  ops @?=
    [ T.OpTransferTokens $
        T.TransferTokens
          { ttContractParameter = T.toVal @Natural 500
          , ttAmount            = transferredAmount
          , ttContract          = T.VContract feather
        }
    ]

-- | Testing properties of `Transfer`.
transfer :: TContract -> [TestTree]
transfer contract =
  [ makeTestCase' "paying"          success
    $ Transfer (#to foo,    #val 10)
  , makeTestCase' "paying 0"        payNothing
    $ Transfer (#to foo,    #val 0)
  , makeTestCase' "paying to self"  nothingChanges
    $ Transfer (#to source, #val 10)
  , makeTestCase' "paying more than you have" overdraft
    $ Transfer (#to foo, #val 500)
  , makeTestCase' "paying to self more than you have" selfOverdraft
    $ Transfer (#to source, #val 500)
  ]
  where
    makeTestCase' = makeTestCase contract

-- | Checking that paying more that you have fails.
overdraft
  :: ContractPropValidator TStorage Expectation
overdraft (result, _) = do
  shouldSatisfy result isLeft

-- | Checking that no changes to storage were made.
nothingChanges
  :: ContractPropValidator TStorage Assertion
nothingChanges (Left _, _) = do
  assertFailure "shouldn't end with an error"
nothingChanges (Right (ops, store), _) = do
  ops   @?= []
  store @?= T.toVal storage

-- | Checking that you can't pay to youself more than you have.
selfOverdraft
  :: ContractPropValidator TStorage Expectation
selfOverdraft (result, _) = do
  shouldSatisfy result isLeft

-- | Checking that test payment was successful.
success
  :: ContractPropValidator TStorage Assertion
success (Left _, _) = do
  assertFailure "shouldn't end with an error"
success (Right (ops, store), _) = do
  ops @?= []
  store @?= T.toVal
    ( T.BigMap $ M.fromList
      [ (source, 190 :: Natural)
      , (foo,    10)
      , (bar,    300)
      ]
    , 500 :: Natural
    )

-- | Checking that paying of 0 creates a record of 0.
payNothing
  :: ContractPropValidator TStorage Assertion
payNothing (Left _, _) = do
  assertFailure "shouldn't end with an error"
payNothing (Right (ops, store), _) = do
  ops @?= []
  store @?= T.toVal
    ( T.BigMap $ M.fromList
      [ (source, 200 :: Natural)
      , (foo,    0)
      , (bar,    300)
      ]
    , 500 :: Natural
    )
