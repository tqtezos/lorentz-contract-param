{-# LANGUAGE DeriveAnyClass #-}

module Test.TZip.FA1
  ( spec_FA1_contract
  ) where

import qualified Data.Map as M
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

import Lorentz (Address, View(..))
import Michelson.Interpret (ceAmount)
import Michelson.Runtime.GState (genesisAddress)
import Michelson.Test (contractProp, dummyContractEnv, specWithTypedContract)
import Michelson.Test.Unit
import qualified Michelson.Typed as T
import Tezos.Address
import Tezos.Core (Mutez)

-- | Runs all tests for FA1.
spec_FA1_contract :: Spec
spec_FA1_contract = do
  specWithTypedContract "contracts/FA1/FA1.mtz" $ \fa1 -> do
    describe "Transfer" $ do
      transfer fa1

    describe "GetTotalSupply" $ do
      getTotalSupply fa1

    describe "GetBalance" $ do
      getBalance fa1

data FA1Parameter'
  = Transfer       (Address, Natural)
  | GetTotalSupply (View () Natural)
  | GetBalance     (View Address (Maybe Natural))
  deriving stock Generic
  deriving anyclass T.IsoValue

type FA1Storage' =
  ( T.BigMap Address Natural
  , Natural
  )

type FA1Parameter = T.ToT FA1Parameter'
type FA1Storage   = T.ToT FA1Storage'

source, foo, bar, feather :: Address
source  = genesisAddress
foo     = mkContractAddressRaw "foo"
bar     = mkContractAddressRaw "bar"
feather = mkContractAddressRaw "feather"

transferredAmount :: Mutez
transferredAmount = ceAmount dummyContractEnv

storage :: FA1Storage'
storage =
  ( T.BigMap $ M.fromList
    [ (bar,    300)
    , (source, 200)
    ]
  , 500
  )

-- | A contract specification factory.
makeTestCase
  :: T.Contract FA1Parameter FA1Storage
  -> String
  -> ContractPropValidator FA1Storage Expectation
  -> FA1Parameter'
  -> Spec
makeTestCase contract name validator param =
  it name $ do
    contractProp contract validator dummyContractEnv param storage

-- | Testing various properties of `GetBalance`.
getBalance
  :: T.Contract FA1Parameter FA1Storage
  -> Spec
getBalance contract = do
  testCase
    "just gets balance for recorded address"
    gettingExistingBalance
    $ GetBalance (View bar (T.ContractAddr feather))

  testCase
    "just gets balance for missing address"
    gettingNonExistingBalance
    $ GetBalance (View foo (T.ContractAddr feather))
  where
    testCase = makeTestCase contract

-- | Testing properties of `GetBalance` when dest address is present.
gettingExistingBalance
  :: ContractPropValidator FA1Storage Expectation
gettingExistingBalance (Left _, _) = do
  expectationFailure "shouldn't end with an error"
gettingExistingBalance (Right (ops, store), _) = do
  store `shouldBe` T.toVal storage
  ops   `shouldBe`
    [ T.OpTransferTokens $
        T.TransferTokens
          { ttContractParameter = T.toVal
              ( bar
              , Just (Just (300 :: Natural))
              )
          , ttAmount   = transferredAmount
          , ttContract = T.VContract feather
        }
    ]

-- | Testing properties of `GetBalance` when dest address is NOT present.
gettingNonExistingBalance
  :: ContractPropValidator FA1Storage Expectation
gettingNonExistingBalance (Left _, _) = do
  expectationFailure "shouldn't end with an error"
gettingNonExistingBalance (Right (ops, store), _) = do
  store `shouldBe` T.toVal storage
  ops `shouldBe`
    [ T.OpTransferTokens $
        T.TransferTokens
          { ttContractParameter = T.toVal
              ( foo
              , Just (Nothing :: Maybe Natural)
              )
          , ttAmount   = transferredAmount
          , ttContract = T.VContract feather
          }
    ]

-- | Testing properties of `GetTotalSupply`.
getTotalSupply
  :: T.Contract FA1Parameter FA1Storage
  -> Spec
getTotalSupply contract = do
  testCase "getting" gettingTotalSupply
    $ GetTotalSupply (View () (T.ContractAddr feather))
  where
    testCase = makeTestCase contract

-- | Checking that `GetTotalSupply` doesn't change the storage
--   and returns total supply to the provided address.
gettingTotalSupply
  :: ContractPropValidator FA1Storage Expectation
gettingTotalSupply (Left _, _) = do
  expectationFailure "shouldn't end with an error"
gettingTotalSupply (Right (ops, store), _) = do
  store `shouldBe` T.toVal storage
  ops `shouldBe`
    [ T.OpTransferTokens $
        T.TransferTokens
          { ttContractParameter = T.toVal ((), Just (500 :: Natural))
          , ttAmount            = transferredAmount
          , ttContract          = T.VContract feather
        }
    ]

-- | Testing properties of `Transfer`.
transfer
  :: T.Contract FA1Parameter FA1Storage
  -> Spec
transfer contract = do
  testCase "paying"          success        $ Transfer (foo,    10)
  testCase "paying 0"        payNothing     $ Transfer (foo,    0)
  testCase "paying to self"  nothingChanges $ Transfer (source, 10)
  testCase "paying more than you have" overdraft $ Transfer (foo, 500)
  testCase "paying to self more than you have" selfOverdraft
    $ Transfer (source, 500)
  where
    testCase = makeTestCase contract

-- | Checking that paying more that you have fails.
overdraft
  :: ContractPropValidator FA1Storage Expectation
overdraft (result, _) = do
  shouldSatisfy result isLeft

-- | Checking that no changes to storage were made.
nothingChanges
  :: ContractPropValidator FA1Storage Expectation
nothingChanges (Left _, _) = do
  expectationFailure "shouldn't end with an error"
nothingChanges (Right (ops, store), _) = do
  ops   `shouldBe` []
  store `shouldBe` T.toVal storage

-- | Checking that you can't pay to youself more than you have.
selfOverdraft
  :: ContractPropValidator FA1Storage Expectation
selfOverdraft (result, _) = do
  shouldSatisfy result isLeft

-- | Checking that test payment was successful.
success
  :: ContractPropValidator FA1Storage Expectation
success (Left _, _) = do
  expectationFailure "shouldn't end with an error"
success (Right (ops, store), _) = do
  ops `shouldBe` []
  store `shouldBe` T.toVal
    ( T.BigMap $ M.fromList
      [ (source, 190 :: Natural)
      , (foo,    10)
      , (bar,    300)
      ]
    , 500 :: Natural
    )

-- | Checking that paying of 0 creates a record of 0.
payNothing
  :: ContractPropValidator FA1Storage Expectation
payNothing (Left _, _) = do
  expectationFailure "shouldn't end with an error"
payNothing (Right (ops, store), _) = do
  ops `shouldBe` []
  store `shouldBe` T.toVal
    ( T.BigMap $ M.fromList
      [ (source, 200 :: Natural)
      , (foo,    0)
      , (bar,    300)
      ]
    , 500 :: Natural
    )
