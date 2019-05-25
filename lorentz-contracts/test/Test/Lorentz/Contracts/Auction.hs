-- | Module, containing spec to test auction.tz contract.
--
-- This spec is an example of using testing capabilities of morley.
module Test.Lorentz.Contracts.Auction
  ( spec_Auction
  ) where

import Test.Hspec (Spec, it, parallel, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, arbitrary, choose, counterexample, (.&&.), (===))
import Test.QuickCheck.Property (expectFailure, forAll, withMaxSuccess)

import Lorentz (compileLorentz)
import Lorentz.Contracts.Auction (contract_auction)
import Michelson.Interpret (ContractEnv(..))
import Michelson.Test (ContractPropValidator, contractProp, midTimestamp, specWithTypedContract)
import Michelson.Test.Dummy
import Michelson.Test.Util (failedProp)
import Michelson.Typed (CValue(..), Operation'(..), ToT, TransferTokens(..))
import qualified Michelson.Typed as T
import Tezos.Address (Address(..))
import Tezos.Core (Mutez, Timestamp, timestampPlusSeconds, unMutez, unsafeMkMutez, unsafeSubMutez)
import Tezos.Crypto (KeyHash)
import Util.Test.Arbitrary (runGen)

type Storage = (Timestamp, (Mutez, KeyHash))
type Param = KeyHash

-- | Spec to test auction.tz contract.
--
-- This spec serves as an example on how to test contract with both unit tests
-- and QuickCheck.
spec_Auction :: Spec
spec_Auction = parallel $ do
  specWithTypedContract "../contracts/auction.tz" auctionSpec'
  auctionSpec' (compileLorentz contract_auction)
  -- Test slightly modified version of auction.tz, it must fail.
  -- This block is given purely for demonstration of that tests are smart
  -- enough to filter common mistakes.
  specWithTypedContract "../contracts/auction_buggy.tz" $ \contract -> do
    prop "Random check (dense end of auction)" $
      expectFailure $ qcProp contract denseTime arbitrary

    prop "Random check (dense amount)" $
      expectFailure $ qcProp contract arbitrary denseAmount

  where
    -- Test auction.tz, everything should be fine
    auctionSpec' contract = do
      it "Bid after end of auction triggers failure" $
        contractProp contract
          (flip shouldSatisfy (isLeft . fst))
          (env { ceAmount = unsafeMkMutez 1200 })
          keyHash2
          (aBitBeforeMidTimestamp, (unsafeMkMutez 1000, keyHash1))

      prop "Random check (sparse distribution)" $ withMaxSuccess 200 $
        qcProp contract arbitrary arbitrary

      prop "Random check (dense end of auction)" $
        qcProp contract denseTime arbitrary

      prop "Random check (dense amount)" $
        qcProp contract arbitrary denseAmount

    qcProp contract eoaGen amountGen =
      forAll ((,) <$> eoaGen <*> ((,) <$> amountGen <*> arbitrary)) $
        \s p ->
          let validate = validateAuction env p s
           in contractProp contract validate env p s

    aBitBeforeMidTimestamp = midTimestamp `timestampPlusSeconds` -1
    -- ^ 1s before NOW

    denseTime = timestampPlusSeconds midTimestamp <$> choose (-4, 4)
    denseAmount = unsafeMkMutez . (midAmount +) . fromInteger <$> choose (-4, 4)

    env = dummyContractEnv
            { ceNow = midTimestamp
            , ceAmount = unsafeMkMutez midAmount
            }
    midAmount = unMutez (maxBound `unsafeSubMutez` minBound) `div` 2

keyHash1 :: KeyHash
keyHash1 = runGen 300406 arbitrary

keyHash2 :: KeyHash
keyHash2 = runGen 142917 arbitrary

-- | This validator checks the result of auction.tz execution.
--
-- It checks following properties:
--
-- * Current timestamp is before end of auction
-- * Amount of new bid is higher than previous one
--
-- In case of successful execution:
--
-- * End of auction timestamp in updated storage is unchanged
-- * Amount in updated storage is equal to @AMOUNT@ of transaction
-- * Key hash in updated storage is equal to contract's parameter
-- * Script returned exactly one operation, @TransferTokens@, which
--   returns money back to the previous bidder
validateAuction
  :: ContractEnv
  -> Param
  -> Storage
  -> ContractPropValidator (ToT Storage) Property
validateAuction env newKeyHash (endOfAuction, (amount, keyHash)) (resE, _)
  | ceNow env > endOfAuction
      = counterexample "Failure didn't trigger on end of auction" $ isLeft resE
  | ceAmount env <= amount
      = counterexample ("Failure didn't trigger on attempt to bid"
                        <> " with amount <= than previous bid") $ isLeft resE
  | Left e <- resE
      = failedProp $ "Unexpected script fail: " <> show e

  | Right (_, (T.VPair ( T.VC (CvTimestamp endOfAuction'), _))) <- resE
  , endOfAuction /= endOfAuction'
      = failedProp "End of auction timestamp of contract changed"

  | Right (_, (T.VPair (_, T.VPair (T.VC (CvMutez amount'), _)))) <- resE
  , amount' /= ceAmount env
      = failedProp $ "Storage updated to wrong value: new amount"
                      <> " is not equal to amount of transaction"
  | Right (_, (T.VPair (_, T.VPair (_, T.VC (CvKeyHash keyHash'))))) <- resE
  , keyHash' /= newKeyHash
      = failedProp $ "Storage updated to wrong value: new key hash"
                      <> " is not equal to contract's parameter"

  | Right (ops, _) <- resE
     = let counterE msg =
              counterexample $ "Invalid money back operation (" <> msg <> ")"
        in case ops of
            [OpTransferTokens (TransferTokens T.VUnit retAmount (T.VContract retAddr))] ->
              counterE "wrong amount" (retAmount === amount)
                .&&.
              counterE "wrong address" (KeyAddress keyHash === retAddr)
            _ -> failedProp $ "Unexpected operation list: " <> show ops
