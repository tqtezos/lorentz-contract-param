-- | Module, containing spec to test auction.tz contract.
--
-- This spec is an example of using testing capabilities of morley.
module Test.Interpreter.Auction
  ( auctionSpec
  ) where

import Test.Hspec (Spec, it, parallel, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, arbitrary, choose, counterexample, (.&&.), (===))
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Property (expectFailure, forAll, withMaxSuccess)
import Test.QuickCheck.Random (mkQCGen)

import Michelson.Interpret (ContractEnv(..))
import Michelson.Typed
  (CT(..), CVal(..), Operation(..), ToT, TransferTokens(..), Val(..), fromVal, toVal)
import Morley.Test (ContractPropValidator, contractProp, midTimestamp, specWithTypedContract)
import Morley.Test.Util (failedProp)
import Tezos.Address (Address(..))
import Tezos.Core (Mutez, Timestamp, timestampPlusSeconds, unMutez, unsafeMkMutez, unsafeSubMutez)
import Tezos.Crypto (KeyHash)

import Test.Util.Interpreter (dummyContractEnv)

type Storage = (Timestamp, (Mutez, KeyHash))
type Param = KeyHash
type ContractParam instr = Val instr (ToT Param)
type ContractStorage instr = Val instr (ToT Storage)

-- | Spec to test auction.tz contract.
--
-- This spec serves as an example on how to test contract with both unit tests
-- and QuickCheck.
auctionSpec :: Spec
auctionSpec = parallel $ do
  -- Test auction.tz, everything should be fine
  specWithTypedContract "contracts/auction.tz" $ \contract -> do
    it "Bid after end of auction triggers failure" $
      contractProp contract
        (\_ _ _ -> flip shouldSatisfy (isLeft . fst))
        (env { ceAmount = unsafeMkMutez 1200 })
        (mkParam keyHash2)
        (toVal (aBitBeforeMidTimestamp, (unsafeMkMutez 1000, keyHash1)))

    prop "Random check (sparse distribution)" $ withMaxSuccess 200 $
      qcProp contract arbitrary arbitrary

    prop "Random check (dense end of auction)" $
      qcProp contract denseTime arbitrary

    prop "Random check (dense amount)" $
      qcProp contract arbitrary denseAmount

  -- Test slightly modified version of auction.tz, it must fail.
  -- This block is given purely for demonstration of that tests are smart
  -- enough to filter common mistakes.
  specWithTypedContract "contracts/auction-buggy.tz" $ \contract -> do
    prop "Random check (dense end of auction)" $
      expectFailure $ qcProp contract denseTime arbitrary

    prop "Random check (dense amount)" $
      expectFailure $ qcProp contract arbitrary denseAmount

  where
    qcProp contract eoaGen amountGen =
      forAll ((,) <$> eoaGen <*> ((,) <$> amountGen <*> arbitrary)) $
        \s' p' ->
          contractProp contract validateAuction env (mkParam p') (mkStorage s')

    aBitBeforeMidTimestamp = midTimestamp `timestampPlusSeconds` -1
    -- ^ 1s before NOW

    -- N.B.: using Gen (CVal 'T_timestamp) from Morley.Test.
    denseTime = CvTimestamp . (timestampPlusSeconds midTimestamp) <$> choose (-4, 4)
    denseAmount = unsafeMkMutez . (midAmount +) . fromInteger <$> choose (-4, 4)

    env = dummyContractEnv
            { ceNow = midTimestamp
            , ceAmount = unsafeMkMutez midAmount
            }
    midAmount = unMutez (maxBound `unsafeSubMutez` minBound) `div` 2

    mkParam :: Param -> ContractParam instr
    mkParam = toVal

    mkStorage :: (CVal 'T_timestamp, (Mutez, KeyHash)) -> ContractStorage instr
    mkStorage (CvTimestamp t, b) = toVal (t, b)

keyHash1 :: KeyHash
keyHash1 = unGen arbitrary (mkQCGen 300406) 0

keyHash2 :: KeyHash
keyHash2 = unGen arbitrary (mkQCGen 142917) 0

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
  :: ContractPropValidator (ToT Param) (ToT Storage) Property
validateAuction env
    (fromVal -> newKeyHash)
    (fromVal -> (endOfAuction, (amount, keyHash :: KeyHash)))
    (resE, _)

  | ceNow env > endOfAuction
      = counterexample "Failure didn't trigger on end of auction" $ isLeft resE
  | ceAmount env <= amount
      = counterexample ("Failure didn't trigger on attempt to bid"
                        <> " with amount <= than previous bid") $ isLeft resE
  | Left e <- resE
      = failedProp $ "Unexpected script fail: " <> show e

  | Right (_, (VPair ( VC (CvTimestamp endOfAuction'), _))) <- resE
  , endOfAuction /= endOfAuction'
      = failedProp "End of auction timestamp of contract changed"

  | Right (_, (VPair (_, VPair (VC (CvMutez amount'), _)))) <- resE
  , amount' /= ceAmount env
      = failedProp $ "Storage updated to wrong value: new amount"
                      <> " is not equal to amount of transaction"
  | Right (_, (VPair (_, VPair (_, VC (CvKeyHash keyHash'))))) <- resE
  , keyHash' /= newKeyHash
      = failedProp $ "Storage updated to wrong value: new key hash"
                      <> " is not equal to contract's parameter"

  | Right (ops, _) <- resE
     = let counterE msg =
              counterexample $ "Invalid money back operation (" <> msg <> ")"
        in case ops of
            OpTransferTokens (TransferTokens VUnit retAmount (VContract retAddr)) : [] ->
              counterE "wrong amount" (retAmount === amount)
                .&&.
              counterE "wrong address" (KeyAddress keyHash === retAddr)
            _ -> failedProp $ "Unexpected operation list: " <> show ops
