module Lorentz.Contracts.Auction
  ( contract_auction
  ) where

import Lorentz

type Parameter = KeyHash

type AuctionEnd = Timestamp
type Bid = (Mutez, KeyHash)

type Storage = (AuctionEnd, Bid)

type Input = (Parameter, Storage)
type Output storage = ([Operation], storage)

contract_auction :: Contract Parameter Storage
contract_auction = do
  checkIfAuctionHasEnded
  setupReplacementStorage
  checkNewBidIsGreater
  getRefund
  makeRefund
  callingConvention

checkIfAuctionHasEnded :: '[ Input ] :-> '[ Input, Timestamp ]
checkIfAuctionHasEnded = do
  dup; cdar; dup; now
  assertLe "Auction has ended"
  swap

setupReplacementStorage :: '[ Input, Timestamp] :-> '[ Bid, Storage ]
setupReplacementStorage =
  do dup; car; dip cddr; amount; pair; swap; dip (swap # pair)

checkNewBidIsGreater :: '[ Bid, Storage ] :-> '[ Bid, Storage ]
checkNewBidIsGreater = do
  dup; car; amount
  assertGt "New big must be greater than the greatest one"

getRefund :: '[ Bid, Storage ] :-> '[ Mutez, Bid, Storage ]
getRefund = do dup; car

makeRefund :: '[ Mutez, Bid, Storage ] :-> '[ Operation, Storage ]
makeRefund = do dip (cdr # implicitAccount); unit; transferTokens

callingConvention :: '[ Operation, Storage ] :-> '[ Output Storage ]
callingConvention = do nil; swap; cons; pair
