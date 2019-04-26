module Test.Lorentz.Contracts.Auction
  ( auction
  ) where

import Lorentz
import Prelude hiding (swap)

type Parameter = KeyHash

type AuctionEnd = Timestamp
type Bid = (Mutez, KeyHash)

type Storage = (AuctionEnd, Bid)

type Input = (Parameter, Storage)
type Output storage = ([Operation], storage)

auction :: Contract Parameter Storage
auction =
  checkIfAuctionHasEnded #
  setupReplacementStorage #
  checkNewBidIsGreater #
  getRefund #
  makeRefund #
  callingConvention

checkIfAuctionHasEnded :: '[ Input ] :-> '[ Input, Timestamp ]
checkIfAuctionHasEnded = dup # cdar # dup # now # gt # if_ fail_ nop # swap

setupReplacementStorage :: '[ Input, Timestamp] :-> '[ Bid, Storage ]
setupReplacementStorage =
  dup # car # dip cddr # amount # pair # swap # dip (swap # pair)

checkNewBidIsGreater :: '[ Bid, Storage ] :-> '[ Bid, Storage ]
checkNewBidIsGreater = dup # car # amount # le # if_ fail_ nop

getRefund :: '[ Bid, Storage ] :-> '[ Mutez, Bid, Storage ]
getRefund = dup # car

makeRefund :: '[ Mutez, Bid, Storage ] :-> '[ Operation, Storage ]
makeRefund = dip (cdr # implicitAccount) # unit # transferTokens

callingConvention :: '[ Operation, Storage ] :-> '[ Output Storage ]
callingConvention = nil # swap # cons # pair
