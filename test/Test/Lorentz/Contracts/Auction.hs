module Test.Lorentz.Contracts.Auction
  ( auction
  ) where

import Lorentz
import Prelude hiding (swap)

type Parameter = TKeyHash

type AuctionEnd = TTimestamp
type Bid = TPair TMutez TKeyHash

type Storage = TPair AuctionEnd Bid

type Input = TPair Parameter Storage
type Output storage = TPair (TList TOperation) storage

auction :: Contract Parameter Storage
auction =
  checkIfAuctionHasEnded #
  setupReplacementStorage #
  checkNewBidIsGreater #
  getRefund #
  makeRefund #
  callingConvention

checkIfAuctionHasEnded :: '[ Input ] :+> '[ Input, TTimestamp ]
checkIfAuctionHasEnded = dup # cdar # dup # now # gt # if_ fail_ nop # swap

setupReplacementStorage :: '[ Input, TTimestamp] :+> '[ Bid, Storage ]
setupReplacementStorage =
  dup # car # dip cddr # amount # pair # swap # dip (swap # pair)

checkNewBidIsGreater :: '[ Bid, Storage ] :+> '[ Bid, Storage ]
checkNewBidIsGreater = dup # car # amount # le # if_ fail_ nop

getRefund :: '[ Bid, Storage ] :+> '[ TMutez, Bid, Storage ]
getRefund = dup # car

makeRefund :: '[ TMutez, Bid, Storage ] :+> '[ TOperation, Storage ]
makeRefund = dip (cdr # implicitAccount) # unit # transferTokens

callingConvention :: '[ TOperation, Storage ] :+> '[ Output Storage ]
callingConvention = nil # swap # cons # pair
