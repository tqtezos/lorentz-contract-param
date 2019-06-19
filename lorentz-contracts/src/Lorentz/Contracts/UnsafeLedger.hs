-- | Unsafe ledger implementation as described in TZIP-FA1.

module Lorentz.Contracts.UnsafeLedger
  ( Parameter (..)
  , Storage (..)
  , unsafeLedgerContract
  ) where

import Lorentz

data Parameter
  = Transfer TransferParams
  | GetTotalSupply (View () Natural)
  | GetBalance     (View Address (Maybe Natural))
  deriving stock Generic
  deriving anyclass IsoValue

type TransferParams = ("to" :! Address, "val" :! Natural)
data Storage = Storage
  { ledger      :: BigMap Address Natural
  , totalSupply :: Natural
  }
  deriving stock Generic
  deriving anyclass IsoValue

unsafeLedgerContract :: Contract Parameter Storage
unsafeLedgerContract = do
  unpair
  caseT @Parameter
    ( #cTransfer /-> do debitSource; creditTo; nil; pair;
    , #cGetTotalSupply /-> view_ (do cdr; toField #totalSupply)
    , #cGetBalance /-> view_ (do unpair; dip ( toField #ledger ); get)
    )

debitSource :: '[TransferParams, Storage]
            :-> '[TransferParams, Storage]
debitSource = do
  dip (do getField #ledger; sender; get; assertSome [mt|Sender address is not in ledger|])
  swap
  dip (getField #val);
  subGt0
  swap;
  dip (do dip (getField #ledger); sender; update; setField #ledger)

creditTo :: '[TransferParams, Storage] :-> '[Storage]
creditTo = do
  getField #to
  swap
  dip (do dip (getField #ledger); get)
  swap
  if IsSome then dip (getField #val) >> add @Natural else getField #val
  some
  dip (toField #to)
  swap;
  dipX @2 (getField #ledger)
  update;
  setField #ledger

subGt0 :: Natural ': Natural ': s :-> Maybe Natural ': s
subGt0 = do
  sub;
  dup; assertGe0 [mt|Transferred value is greater than balance|]
  dup; eq0
  if Holds
  then drop >> none
  else isNat
