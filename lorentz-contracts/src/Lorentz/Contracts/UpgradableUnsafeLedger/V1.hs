-- | A buggy implementation of Unsafe ledger, returns balances multiplied by 2

module Lorentz.Contracts.UpgradableUnsafeLedger.V1
  ( UpgradableStorageSkeleton(..)
  , UpgradableInterfaceSkeleton(..)
  , migrate
  , unsafeLedgerContract

  -- The following are used in V2
  , TransferParams
  , transfer
  , getTotalSupply
  , toElementOfBigMap_ledger
  ) where

import Lorentz

import Lorentz.Contracts.Upgradable.Common

-- Currently ignored
data UpgradableInterfaceSkeleton
  = Transfer TransferParams
  | GetTotalSupply (View () Natural)
  | GetBalance     (View Address (Maybe Natural))

-- We lose arg names here but I believe it's possible to bring them back
-- in generic implementation
type TransferParams = (Address, Natural)

-- Currently ignored
data UpgradableStorageSkeleton = Storage
  { ledger      :: BigMap Address Natural
  , totalSupply :: Natural
  }

toElementOfBigMap_ledger
  :: Address ': UStorage ': s
  :-> Maybe Natural ': s
toElementOfBigMap_ledger = toElementOfBigMap @Address @Natural [mt|ledger|]

getElementOfBigMap_ledger
  :: Address ': UStorage ': s
  :-> Maybe Natural ': UStorage ': s
getElementOfBigMap_ledger = getElementOfBigMap @Address @Natural [mt|ledger|]

setElementOfBigMap_ledger
  :: Address ': Maybe Natural ': UStorage ': s
  :-> UStorage ': s
setElementOfBigMap_ledger = setElementOfBigMap @Address @Natural [mt|ledger|]

toUField_totalSupply :: UStorage ': s :-> Natural ': s
toUField_totalSupply = do
  toUField @Natural [mt|#totalSupply|]
  assertSome [mt|Inconsistent storage|]

setUField_totalSupply :: Natural ': UStorage ': s :-> UStorage ': s
setUField_totalSupply = setUField @Natural [mt|#totalSupply|]

migrate :: '[UStorage] :-> '[UStorage]
migrate = do
  push @Natural 500
  dup
  dip $ do
    some
    sender
    setElementOfBigMap_ledger
  setUField_totalSupply

unsafeLedgerContract :: ContractCode
unsafeLedgerContract = do
  dispatch
    [ ifArg @TransferParams [mt|Transfer|] transfer
    , ifArg @(View () Natural) [mt|GetTotalSupply|] getTotalSupply
    , ifArg @(View Address (Maybe Natural)) [mt|GetBalance|] buggyGetBalance
    ]

transfer :: '[TransferParams, UStorage]
         :-> '[([Operation], UStorage)]
transfer = do
  debitSource; creditTo; nil; pair;

getTotalSupply :: '[View () Natural, UStorage]
               :-> '[([Operation], UStorage)]
getTotalSupply = view_ (do cdr; toUField_totalSupply)

-- Buggy getBalance returns balance multiplied by 2
buggyGetBalance :: '[View Address (Maybe Natural), UStorage]
                :-> '[([Operation], UStorage)]
buggyGetBalance = view_ $ do
  unpair
  toElementOfBigMap_ledger
  if IsSome
  then push @Natural 2 >> mul >> some
  else none

debitSource :: '[TransferParams, UStorage]
            :-> '[TransferParams, UStorage]
debitSource = do
  dip $ do
    sender
    getElementOfBigMap_ledger
    assertSome [mt|Sender address is not in ledger|]
  swap
  dip (dup # cdr)
  subGt0
  swap
  dip (do sender; setElementOfBigMap_ledger)

creditTo :: '[TransferParams, UStorage] :-> '[UStorage]
creditTo = do
  dup; car
  swap
  dip (getElementOfBigMap_ledger)
  swap
  if IsSome then dip (dup >> cdr) >> add @Natural else (dup >> cdr)
  some
  dip (car)
  swap
  setElementOfBigMap_ledger

subGt0 :: Natural ': Natural ': s :-> Maybe Natural ': s
subGt0 = do
  sub;
  dup; assertGe0 [mt|Transferred value is greater than balance|]
  dup; eq0
  if Holds
  then drop >> none
  else isNat
