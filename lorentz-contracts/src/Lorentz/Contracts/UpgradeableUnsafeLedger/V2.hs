-- | The implementation of Unsafe ledger with V1 balance bug fixed

module Lorentz.Contracts.UpgradeableUnsafeLedger.V2
  ( migrate
  , version
  , unsafeLedgerContract
  ) where

import Lorentz

import Lorentz.Contracts.Upgradeable.Common
import qualified Lorentz.Contracts.UpgradeableUnsafeLedger.V1 as V1

version :: Natural
version = 2

-- The storage does not change
type UStoreV2 = V1.UStoreV1

type Interface = V1.Interface

-- | Storage migration function. Since the storage is the same,
--   there's nothing to migrate
migrate :: '[UStore_] :-> '[UStore_]
migrate = nop

-- | The second version of the UpgradeableUnsafeLedger.
--   Most of the functions are from V1 except for getBalance.
unsafeLedgerContract :: ContractCode
unsafeLedgerContract = do
  unpair
  coerce_ @UParameter @(UParam Interface)
  dip (coerce_ @UStore_ @UStoreV2)
  caseUParamT
    ( #transfer /-> V1.transfer
    , #getTotalSupply /-> V1.getTotalSupply
    , #getBalance /-> getBalance
    )
    uparamFallbackFail
  unpair
  dip (coerce_ @UStoreV2 @UStore_)
  pair

-- Note that the new getBalance returns the correct balance
getBalance :: '[View Address (Maybe Natural), UStoreV2]
           :-> '[([Operation], UStoreV2)]
getBalance = view_ (do unpair; ustoreGet #ledger)
