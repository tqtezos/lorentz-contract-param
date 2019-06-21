-- | The implementation of Unsafe ledger with V1 balance bug fixed

module Lorentz.Contracts.UpgradableUnsafeLedger.V2
  ( migrate
  , unsafeLedgerContract
  ) where

import Lorentz

import Lorentz.UStore
import Lorentz.Contracts.Upgradable.Common
import qualified Lorentz.Contracts.UpgradableUnsafeLedger.V1 as V1

-- The storage does not change
type UStoreV2 = V1.UStoreV1

-- | Storage migration function. Since the storage is the same,
--   there's nothing to migrate
migrate :: '[UStore_] :-> '[UStore_]
migrate = nop

-- | The second version of the UpgradableUnsafeLedger.
--   Most of the functions are from V1 except for getBalance.
unsafeLedgerContract :: ContractCode
unsafeLedgerContract = do
  dispatch
    [ ifArg @V1.TransferParams [mt|Transfer|] V1.transfer
    , ifArg @(View () Natural) [mt|GetTotalSupply|] V1.getTotalSupply
    , ifArg @(View Address (Maybe Natural)) [mt|GetBalance|] getBalance
    ]

-- Note that the new getBalance returns the correct balance
getBalance :: '[View Address (Maybe Natural), UStoreV2]
           :-> '[([Operation], UStoreV2)]
getBalance = view_ (do unpair; ustoreGet #ledger)
