-- | Implementation of managed ledger compatible with FA1.2.1 which targets Athens.
-- Requires a proxy contract ('ManagedLedgerProxy').

module Lorentz.Contracts.ManagedLedger.Athens
  ( Parameter (..)

  , Storage
  , mkStorage
  , managedLedgerAthensContract
  , AthensError (..)
  ) where

import Lorentz
import Prelude (sum)

import Lorentz.Contracts.ManagedLedger.Types
import Lorentz.Contracts.ManagedLedger.Impl

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

----------------------------------------------------------------------------
-- Parameter
----------------------------------------------------------------------------

data Parameter
  = Transfer         !TransferParams
  | TransferViaProxy !("sender" :! Address, TransferParams)
  | Approve          !ApproveParams
  | ApproveViaProxy  !("sender" :! Address, ApproveParams)
  | GetAllowance     !(View GetAllowanceParams Natural)
  | GetBalance       !(View Address Natural)
  | GetTotalSupply   !(View () Natural)
  | SetPause         !Bool
  | SetAdministrator !Address
  | GetAdministrator !(View () Address)
  | Mint             !MintParams
  | Burn             !BurnParams
  | SetProxy         !Address
  deriving stock Generic
  deriving anyclass IsoValue

----------------------------------------------------------------------------
-- Storage
----------------------------------------------------------------------------

data StorageFields = StorageFields
  { admin       :: !Address
  , paused      :: !Bool
  , totalSupply :: !Natural
  , proxy       :: !(Either Address Address)
  -- ^ Left means that proxy is not set, it contains the address which
  -- is allowed to set proxy. Right means that proxy is set and
  -- contains its address.
  } deriving stock Generic
    deriving anyclass IsoValue

type Storage = Storage' StorageFields

-- | Create a default storage. In order to make this contract fully
-- usable administrator must call the 'SetProxy' entrypoint. Only
-- administrator can do it (though in principle we can accept two
-- addresses here and let someone else set proxy instead.
-- Balances can be set to non-zero values if they are passed in one of
-- the arguments. Approvals are all empty.
mkStorage :: Address -> Map Address Natural -> Storage
mkStorage adminAddress balances = mkStorage' balances $
  StorageFields
  { admin = adminAddress
  , paused = False
  , totalSupply = sum balances
  , proxy = Left adminAddress
  }

-- | Errors specific to this (i. e. compatible with Athens) implementation.
data AthensError
  = ProxyAlreadySet
  | NotAllowedToSetProxy
  | ProxyIsNotSet
  | CallerIsNotProxy
  deriving stock (Eq, Generic)

deriveCustomError ''AthensError

----------------------------------------------------------------------------
-- Implementation
----------------------------------------------------------------------------

managedLedgerAthensContract :: Contract Parameter Storage
managedLedgerAthensContract = do
  unpair
  caseT @Parameter
    ( #cTransfer /-> transfer
    , #cTransferViaProxy /-> do
        dip authorizeProxy
        transfer'
    , #cApprove /-> approve
    , #cApproveViaProxy /-> do
        dip authorizeProxy
        -- a hack for performance
        -- can be replaced with 'constructT' call if stops working
        coerce_ @("sender" :! Address, ApproveParams) @AllowanceParams
        approve'
    , #cGetAllowance /-> getAllowance
    , #cGetBalance /-> getBalance
    , #cGetTotalSupply /-> getTotalSupply
    , #cSetPause /-> setPause
    , #cSetAdministrator /-> setAdministrator
    , #cGetAdministrator /-> getAdministrator
    , #cMint /-> mint
    , #cBurn /-> burn
    , #cSetProxy /-> do
        -- Check sender
        dip $ do
          getField #fields
          toField #proxy
          ifLeft
            (do sender; if IsEq then nop else failUsing NotAllowedToSetProxy)
            (failUsing ProxyAlreadySet)
        right @Address
        dip $ getField #fields
        setField #proxy
        setField #fields
        nil; pair
    )

authorizeProxy :: Storage ': s :-> Storage ': s
authorizeProxy = do
  getField #fields
  toField #proxy
  ifLeft (failUsing ProxyIsNotSet) $ do
    sender
    if IsEq then nop else failUsing CallerIsNotProxy
