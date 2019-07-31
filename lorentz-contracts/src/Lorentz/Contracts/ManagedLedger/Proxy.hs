-- | Proxy contract to be used with ManagedLedgerAthens.
-- See FA1.2.1.

module Lorentz.Contracts.ManagedLedger.Proxy
  ( Parameter (..)
  , SaneParameter (..)
  , fromSaneParameter

  , Storage
  , managedLedgerProxyContract
  ) where

import Lorentz

import qualified Lorentz.Contracts.ManagedLedger.Athens as Athens
import Lorentz.Contracts.ManagedLedger.Types

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

----------------------------------------------------------------------------
-- Parameter
----------------------------------------------------------------------------

-- We have to manually assemble linear parameter type, because Lorentz
-- uses balanced trees by default.

data Parameter0
  = Approve        !ApproveParams
  | Parameter1     !Parameter1
  deriving stock Generic
  deriving anyclass IsoValue

data Parameter1
  = GetAllowance   !(View GetAllowanceParams Natural)
  | Parameter2     !Parameter2
  deriving stock Generic
  deriving anyclass IsoValue

data Parameter2
  = GetBalance     !(View Address Natural)
  | GetTotalSupply !(View () Natural)
  deriving stock Generic
  deriving anyclass IsoValue

data Parameter
  = Transfer       !TransferParams
  | Parameter0     !Parameter0
  deriving stock Generic
  deriving anyclass IsoValue

-- | Sane representation of parameter which we currently can't use
-- because it's Michelson representation is a balanced tree, while we
-- need to be compatible with FA1.2 which requires linear structure.
data SaneParameter
  = STransfer       !TransferParams
  | SApprove        !ApproveParams
  | SGetAllowance   !(View GetAllowanceParams Natural)
  | SGetBalance     !(View Address Natural)
  | SGetTotalSupply !(View () Natural)

-- | Convert sane parameter to parameter of this contract.
fromSaneParameter :: SaneParameter -> Parameter
fromSaneParameter =
  \case
    STransfer tp -> Transfer tp
    SApprove ap ->
      Parameter0 $
      Approve ap
    SGetAllowance v ->
      Parameter0 $
      Parameter1 $
      GetAllowance v
    SGetTotalSupply v ->
      Parameter0 $
      Parameter1 $
      Parameter2 $
      GetTotalSupply v
    SGetBalance v ->
      Parameter0 $
      Parameter1 $
      Parameter2 $
      GetBalance v

type Storage = ContractAddr Athens.Parameter

managedLedgerProxyContract :: Contract Parameter Storage
managedLedgerProxyContract = do
  unpair
  caseT @Parameter
    ( #cTransfer /-> do
        sender; toNamed #sender
        pair
        wrap_ #cTransferViaProxy
        callAthens
    , #cParameter0 /-> caseT @Parameter0
      ( #cApprove /-> do
          sender; toNamed #sender
          pair
          wrap_ #cApproveViaProxy
          callAthens
      , #cParameter1 /-> caseT @Parameter1
        ( #cGetAllowance /-> wrap_ #cGetAllowance >> callAthens
        , #cParameter2 /-> caseT @Parameter2
          ( #cGetBalance /-> wrap_ #cGetBalance >> callAthens
          , #cGetTotalSupply /-> wrap_ #cGetTotalSupply >> callAthens
          )
        )
      )
    )

callAthens :: '[ Athens.Parameter, Storage ] :-> ContractOut Storage
callAthens = do
  dip $ dup >> amount
  transferTokens; nil; swap; cons
  pair
