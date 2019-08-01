-- | Implementation of manalged ledger which does not require
-- particular storage type.

module Lorentz.Contracts.ManagedLedger.Impl
  ( StorageFieldsC

  , transfer
  , transfer'
  , approve
  , approve'
  , getAllowance
  , getBalance
  , getTotalSupply
  , setPause
  , setAdministrator
  , getAdministrator
  , mint
  , burn
  ) where

import Lorentz

import Lorentz.Contracts.ManagedLedger.Types

type StorageFieldsC fields =
  fields `HasFieldsOfType`
  [ "admin" := Address
  , "paused" := Bool
  , "totalSupply" := Natural
  ]

type Entrypoint param fields = '[ param, Storage' fields ] :-> ContractOut (Storage' fields)

----------------------------------------------------------------------------
-- Entrypoints
----------------------------------------------------------------------------

transfer
  :: forall fields. StorageFieldsC fields
  => Entrypoint TransferParams fields
transfer = do
  sender; toNamed #sender
  pair
  transfer'

-- | This is a slightly more general version of 'transfer' which is
-- needed to implement 'transferViaProxy'. It does not use 'sender'.
transfer'
  :: forall fields. StorageFieldsC fields
  => Entrypoint ("sender" :! Address, TransferParams) fields
transfer' = do
  dip ensureNotPaused
  dup; cdr; getField #to; dip (toField #from)
  stackType @[Address, Address, ("sender" :! Address, TransferParams), Storage' _]
  if IsEq
    then drop
    else do
      unpair
      -- Check whether we need to consider allowance
      stackType @["sender" :! Address, TransferParams, Storage' _]
      dip $ getField #from
      swap
      dip $ dup >> fromNamed #sender
      stackType @[Address, Address, "sender" :! Address, TransferParams, Storage' _]
      -- Consume allowance if necessary
      if IsEq
        then drop
        else dip dup >> swap >> dip @TransferParams consumeAllowance
      -- Perform transfer
      creditTo; debitFrom
      drop @TransferParams
  nil; pair

approve
  :: forall fields. StorageFieldsC fields
  => Entrypoint ApproveParams fields
approve = do
  sender; toNamed #sender
  approveParamsToAllowanceParams
  approve'

-- | This is a slightly more general version of 'approve' which is
-- needed to implement 'approveViaProxy'. It does not use 'sender'.
approve'
  :: forall fields. StorageFieldsC fields
  => Entrypoint AllowanceParams fields
approve' = do
  dip ensureNotPaused
  dupT @(Storage' fields); dupT @AllowanceParams; allowance
  dup; int
  if IsZero
    then drop
    else do duupX @2; toField #value; int
            ifEq0 drop (userFail #cUnsafeAllowanceChange)
  setAllowance; nil; pair

getAllowance
  :: StorageFieldsC fields
  => Entrypoint (View GetAllowanceParams Natural) fields
getAllowance = view_ (do unpair; allowance)

getBalance :: StorageFieldsC fields => Entrypoint (View Address Natural) fields
getBalance = view_ $ do
  unpair; dip ( toField #ledger ); get;
  ifSome (toField #balance) (push 0)

getTotalSupply :: StorageFieldsC fields => Entrypoint (View () Natural) fields
getTotalSupply = view_ (do cdr; toField #fields; toField #totalSupply)

setPause :: StorageFieldsC fields => Entrypoint Bool fields
setPause = do
  dip authorizeAdmin
  dip (getField #fields); setField #paused; setField #fields
  nil; pair

setAdministrator :: StorageFieldsC fields => Entrypoint Address fields
setAdministrator = do
  dip authorizeAdmin;
  stackType @[Address, Storage' _]
  dip (getField #fields); setField #admin; setField #fields
  nil; pair;

getAdministrator :: StorageFieldsC fields => Entrypoint (View () Address) fields
getAdministrator = view_ (do cdr; toField #fields; toField #admin)

mint :: StorageFieldsC fields => Entrypoint MintParams fields
mint = do
  dip authorizeAdmin
  creditTo
  drop @MintParams
  nil; pair

burn :: StorageFieldsC fields => Entrypoint BurnParams fields
burn = do
  dip authorizeAdmin
  debitFrom
  drop @BurnParams
  nil; pair

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

userFail :: forall name fieldTy s s'. FailUsingArg Error name fieldTy s s'
userFail = failUsingArg @Error @name

authorizeAdmin ::
  StorageFieldsC fields => Storage' fields : s :-> Storage' fields : s
authorizeAdmin = do
  getField #fields; toField #admin; sender; eq
  if_ nop (failUsing SenderIsNotAdmin)

addTotalSupply
  :: StorageFieldsC fields
  => Integer : Storage' fields : s :-> Storage' fields : s
addTotalSupply = do
  dip $ getField #fields >> getField #totalSupply
  add; isNat; ifSome nop (failUnexpected [mt|Negative total supply|])
  setField #totalSupply; setField #fields

debitFrom
  :: forall param fields.
     ( param `HasFieldsOfType` ["from" := Address, "value" := Natural]
     , StorageFieldsC fields
     )
  => '[param, Storage' fields] :-> '[param, Storage' fields]
debitFrom = do
    -- Get LedgerValue
    duupX @2; toField #ledger; duupX @2; toField #from
    get; ifSome nop $ do
      -- Fail if absent
      stackType @[param, Storage' _]
      toField #value; toNamed #required; push 0; toNamed #present
      swap; pair; userFail #cNotEnoughBalance
    -- Get balance
    stackType @[LedgerValue, param, Storage' _]
    getField #balance
    duupX @3; toField #value
    rsub; isNat
    ifSome nop $ do
      -- Fail if balance is not enough
      stackType @[LedgerValue, param, Storage' _]
      toField #balance; toNamed #present
      duupX @2; toField #value; toNamed #required
      pair; userFail #cNotEnoughBalance
    -- Update balance, LedgerValue and Storage
    setField #balance;
    duupX @2; dip $ do
      nonEmptyLedgerValue; swap; toField #from
      dip (dip $ getField #ledger)
      update; setField #ledger

    -- Update total supply
    dup; dip $ do toField #value; neg; addTotalSupply

creditTo
  :: ( param `HasFieldsOfType` ["to" := Address, "value" := Natural]
     , StorageFieldsC fields
     )
  => '[param, Storage' fields] :-> '[param, Storage' fields]
creditTo = do
    -- Get LedgerValue
    duupX @2; toField #ledger; duupX @2; toField #to
    get
    if IsSome
      then do -- Get balance
              duupX @2; toField #value; dip (getField #balance)
              add @Natural; setField #balance; some
      else do -- Construct LedgerValue (if not empty)
              getField #value; int
              ifEq0 none $ do
                constructT @LedgerValue
                  ( fieldCtor $ getField #value >> toNamed #balance
                  , fieldCtor $ emptyMap >> toNamed #approvals
                  )
                some
    -- Update LedgerValue and Storage
    swap
    dipX @2 (getField #ledger)
    dup; dip $ do toField #to; update; setField #ledger

    -- Update total supply
    dup; dip $ do toField #value; int; addTotalSupply

emptyLedgerValue :: s :-> LedgerValue : s
emptyLedgerValue =
  constructT @LedgerValue
    ( fieldCtor $ push 0 >> coerceWrap
    , fieldCtor $ emptyMap >> coerceWrap
    )

-- | Ensure that given 'LedgerValue' value cannot be safely removed
-- and return it.
nonEmptyLedgerValue :: LedgerValue : s :-> Maybe LedgerValue : s
nonEmptyLedgerValue = do
  getField #balance; int
  if IsNotZero
  then some
  else do
    getField #approvals
    size; int
    if IsNotZero
    then some
    else drop >> none

approveParamsToAllowanceParams ::
  "sender" :! Address : ApproveParams : s :-> AllowanceParams : s
approveParamsToAllowanceParams = do
  -- a hack for performance
  -- can be replaced with 'constructT' call if stops working
  pair; coerce_

allowance
  :: ( param `HasFieldsOfType` ["owner" := Address, "spender" := Address]
     , StorageFieldsC fields
     )
  => param ': Storage' fields ': s :-> Natural ': s
allowance = do
  dup; dip $ do
    toField #owner; dip (toField #ledger); get;
    ifSome (toField #approvals) emptyMap
  toField #spender; get; ifNone (push 0) nop

setAllowance
  :: StorageFieldsC fields
  => (AllowanceParams ': Storage' fields ': s) :-> (Storage' fields ': s)
setAllowance = do
  dip (getField #ledger); swap;
  dip (getField #owner); swap;
  get; ifNone (emptyLedgerValue >> emptyMap) (getField #approvals);
  stackType @(Map Address Natural : LedgerValue : AllowanceParams : Storage' _ : _)
  duupX @3; toField #value; nonZero
  duupX @4; toField #spender
  update; setField #approvals
  stackType @(LedgerValue : AllowanceParams : Storage' _ : _)
  some; swap
  toField #owner; dip (dip $ getField #ledger)
  update
  setField #ledger

consumeAllowance
  :: forall fields s. (StorageFieldsC fields)
  => ("sender" :! Address ': TransferParams ': Storage' fields ': s) :-> (Storage' fields ': s)
consumeAllowance = do
  -- Copy two topmost items
  duupX @2; duupX @2
  -- Get current allowance
  dipX @2 $ do
    stackType @("sender" :! Address ': TransferParams ': Storage' fields ': s)
    dipX @2 (dup @(Storage' fields))
    dip $ toField #from >> toNamed #owner
    fromNamed #sender >> toNamed #spender
    pair
    allowance
  stackType @("sender" :! Address ': TransferParams : Natural : Storage' fields : s)
  -- Construct value to be passed to 'setAllowance' and call it.
  constructT @AllowanceParams
    ( fieldCtor $ dip (getField #from >> toNamed #owner) >> swap
    , fieldCtor $ dup >> fromNamed #sender >> toNamed #spender
    , fieldCtor $ do
        dip $ do
          getField #value; duupX @3
          sub; isNat;
          if IsSome
            then nop
            else do duupX @2; toNamed #present
                    duupX @2; toField #value; toNamed #required
                    pair; userFail #cNotEnoughAllowance
          toNamed #value
        swap
    )
  dip $ drop @("sender" :! Address) >> drop @TransferParams >> drop @Natural
  setAllowance

ensureNotPaused ::
  StorageFieldsC fields => Storage' fields : s :-> Storage' fields : s
ensureNotPaused = do
  getField #fields; toField #paused
  if_ (failUsing OperationsArePaused) nop
