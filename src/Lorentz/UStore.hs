{- | This module contains implementation of 'UStore'.

@UStore@ is essentially 'Lorentz.Store.Store' modified for the sake of
upgradeability.

In API it differs from @Store@ in the following ways:
1. It keeps both virtual @big_map@s and plain fields;
2. Neat conversion between Michelson and Haskell values
is implemented;
3. Regarding composabililty, one can operate with one @UStore@
and then lift it to a bigger one which includes the former.
This allows for simpler management of stores and clearer error messages.
In spite of this, operations with 'UStore's over deeply nested templates will
still work as before.

We represent 'UStore' as @big_map bytes bytes@.

* Plain fields are stored as
@key = pack fieldName; value = pack originalValue@.

* Virtual @big_map@s are kept as
@key = pack (bigMapName, originalKey); value = pack originalValue@.

-}
module Lorentz.UStore
  ( -- * UStore and related type definitions
    UStore
  , type (|~>)(..)
  , UStoreField (..)

    -- ** Type-lookup-by-name
  , GetUStoreKey
  , GetUStoreValue

    -- ** Instructions
  , ustoreMem
  , ustoreGet
  , ustoreUpdate
  , ustoreInsert
  , ustoreInsertNew
  , ustoreDelete

  , ustoreToField
  , ustoreGetField
  , ustoreSetField

    -- ** Instruction constraints
  , HasUStore

    -- * UStore composability
  , liftUStore
  , unliftUStore

    -- * UStore management from Haskell
  , UStoreConversible
  , mkUStore
  , ustoreDecompose
  , ustoreDecomposeFull
  ) where

import Lorentz.UStore.Types
import Lorentz.UStore.Instr
import Lorentz.UStore.Haskell
import Lorentz.UStore.Lift
