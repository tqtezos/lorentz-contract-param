-- | Convenient aliases for typed representation of Michelson types as
-- well as some re-exports.

module Lorentz.Type
  ( CT (..)
  , T (..)
  , Contract
  , type ( & )
  , ( # )
  , (:+>)
  , ToT
  , ToCT

  -- * Aliases for comparable types to avoid writing '
  , CInt
  , CNat
  , CString
  , CBytes
  , CMutez
  , CBool
  , CKeyHash
  , CTimestamp
  , CAddress

  -- * Aliases for comparable types to avoid using Tc
  , Tc
  , TInt
  , TNat
  , TString
  , TBytes
  , TMutez
  , TBool
  , TKeyHash
  , TTimestamp
  , TAddress

  -- * Aliases for non-comparable types to avoid writing '
  , TKey
  , TUnit
  , TSignature
  , TOption
  , TList
  , TSet
  , TOperation
  , TContract
  , TPair
  , TOr
  , TLambda
  , TMap
  , TBigMap
  ) where

import Michelson.Typed

type Tc = 'Tc

type CInt       = 'CInt
type CNat       = 'CNat
type CString    = 'CString
type CBytes     = 'CBytes
type CMutez     = 'CMutez
type CBool      = 'CBool
type CKeyHash   = 'CKeyHash
type CTimestamp = 'CTimestamp
type CAddress   = 'CAddress

type TInt       = 'Tc 'CInt
type TNat       = 'Tc 'CNat
type TString    = 'Tc 'CString
type TBytes     = 'Tc 'CBytes
type TMutez     = 'Tc 'CMutez
type TBool      = 'Tc 'CBool
type TKeyHash   = 'Tc 'CKeyHash
type TTimestamp = 'Tc 'CTimestamp
type TAddress   = 'Tc 'CAddress

type TKey       = 'TKey
type TUnit      = 'TUnit
type TSignature = 'TSignature
type TOption    = 'TOption
type TList      = 'TList
type TSet       = 'TSet
type TOperation = 'TOperation
type TContract  = 'TContract
type TPair      = 'TPair
type TOr        = 'TOr
type TLambda    = 'TLambda
type TMap       = 'TMap
type TBigMap    = 'TBigMap
