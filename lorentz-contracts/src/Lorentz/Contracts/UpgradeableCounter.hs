-- | UpgradeableCounter demonstrates the implementation of a simple contract
--   that has upgradeable storage, interface, and implementation.
--
--   In the first version it stores a Natural and allows to add some value
--   to it or multiply the current value by a certain natural number.
--
--   The second version changes the type of the stored value to Integer,
--   and instead of providing Mul Natural and Add Natural endpoints, it
--   just allows to increment or decrement the current value.
--
--   While the contract does not have any advanced functionality, it provides
--   a birds-eye view on all the aspects of the upgradeable contracts concept
--   and serves as an example on how to apply this concept.


module Lorentz.Contracts.UpgradeableCounter
  ( Parameter(..)
  , Storage
  , Error(..)
  , upgradeableCounterContract
  , mkEmptyStorage
  ) where

import Lorentz (Contract, EntryPointKind)

import Lorentz.Contracts.Upgradeable.Common

upgradeableCounterContract
  :: forall (interface :: [EntryPointKind]).
     Contract (Parameter interface) Storage
upgradeableCounterContract = upgradeableContract
