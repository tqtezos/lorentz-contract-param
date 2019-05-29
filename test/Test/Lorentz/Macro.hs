-- | Tests for Lorentz macros.
--
-- They test logic of macros and type-level logic.  Also they serve as
-- examples of using complex macros (e. g. parameterized with
-- type-level numbers)

module Test.Lorentz.Macro
  ( unit_dropX
  , unit_elevateX
  ) where

import Prelude hiding (drop, swap)
import Test.HUnit (Assertion, (@?=))

import Lorentz

unit_dropX :: Assertion
unit_dropX = do
  dropX @0 @?= drop
  dropX @2 @?= dropX2
  where
    dropX2 :: [Integer, Bool, (), Bool] :-> [Integer, Bool, Bool]
    dropX2 = dip $ dip drop

unit_elevateX :: Assertion
unit_elevateX = do
  elevateX @0 @?= (dup # dip drop) -- current implementation is not efficient
  elevateX @1 @?= (dip dup # swap # dip (dip drop))
  elevateX @3 @?= elevateX3
  where
    elevateX3 :: [Bool, Integer, Natural, (), Bool] :-> [(), Bool, Integer, Natural, Bool]
    elevateX3 = duupX @4 # dropX @4
