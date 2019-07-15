-- | Tests for optimizer.

module Test.Optimizer
  ( unit_Sample_optimize
  , unit_Sample_linearizeRight
  ) where

import Prelude hiding (EQ)

import Test.HUnit (Assertion, (@?=))

import Michelson.Optimizer (optimize, linearizeRight)
import Michelson.Text
import qualified Michelson.Typed as T
import Michelson.Typed.Instr
import Michelson.Untyped (CT(..))

unit_Sample_optimize :: Assertion
unit_Sample_optimize = optimize nonOptimal @?= expectedOptimized

-- Note: unfortunately this test will pass even if we change tree
-- structure in 'expectedRightLinear' because our `Eq` instance does
-- not account for that. Hopefully one day we will fix it (though
-- probably no). Anyway, at least we know that we don't do something
-- insane there.
unit_Sample_linearizeRight :: Assertion
unit_Sample_linearizeRight = linearizeRight nonOptimal @?= expectedRightLinear

str :: MText
str = [mt|aa|]

nonOptimal :: T.Contract ('T.Tc 'CString) ('T.Tc 'CString)
nonOptimal =
  CAR `Seq`
  -- `PUSH; DROP` is erased
  -- We also arbitrarily group two instructions here to make
  -- structure definitely non-linear.
  (PUSH (T.VC $ T.CvString str) `Seq` DROP) `Seq`
  -- If we PUSH and then DIP, DIP is not necessary
  PUSH (T.VC $ T.CvString str) `Seq`
  -- `DUP; DROP` is also erased
  DIP (DUP `Seq` DUP `Seq` DROP) `Seq`
  -- `SWAP; SWAP` is erased along with `DIP` containing it
  DIP (SWAP `Seq` SWAP) `Seq`
  CONCAT `Seq`
  SIZE `Seq`
  -- `COMPARE` with 0 is redundant
  PUSH (T.VC $ T.CvNat 0) `Seq` COMPARE `Seq` EQ `Seq`
  -- Here both bodys of `IF` can be erased and then `IF` can be replaced with `DROP`
  IF (DUP `Seq` DROP) (UNIT `Seq` DROP) `Seq`
  -- `LEFT` followed by `IF_LEFT` can be optimized
  LEFT @('T.TKey) `Seq` IF_LEFT Nop (UNIT `Seq` FAILWITH) `Seq`
  -- SWAP is redundant after DUP
  DUP `Seq` SWAP `Seq` CONCAT `Seq`
  NIL `Seq` PAIR

-- Auxiliary operator to produce right linear sequence.  We do not use
-- it above because input instruction can have arbitrary structure,
-- but we know that the output is right balanced. In practice we can't
-- check it though, because that's how our 'Eq' is defined.
(#<#) :: T.Instr a b -> T.Instr b c -> T.Instr a c
(#<#) = Seq
infixr 1 #<#

expectedRightLinear :: T.Contract ('T.Tc 'CString) ('T.Tc 'CString)
expectedRightLinear =
  CAR #<#
  PUSH (T.VC $ T.CvString str) #<# DROP #<#
  PUSH (T.VC $ T.CvString str) #<#
  DIP (DUP #<# DUP #<# DROP) #<#
  DIP (SWAP #<# SWAP) #<#
  CONCAT #<#
  SIZE #<#
  PUSH (T.VC $ T.CvNat 0) #<# COMPARE #<# EQ #<#
  IF (DUP #<# DROP) (UNIT #<# DROP) #<#
  LEFT @('T.TKey) #<# IF_LEFT Nop (UNIT #<# FAILWITH) #<#
  DUP #<# SWAP #<# CONCAT #<#
  NIL #<# PAIR

-- This is what we should get if we implement optimizer properly.
-- Currently it's in somewhat intermediate and experimental state.
_expectedOptimized :: T.Contract ('T.Tc 'CString) ('T.Tc 'CString)
_expectedOptimized =
  CAR #<#
  DUP #<#
  PUSH (T.VC $ T.CvString str) #<#
  CONCAT #<#
  SIZE #<#
  INT #<# EQ #<#
  DROP #<#
  DUP #<# CONCAT #<#
  NIL #<# PAIR

expectedOptimized :: T.Contract ('T.Tc 'CString) ('T.Tc 'CString)
expectedOptimized =
  CAR #<#
  DUP #<#
  PUSH (T.VC $ T.CvString str) #<#
  CONCAT #<#
  SIZE #<#
  INT #<# EQ #<#
  IF (DUP #<# DROP) (UNIT #<# DROP) #<# -- sad
  DUP #<# CONCAT #<#
  NIL #<# PAIR
