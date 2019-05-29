{- | Reimplementation of some syntax sugar.

You need the following module pragmas to make it work smoothly:

{-# LANGUAGE NoApplicativeDo, RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-}
module Lorentz.Rebinded
  ( (>>)
  , pure
  , return
  , ifThenElse
  , Condition (..)

    -- * Re-exports required for RebindableSyntax
  , fromInteger
  , fromString
  , fromLabel
  ) where


import Prelude hiding ((>>), (>>=))

import Lorentz.Arith
import Lorentz.Base
import Lorentz.Instr
import Lorentz.Macro
import Michelson.Typed.Arith

-- | Aliases for '(#)' used by do-blocks.
(>>) :: (a :-> b) -> (b :-> c) -> (a :-> c)
(>>) = (#)

-- | Predicate for @if ... then .. else ...@ construction,
-- defines a kind of operation applied to the top elements of the current stack.
data Condition st arg argl argr where
  Holds :: Condition s (Bool ': s) s s
  IsSome :: Condition s (Maybe a ': s) (a ': s) s
  IsNone :: Condition s (Maybe a ': s) s (a ': s)
  IsLeft :: Condition s (Either l r ': s) (l ': s) (r ': s)
  IsRight :: Condition s (Either l r ': s) (r ': s) (l ': s)
  IsCons :: Condition s ([a] ': s) (a ': [a] ': s) s
  IsNil :: Condition s ([a] ': s) s (a ': [a] ': s)

  IsZero :: (UnaryArithOpHs Eq' a, UnaryArithResHs Eq' a ~ Bool)
         => Condition s (a ': s) s s
  IsNotZero :: (UnaryArithOpHs Eq' a, UnaryArithResHs Eq' a ~ Bool)
         => Condition s (a ': s) s s

  IsEq :: IfCmpXConstraints a b Eq' => Condition s (a ': b ': s) s s
  IsNeq :: IfCmpXConstraints a b Neq => Condition s (a ': b ': s) s s
  IsLt :: IfCmpXConstraints a b Lt => Condition s (a ': b ': s) s s
  IsGt :: IfCmpXConstraints a b Gt => Condition s (a ': b ': s) s s
  IsLe :: IfCmpXConstraints a b Le => Condition s (a ': b ': s) s s
  IsGe :: IfCmpXConstraints a b Ge => Condition s (a ': b ': s) s s

-- | Defines semantics of @if ... then ... else ...@ construction.
ifThenElse
  :: Condition st arg argl argr
  -> (argl :-> o) -> (argr :-> o) -> (arg :-> o)
ifThenElse = \case
  Holds -> if_
  IsSome -> flip ifNone
  IsNone -> ifNone
  IsLeft -> ifLeft
  IsRight -> flip ifLeft
  IsCons -> ifCons
  IsNil -> flip ifCons

  IsZero -> \l r -> eq0 # if_ l r
  IsNotZero -> \l r -> eq0 # if_ r l

  IsEq -> ifEq
  IsNeq -> ifNeq
  IsLt -> ifLt
  IsGt -> ifGt
  IsLe -> ifLe
  IsGe -> ifGe
