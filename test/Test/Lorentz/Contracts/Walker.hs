{-# LANGUAGE DeriveAnyClass, DerivingStrategies, NoApplicativeDo, RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Test.Lorentz.Contracts.Walker
  ( walkerContract
  , Parameter (..)
  , Storage (..)
  , Position (..)
  ) where

import Lorentz

type BoostParams = ("coef1" :! Integer, "coef2" :! Integer)
type IterationId = Natural
data Parameter
  = GoLeft
  | GoRight
  | GoUp
  | GoDown
  | Boost BoostParams
  | Reset IterationId
  deriving stock Generic
  deriving anyclass IsoValue

type Power = Integer
data Position = Position { x :: Integer, y :: Integer }
  deriving stock Generic
  deriving anyclass IsoValue
data Storage = Storage { pos :: Position, power :: Power, iterId :: IterationId }
  deriving stock Generic
  deriving anyclass IsoValue

walkerContract :: Contract Parameter Storage
walkerContract =
  unpair # caseT @Parameter
    ( #cGoLeft /-> do
        modify_ #pos $ modify_ #x $ do
          push @Integer 1
          rsub

    , #cGoRight /-> do
        modify_ #pos $ modify_ #x $ do
          push @Integer 1
          add

    , #cGoUp /-> do
        modify_ #pos $ modify_ #y $ do
          push @Integer 1
          add

    , #cGoDown /-> do
        modify_ #pos $ modify_ #y $ do
          push @Integer 1
          rsub

    , #cBoost /-> do
        dip (get_ #power)
        access_ #coef1
        add
        limitPower
        set_ #power

    , #cReset /-> do
        dip drop
        construct $
             fieldCtor (push Position{ x = 0, y = 0 })
          :& fieldCtor (push 0)
          :& fieldCtor dup
          :& RNil
        dip drop
    )
  # nil # pair
  where
    limitPower = do
      let maxPower = 100 :: Integer
      dup
      push maxPower
      if IsGt
        then nop
        else do drop; push maxPower
