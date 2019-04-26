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
data Parameter
  = GoLeft
  | GoRight
  | GoUp
  | GoDown
  | Boost BoostParams
  deriving stock Generic
  deriving anyclass IsoValue

type Power = Integer
data Position = Position { x :: Integer, y :: Integer }
  deriving stock Generic
  deriving anyclass IsoValue
data Storage = Storage { pos :: Position, power :: Power }
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
        set_ #power
    )
  # nil # pair
