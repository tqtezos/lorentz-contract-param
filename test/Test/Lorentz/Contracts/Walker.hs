{-# LANGUAGE DeriveAnyClass, DerivingStrategies #-}

module Test.Lorentz.Contracts.Walker
  ( walkerContract
  , Parameter (..)
  , Storage (..)
  , Position (..)
  ) where

import Prelude (Generic, Integer)

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
    ( #cGoLeft /->
        modify_ #pos
          ( modify_ #x
            ( push @Integer 1 # rsub
            )
          )
    , #cGoRight /->
        modify_ #pos
          ( modify_ #x
            ( push @Integer 1 # add
            )
          )
    , #cGoUp /->
        modify_ #pos
          ( modify_ #y
            ( push @Integer 1 # add
            )
          )
    , #cGoDown /->
        modify_ #pos
          ( modify_ #y
            ( push @Integer 1 # rsub
            )
          )
    , #cBoost /->
        ( dip (get_ #power)
        # access_ #coef1
        # add
        # set_ #power
        )
    )
  # nil # pair
