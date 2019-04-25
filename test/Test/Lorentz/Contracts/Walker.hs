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

walkerContract :: Contract (ToT Parameter) (ToT Storage)
walkerContract =
  unpair # caseT @Parameter
    ( #cGoLeft /->
        modify_ @Storage #pos
          ( modify_ @Position #x
            ( push (toVal @Integer 1) # rsub
            )
          )
    , #cGoRight /->
        modify_ @Storage #pos
          ( modify_ @Position #x
            ( push (toVal @Integer 1) # add
            )
          )
    , #cGoUp /->
        modify_ @Storage #pos
          ( modify_ @Position #y
            ( push (toVal @Integer 1) # add
            )
          )
    , #cGoDown /->
        modify_ @Storage #pos
          ( modify_ @Position #y
            ( push (toVal @Integer 1) # rsub
            )
          )
    , #cBoost /->
        ( dip (get_ @Storage #power)
        # access_ @BoostParams #coef1
        # add
        # set_ @Storage #power
        )
    )
  # nil # pair
