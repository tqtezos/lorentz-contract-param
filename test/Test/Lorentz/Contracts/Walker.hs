{-# LANGUAGE DeriveAnyClass, DerivingStrategies, NoApplicativeDo, RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Test.Lorentz.Contracts.Walker
  ( walkerContract
  , Parameter (..)
  , Storage (..)
  , Position (..)
  , PowerUp (..)
  ) where

import Data.Default (Default(..))
import Util.Instances ()

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
  deriving stock (Eq, Generic)
  deriving anyclass (Default, IsoValue)

data PowerUp
  = BoostPowerUp Integer
  deriving stock Generic
  deriving anyclass IsoValue

data StoreTemplate
  = PowerUps (Position |-> PowerUp)
  | Visited (Position |-> ())
  deriving stock Generic
  deriving anyclass IsoValue

data Storage = Storage
  { world :: Store StoreTemplate
  , pos :: Position
  , power :: Power
  , iterId :: IterationId
  } deriving stock Generic
    deriving anyclass (Default, IsoValue)

walkerContract :: Contract Parameter Storage
walkerContract =
  unpair # caseT @Parameter
    ( #cGoLeft /-> do
        modify_ #pos $ modify_ #x $ do
          push @Integer 1
          rsub
        applyCurrentPowerUps
        markCellVisited

    , #cGoRight /-> do
        modify_ #pos $ modify_ #x $ do
          push @Integer 1
          add
        applyCurrentPowerUps
        markCellVisited

    , #cGoUp /-> do
        modify_ #pos $ modify_ #y $ do
          push @Integer 1
          add
        applyCurrentPowerUps
        markCellVisited

    , #cGoDown /-> do
        modify_ #pos $ modify_ #y $ do
          push @Integer 1
          rsub
        applyCurrentPowerUps
        markCellVisited

    , #cBoost /-> do
        access_ #coef1
        doBoost

    , #cReset /-> do
        construct $
             fieldCtor (do dip (dup @Storage); swap; access_ #world)
          :& fieldCtor (push Position{ x = 0, y = 0 })
          :& fieldCtor (push 0)
          :& fieldCtor dup
          :& RNil
        dip $ drop >> drop
    )
  # nil # pair

limitPower :: Integer : s :-> Integer : s
limitPower = do
  dup
  push maxPower
  if IsGt
    then nop
    else do drop; push maxPower
  where
    maxPower = 100 :: Integer

doBoost :: Integer : Storage : s :-> Storage : s
doBoost = do
  dip (get_ #power)
  add
  limitPower
  set_ #power

applyPowerUp :: PowerUp : Storage : s :-> Storage : s
applyPowerUp = caseT
  ( #cBoostPowerUp /-> doBoost
  )

applyCurrentPowerUps :: Storage : s :-> Storage : s
applyCurrentPowerUps = do
  get_ #pos
  dip $ get_ #world
  storeGet #cPowerUps
  if IsSome
    then applyPowerUp
    else nop

markCellVisited :: Storage : s :-> Storage : s
markCellVisited = do
  get_ #pos
  dip $ get_ #world
  dip unit
  storeInsert #cVisited
  set_ #world
