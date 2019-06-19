module Lorentz.Contracts.Walker
  ( walkerContract
  , Parameter (..)
  , Storage
  , StorageFields (..)
  , Position (..)
  , PowerUp (..)
  ) where

import Data.Default (Default(..))
import Util.Instances ()

import Lorentz

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

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

data StorageFields = StorageFields
  { pos :: Position
  , power :: Power
  , iterId :: IterationId
  } deriving stock Generic
    deriving anyclass (Default, IsoValue)

type Storage = StorageSkeleton StoreTemplate StorageFields

walkerContract :: Contract Parameter Storage
walkerContract =
  unpair # caseT @Parameter
    ( #cGoLeft /-> do
        modifyField #sFields $ modifyField #pos $ modifyField #x $ do
          push @Integer 1
          rsub
        applyCurrentPowerUps
        markCellVisited

    , #cGoRight /-> do
        modifyField #sFields $ modifyField #pos $ modifyField #x $ do
          push @Integer 1
          add
        applyCurrentPowerUps
        markCellVisited

    , #cGoUp /-> do
        modifyField #sFields $ modifyField #pos $ modifyField #y $ do
          push @Integer 1
          add
        applyCurrentPowerUps
        markCellVisited

    , #cGoDown /-> do
        modifyField #sFields $ modifyField #pos $ modifyField #y $ do
          push @Integer 1
          rsub
        applyCurrentPowerUps
        markCellVisited

    , #cBoost /-> do
        toField #coef1
        doBoost

    , #cReset /-> do
        construct @StorageFields $
             fieldCtor (push Position{ x = 0, y = 0 })
          :& fieldCtor (push 0)
          :& fieldCtor dup
          :& RNil
        dip drop
        construct $
             fieldCtor (do dip (dup @Storage); swap; toField #sMap)
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
  dip (do getField #sFields; getField #power)
  add
  limitPower
  setField #power; setField #sFields

applyPowerUp :: PowerUp : Storage : s :-> Storage : s
applyPowerUp = caseT
  ( #cBoostPowerUp /-> doBoost
  )

applyCurrentPowerUps :: Storage : s :-> Storage : s
applyCurrentPowerUps = do
  getField #sFields; toField #pos
  dip dup
  storageGet #cPowerUps
  if IsSome
    then applyPowerUp
    else nop

markCellVisited :: Storage : s :-> Storage : s
markCellVisited = do
  getField #sFields; toField #pos
  dip unit
  storageInsert #cVisited
