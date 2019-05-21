{-# OPTIONS_GHC -Wno-orphans #-}

module Util.TypeTuple.Instances () where

import Util.TypeTuple.TH

concatMapM deriveRecFromTuple [0..15]
