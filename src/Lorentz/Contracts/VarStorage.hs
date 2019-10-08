{-# OPTIONS -Wno-missing-export-lists #-}

module Lorentz.Contracts.VarStorage where

import Lorentz

-- | A contract storing (and accepting) a single variable
-- of given type
varStorageContract :: forall t. Contract t t
varStorageContract = do
  car
  nil
  pair

