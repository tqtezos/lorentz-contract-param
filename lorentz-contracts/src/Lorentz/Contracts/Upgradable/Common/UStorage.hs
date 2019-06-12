module Lorentz.Contracts.Upgradable.Common.UStorage
  ( toUField
  , getUField
  , setUField
  ) where

import Lorentz

import Lorentz.Contracts.Upgradable.Common.Base

toUField ::
  forall a s. (KnownValue a, NoOperation a, NoBigMap a)
  => MText -> UStorage ': s :-> (Maybe a) ': s
toUField fieldName = do
  push fieldName; get;
  if IsSome
  then unpack @a
  else none

getUField ::
  forall a s. (KnownValue a, NoOperation a, NoBigMap a)
  => MText -> UStorage ': s :-> (Maybe a) ': UStorage ': s
getUField fieldName = dup # toUField @a fieldName

setUField ::
  forall a s. (KnownValue a, NoOperation a, NoBigMap a)
  => MText -> a ': UStorage ': s :-> UStorage ': s
setUField fieldName = do
  pack; some; push fieldName; update
