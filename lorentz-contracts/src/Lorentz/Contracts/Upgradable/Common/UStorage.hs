module Lorentz.Contracts.Upgradable.Common.UStorage
  ( getLocationOf
  , toUField
  , getUField
  , setUField
  , toElementOfBigMap
  , getElementOfBigMap
  , setElementOfBigMap
  ) where

import Lorentz

import Lorentz.Contracts.Upgradable.Common.Base

getLocationOf :: MText -> s :-> ByteString ': s
getLocationOf fieldName = do
  push ((), fieldName)
  pack

toUField ::
  forall a s. (KnownValue a, NoOperation a, NoBigMap a)
  => MText -> UStorage ': s :-> (Maybe a) ': s
toUField field = do
  getLocationOf field; get;
  if IsSome
  then unpack @a
  else none

getUField ::
  forall a s. (KnownValue a, NoOperation a, NoBigMap a)
  => MText -> UStorage ': s :-> (Maybe a) ': UStorage ': s
getUField field = dup # toUField @a field

setUField ::
  forall a s. (KnownValue a, NoOperation a, NoBigMap a)
  => MText -> a ': UStorage ': s :-> UStorage ': s
setUField field = do
  pack @a
  some
  getLocationOf field
  update

getLocationInBigMap
  :: forall kt s. (KnownValue kt, NoOperation kt, NoBigMap kt)
  => MText -> kt ': s :-> ByteString ': s
getLocationInBigMap bm = do
  dip (push bm)
  pair
  pack

toElementOfBigMap
  :: forall kt vt s.
  ( KnownValue kt, NoOperation kt, NoBigMap kt
  , KnownValue vt, NoOperation vt, NoBigMap vt
  )
  => MText -> kt ': UStorage ': s :-> (Maybe vt) ': s
toElementOfBigMap bm = do
  getLocationInBigMap bm; get
  if IsSome
  then unpack @vt
  else none

getElementOfBigMap
  :: forall kt vt s.
  ( KnownValue kt, NoOperation kt, NoBigMap kt
  , KnownValue vt, NoOperation vt, NoBigMap vt
  )
  => MText -> kt ': UStorage ': s :-> (Maybe vt) ': UStorage ': s
getElementOfBigMap bm = dip (dup) # toElementOfBigMap @kt @vt bm

setElementOfBigMap
  :: forall kt vt s.
  ( KnownValue kt, NoOperation kt, NoBigMap kt
  , KnownValue vt, NoOperation vt, NoBigMap vt
  )
  => MText -> kt ': (Maybe vt) ': UStorage ': s :-> UStorage ': s
setElementOfBigMap bm = do
  swap
  dip (getLocationInBigMap bm)
  if IsSome
  then pack @vt # some
  else none
  swap
  update
