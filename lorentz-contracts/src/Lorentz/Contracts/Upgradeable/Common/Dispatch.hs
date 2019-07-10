module Lorentz.Contracts.Upgradeable.Common.Dispatch
  ( dispatch
  , ifArg
  ) where

import Lorentz
import Prelude (foldl')

import Lorentz.Contracts.Upgradeable.Common.Base

type Action a st = '[a, st] :-> '[([Operation], st)]
type DispatchStack = '[Maybe ([Operation], UStore_), (UParameter, UStore_)]

getParameter
  :: forall a st. (KnownValue a, NoOperation a, NoBigMap a)
  => MText
  -> '[UParameter, st] :-> '[Maybe a, st]
getParameter parameterName = do
  unpair
  push parameterName
  if IsEq
  then unpack @a
  else drop # none

ifArg
  :: forall a st. (KnownValue a, NoOperation a, NoBigMap a, Coercible_ UStore_ st)
  => MText
  -> Action a st
  -> DispatchStack :-> DispatchStack
ifArg parameterName action = do
  if IsSome   -- Maybe ([Operation], UStore_) : (UParameter, UStore_)
  then some   -- Maybe ([Operation], UStore_) : (UParameter, UStore_)
  else do     -- (UParameter, UStore_)
    dup       -- (UParameter, UStore_) : (UParameter, UStore_)
    dip $ do
      unpair    -- UParameter : UStorage
      getParameter @a parameterName -- Maybe a : UStore_
      if IsSome
      then do   -- a : UStore_
        dip $ coerce_ @UStore_ @st -- a : st
        action  -- ([Operation], st)
        unpair
        dip $ coerce_ @st @UStore_ -- ([Operation], UStore_)
        pair
        some
      else do   -- UStorage
        drop    --
        none    -- Maybe ([Operation], UStore_)
    swap

dispatch :: [DispatchStack :-> DispatchStack] -> ContractCode
dispatch ds = do
  foldl' (#) none ds
  assertSome [mt|Unknown argument|]
  dip drop
