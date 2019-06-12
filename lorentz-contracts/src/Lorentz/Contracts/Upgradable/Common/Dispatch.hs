module Lorentz.Contracts.Upgradable.Common.Dispatch
  ( dispatch
  , ifArg
  ) where

import Prelude (foldl')
import Lorentz

import Lorentz.Contracts.Upgradable.Common.Base

type Action a = '[a, UStorage] :-> '[([Operation], UStorage)]
type DispatchStack = '[Maybe ([Operation], UStorage), (UParameter, UStorage)]

getParameter
  :: forall a. (KnownValue a, NoOperation a, NoBigMap a)
  => MText
  -> '[UParameter, UStorage] :-> '[Maybe a, UStorage]
getParameter parameterName = do
  unpair
  push parameterName
  if IsEq
  then unpack @a
  else drop # none

ifArg
  :: forall a. (KnownValue a, NoOperation a, NoBigMap a)
  => MText
  -> Action a
  -> DispatchStack :-> DispatchStack
ifArg parameterName action = do
  if IsSome   -- Maybe ([Operation], UStorage) : (UParameter, UStorage)
  then some   -- Maybe ([Operation], UStorage) : (UParameter, UStorage)
  else do     -- (UParameter, UStorage)
    dup       -- (UParameter, UStorage) : (UParameter, UStorage)
    dip $ do
      unpair    -- UParameter : UStorage
      getParameter @a parameterName -- Maybe Natural : UStorage
      if IsSome
      then do   -- Natural : UStorage
        action  -- ([Operation], UStorage)
        some
      else do   -- UStorage
        drop    --
        none    -- Maybe ([Operation], UStorage)
    swap

dispatch :: [DispatchStack :-> DispatchStack] -> ContractCode
dispatch ds = do
  foldl' (#) none ds
  assertSome [mt|Unknown argument|]
  dip drop
