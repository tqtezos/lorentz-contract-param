-- | 'TxData' type and associated functionality.

module Michelson.Runtime.TxData
       ( TxData (..)
       ) where

import Michelson.Untyped (Value)
import Tezos.Address (Address)
import Tezos.Core (Mutez)

-- | Data associated with a particular transaction.
data TxData = TxData
  { tdSenderAddress :: !Address
  , tdParameter :: !Value
  , tdAmount :: !Mutez
  } deriving (Show, Eq)
