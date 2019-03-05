-- | 'TxData' type and associated functionality.

module Morley.Runtime.TxData
       ( TxData (..)
       ) where

import Michelson.Untyped (Op, Value)
import Morley.Types (NopInstr)
import Tezos.Address (Address)
import Tezos.Core (Mutez)

-- | Data associated with a particular transaction.
data TxData = TxData
  { tdSenderAddress :: !Address
  , tdParameter :: !(Value (Op NopInstr))
  , tdAmount :: !Mutez
  } deriving (Show)
