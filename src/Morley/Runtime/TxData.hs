-- | 'TxData' type and associated functionality.

module Morley.Runtime.TxData
       ( TxData (..)
       ) where

import Michelson.Types (Op, Value)
import Tezos.Core (Mutez)
import Tezos.Crypto (Address)

-- | Data associated with a particular transaction.
data TxData = TxData
  { tdSenderAddress :: !Address
  , tdParameter :: !(Value Op)
  , tdAmount :: !Mutez
  } deriving (Show)
