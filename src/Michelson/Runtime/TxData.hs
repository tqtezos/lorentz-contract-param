-- | 'TxData' type and associated functionality.

module Michelson.Runtime.TxData
       ( TxData (..)
       ) where

import Michelson.Untyped (Value)
import Morley.Types (ExpandedUExtInstr)
import Tezos.Address (Address)
import Tezos.Core (Mutez)

-- | Data associated with a particular transaction.
data TxData = TxData
  { tdSenderAddress :: !Address
  , tdParameter :: !Value
  , tdAmount :: !Mutez
  }

deriving instance Show ExpandedUExtInstr => Show TxData
