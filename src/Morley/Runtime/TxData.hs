-- | 'TxData' type and associated functionality.

module Morley.Runtime.TxData
       ( TxData (..)
       ) where

import Michelson.Untyped (UntypedValue)
import Morley.Types (ExpandedUExtInstr)
import Tezos.Address (Address)
import Tezos.Core (Mutez)

-- | Data associated with a particular transaction.
data TxData = TxData
  { tdSenderAddress :: !Address
  , tdParameter :: !UntypedValue
  , tdAmount :: !Mutez
  }

deriving instance Show ExpandedUExtInstr => Show TxData
