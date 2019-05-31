-- | 'TxData' type and associated functionality.

module Michelson.Runtime.TxData
       ( TxData (..)
       , tdSenderAddressL
       , tdParameterL
       , tdAmountL
       ) where

import Control.Lens (makeLensesWith)

import Michelson.Untyped (Value)
import Tezos.Address (Address)
import Tezos.Core (Mutez)
import Util.Lens (postfixLFields)

-- | Data associated with a particular transaction.
data TxData = TxData
  { tdSenderAddress :: !Address
  , tdParameter :: !Value
  , tdAmount :: !Mutez
  } deriving (Show, Eq)

makeLensesWith postfixLFields ''TxData
