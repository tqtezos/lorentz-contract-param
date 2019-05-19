-- | Utilities used for contracts discovery.
--
-- All the discovery logic resides in 'lorentz-discover' executable.
module Lorentz.Discover
  ( IsContract (..)
  ) where

import Data.Singletons (SingI)

import qualified Lorentz.Base as L
import Lorentz.Constraints
import qualified Michelson.Typed as T
import qualified Michelson.Untyped as U

-- | Defined for values representing a contract.
class IsContract c where
  toUntypedContract :: c -> U.Contract

instance IsContract U.Contract where
  toUntypedContract = id

instance (SingI cp, SingI st) => IsContract (T.Contract cp st) where
  toUntypedContract = T.convertContract

instance ( SingI (T.ToT cp), SingI (T.ToT st)
         , NoOperation cp, NoOperation st, NoBigMap cp, CanHaveBigMap st
         ) =>
         IsContract (L.Contract cp st) where
  toUntypedContract = toUntypedContract . L.compileLorentzContract
