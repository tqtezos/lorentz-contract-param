module Michelson.TypeCheck.TypeCheck
  ( TcInstrHandler
  , TcOriginatedContracts
  , TcResult
  , TypeCheckEnv (..)
  , TypeCheckT
  , runTypeCheckT
  ) where

import Michelson.TypeCheck.Error (TCError)
import Michelson.TypeCheck.Types
import qualified Michelson.Untyped as U
import Tezos.Address (Address)

type TypeCheckT a =
  ExceptT TCError
    (State TypeCheckEnv) a

type TcOriginatedContracts = Map Address U.Type

-- | The typechecking state
data TypeCheckEnv = TypeCheckEnv
  { tcExtFrames     :: TcExtFrames
  , tcContractParam :: U.Type
  , tcContracts     :: TcOriginatedContracts
  }

runTypeCheckT :: U.Type -> TcOriginatedContracts -> TypeCheckT a -> Either TCError a
runTypeCheckT param contracts act =
  evaluatingState (TypeCheckEnv [] param contracts) $ runExceptT act

type TcResult inp = Either TCError (SomeInstr inp)

type TcInstrHandler
   = forall inp. Typeable inp
      => U.ExpandedInstr
      -> HST inp
      -> TypeCheckT (SomeInstr inp)
