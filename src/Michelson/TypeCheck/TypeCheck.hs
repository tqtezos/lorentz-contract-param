module Michelson.TypeCheck.TypeCheck
  ( TcInstrHandler
  , TcOriginatedContracts
  , TcResult
  , TypeCheckEnv (..)
  , TypeCheck
  , runTypeCheck
  , TypeCheckInstr

  , tcContractParamL
  , tcContractsL
  , tcExtFramesL
  ) where

import Control.Lens (makeLensesWith)

import Michelson.ErrorPos (InstrCallStack)
import Michelson.TypeCheck.Error (TCError)
import Michelson.TypeCheck.Types
import qualified Michelson.Untyped as U
import Tezos.Address (Address)
import Util.Lens

type TypeCheck a =
  ExceptT TCError
    (State TypeCheckEnv) a

type TcOriginatedContracts = Map Address U.Type

-- | The typechecking state
data TypeCheckEnv = TypeCheckEnv
  { tcExtFrames     :: TcExtFrames
  , tcContractParam :: U.Type
  , tcContracts     :: TcOriginatedContracts
  }

makeLensesWith postfixLFields ''TypeCheckEnv

runTypeCheck :: U.Type -> TcOriginatedContracts -> TypeCheck a -> Either TCError a
runTypeCheck param contracts act =
  evaluatingState (TypeCheckEnv [] param contracts) $ runExceptT act

type TcResult inp = Either TCError (SomeInstr inp)

type TypeCheckInstr a =
       ReaderT InstrCallStack (ExceptT TCError (State TypeCheckEnv)) a

-- pva701: it's really painful to add arguments to TcInstrHandler
-- due to necessity to refactor @typeCheckInstr@.
-- Also functions which are being called from @typeCheckInstr@ would
-- have to be refactored too.
-- Therefore, I am using ReaderT over TypeCheck.
type TcInstrHandler
   = forall inp. Typeable inp
      => U.ExpandedInstr
      -> HST inp
      -> TypeCheckInstr (SomeInstr inp)
