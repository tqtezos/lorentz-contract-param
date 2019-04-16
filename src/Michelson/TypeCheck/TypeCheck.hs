module Michelson.TypeCheck.TypeCheck
  ( TcInstrHandler
  , TcExtHandler
  , TcOriginatedContracts
  , TcResult
  , TypeCheckEnv (..)
  , TypeCheckT
  , runTypeCheckT
  ) where

import Michelson.TypeCheck.Error (TCError)
import Michelson.Typed.Instr (InstrExtT)
import Michelson.TypeCheck.Types
import qualified Michelson.Untyped as U
import Tezos.Address (Address)

type TypeCheckT a =
  ExceptT TCError
    (State TypeCheckEnv) a

-- | Function for typeChecking a @nop@ and updating state
-- TypeCheckT is used because inside
-- inside of TEST_ASSERT could be PRINT/STACKTYPE/etc extended instructions.
type TcExtHandler
  = U.ExpandedInstrExtU -> TcExtFrames -> SomeHST -> TypeCheckT (TcExtFrames, Maybe InstrExtT)

type TcOriginatedContracts = Map Address U.Type

-- | The typechecking state
data TypeCheckEnv = TypeCheckEnv
  { tcExtHandler    :: TcExtHandler
  , tcExtFrames     :: TcExtFrames
  , tcContractParam :: U.Type
  , tcContracts     :: TcOriginatedContracts
  }

runTypeCheckT :: TcExtHandler -> U.Type -> TcOriginatedContracts -> TypeCheckT a -> Either TCError a
runTypeCheckT nh param contracts act =
  evaluatingState (TypeCheckEnv nh [] param contracts) $ runExceptT act

type TcResult inp = Either TCError (SomeInstr inp)

type TcInstrHandler
   = forall inp. Typeable inp
      => U.ExpandedInstr
      -> HST inp
      -> TypeCheckT (SomeInstr inp)

