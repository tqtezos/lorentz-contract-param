{-# LANGUAGE DerivingStrategies #-}

-- | Advanced errors.
module Lorentz.Errors
  ( LorentzUserError
  , unLorentzUserError
  , UserFailInstr
  , userFailWith
  ) where

import Data.Singletons (SingI)
import Data.Vinyl.Derived (Label)
import GHC.TypeLits (KnownSymbol, symbolVal)

import Lorentz.ADT
import Lorentz.Base
import Lorentz.Coercions
import Lorentz.Instr
import Lorentz.Value
import Michelson.Text
import Michelson.Typed.Haskell

-- | A unique error identifier.
newtype ErrorTag = ErrorTag MText
  deriving newtype (Show, Eq, Ord, IsString, IsoValue)

-- | An error indicating a normal failure caused by such user input.
type LorentzUserError e = (ErrorTag, e)

-- | Pseudo-getter for error within 'LorentzUserError'.
unLorentzUserError :: LorentzUserError e -> e
unLorentzUserError = snd

-- | Signature of 'userFailWith'.
type UserFailInstr e name s s'
  = (InstrWrapC e name, KnownSymbol name)
  => Label name -> AppendCtorField (GetCtorField e name) s :-> s'

-- | Fail with given error, picking argument for error from the top
-- of the stack if any required. Error will be wrapped into 'LorentzUserError'
-- (i.e. an error tag will be attached to the error data).
--
-- Consider the following practice: once error datatype for your contract
-- is defined, create a specialization of this function to the error type.
userFailWith
  :: forall err name s s'.
      (Typeable (ToT err), SingI (ToT err))
  => UserFailInstr err name s s'
userFailWith label =
  wrap_ @err @_ @s label # push (mkMTextUnsafe . toText $ symbolVal (Proxy @name)) # pair #
  coerce_ @_ @(LorentzUserError err) #
  failWith
