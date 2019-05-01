{-# LANGUAGE DerivingStrategies, FunctionalDependencies #-}

-- | Referenced-by-type versions of some instructions.
--
-- They allow to "dip" into stack or copy elements of stack referring them
-- by type. Their use is justified, because in most cases there is only
-- one element of each type of stack, and in cases when this does not hold
-- (e.g. entry point with multiple parameters of the same type), it might be
-- a good idea to wrap those types into a newtype or to use primitives from
-- 'named' package.
--
-- This module is experimental, i.e. everything here should work but may be
-- removed in favor of better development practices.
--
-- Each instruction is followed with usage example.
module Lorentz.Referenced
  ( dupT
  , dipT
  , dropT
  ) where

import Prelude hiding (drop, swap)

import qualified Data.Kind as Kind
import Data.Type.Bool (If)
import GHC.TypeLits (ErrorMessage(..), TypeError)

import Lorentz.Base
import Lorentz.Instr
import Util.Type

-- Errors
----------------------------------------------------------------------------

type family StackElemNotFound st a :: ErrorMessage where
  StackElemNotFound st a =
    'Text "Element of type `" ':<>: 'ShowType a ':<>:
    'Text "` is not present on stack" ':$$: 'ShowType st

type family StackElemAmbiguous st a :: ErrorMessage where
  StackElemAmbiguous st a =
    'Text "Ambigous reference to element of type `" ':<>: 'ShowType a ':<>:
    'Text "` for stack" ':$$: 'ShowType st

-- Dup
----------------------------------------------------------------------------

-- | Allows duplicating stack elements referring them by type.
class DupT (origSt :: [Kind.Type]) (a :: Kind.Type) (st :: [Kind.Type]) where
  dupTImpl :: st :-> a : st

instance TypeError (StackElemNotFound origSt a) =>
         DupT origSt a '[] where
  dupTImpl = error "impossible"

instance {-# OVERLAPPING #-}
         If (a `IsElem` st)
            (TypeError (StackElemAmbiguous origSt a))
            (() :: Constraint) =>
         DupT origSt a (a : st) where
  dupTImpl = dup

instance {-# OVERLAPPABLE #-}
         DupT origSt a st =>
         DupT origSt a (b : st) where
  dupTImpl = dip (dupTImpl @origSt) # swap

-- | Duplicate an element of stack referring it by type.
--
-- If stack contains multiple entries of this type, compile error is raised.
dupT :: forall a st. DupT st a st => st :-> a : st
dupT = dupTImpl @st @a @st

_dupSample1 :: [Integer, Text, ()] :-> [Text, Integer, Text, ()]
_dupSample1 = dupT @Text

-- Dip
----------------------------------------------------------------------------

-- | Allows diving into stack referring expected new tip by type.
--
-- Implemented with fun deps for conciseness; we can replace them
-- with a type family anytime, but that would probably require more declarations.
class DipT (origInp :: [Kind.Type]) (a :: Kind.Type)
           (inp :: [Kind.Type]) (dipInp :: [Kind.Type])
           (dipOut :: [Kind.Type]) (out :: [Kind.Type])
           | inp a -> dipInp, dipOut inp a -> out where
  dipTImpl :: (dipInp :-> dipOut) -> (inp :-> out)

instance ( TypeError (StackElemNotFound origSt a)
         , dipInp ~ TypeError ('Text "Undefined type (see next error)")
         , out ~ TypeError ('Text "Undefined type (see next error)")
         ) =>
         DipT origSt a '[] dipInp dipOut out where
  dipTImpl = error "impossible"

instance {-# OVERLAPPING #-}
         ( If (a `IsElem` st)
            (TypeError (StackElemAmbiguous origSt a))
            (() :: Constraint)
         , dipInp ~ (a : st)
         , dipOut ~ out
         ) =>
         DipT origSt a (a : st) dipInp dipOut out where
  dipTImpl = id

instance {-# OVERLAPPABLE #-}
         ( DipT origSt a st dipInp dipOut out
         , out1 ~ (b : out)
         ) =>
         DipT origSt a (b : st) dipInp dipOut out1 where
  dipTImpl = dip . dipTImpl @origSt @a @st

-- | Dip repeatedly until element of the given type is on top of the stack.
--
-- If stack contains multiple entries of this type, compile error is raised.
dipT
  :: forall a inp dinp dout out.
     DipT inp a inp dinp dout out
  => (dinp :-> dout) -> (inp :-> out)
dipT = dipTImpl @inp @a @inp @dinp

_dipSample1
  :: [Natural, ()]
      :-> '[ByteString]
  -> [Integer, Text, Natural, ()]
      :-> [Integer, Text, ByteString]
_dipSample1 = dipT @Natural

-- Drop
----------------------------------------------------------------------------

-- | Remove element with the given type from the stack.
dropT
  :: forall a inp dinp dout out.
     ( DipT inp a inp dinp dout out
     , dinp ~ (a ': dout)
     )
  => inp :-> out
dropT = dipT @a drop

_dropSample1 :: [Integer, (), Natural] :-> [Integer, Natural]
_dropSample1 = dropT @()

-- Framing
----------------------------------------------------------------------------

{- Note that there instructions are only usable for concrete stacks.

When you know your stack only partially, and you try to refer to element of
type "X", then with the current approach compiler will require the unknown
part of stack to contain no elements of type "X", and this is annoying
at least because it ruins modularity.

For now I suggest to use these instructions only in contract code directly, and
if you need to extract a set of instructions to a reusable function polymorphic
over stack tail then it will probably be small enough to use just plain
instructions.

If this module is in demand, I hope we will find a way to resolve this issue.

-}
