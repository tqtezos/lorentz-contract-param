-- | Foundation of Lorentz development.
module Lorentz.Base
  ( (:->) (..)
  , type (&)
  , (#)
  , compileLorentz

  , Contract
  , Lambda

  , Coercible_
  , coerce_
  ) where

import qualified Data.Kind as Kind

import Lorentz.Value
import Michelson.Typed ((:+>), Instr(..), T(..), ToT, ToTs, Value'(..))
import Michelson.Typed.Polymorphic ()

-- | Alias for instruction which hides inner types representation via 'T'.
newtype (inp :: [Kind.Type]) :-> (out :: [Kind.Type]) =
  I { unI :: ToTs inp :+> ToTs out }
infixr 1 :->

-- | For use outside of Lorentz.
compileLorentz :: (inp :-> out) -> (ToTs inp :+> ToTs out)
compileLorentz = unI

type (&) (a :: Kind.Type) (b :: [Kind.Type]) = a ': b
infixr 2 &

-- TODO: this is the second operator with this name
-- call it differently?
(#) :: (a :-> b) -> (b :-> c) -> a :-> c
I l # I r = I (l `Seq` r)

type Contract cp st = '[(cp, st)] :-> '[([Operation], st)]

type Lambda i o = '[i] :-> '[o]

instance IsoValue (Lambda inp out) where
  type ToT (Lambda inp out) = 'TLambda (ToT inp) (ToT out)
  toVal = VLam . unI
  fromVal (VLam l) = I l

-- | Whether two types have the same Michelson representation.
type Coercible_ a b = ToT a ~ ToT b

-- | Convert between values of types that have the same representation.
coerce_ :: Coercible_ a b => a & s :-> b & s
coerce_ = I Nop
