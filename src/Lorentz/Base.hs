{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Foundation of Lorentz development.
module Lorentz.Base
  ( (:->) (..)
  , type (&)
  , (#)

  , compileLorentz
  , compileLorentzContract
  , printLorentzContract

  , ContractOut
  , Contract
  , Lambda

  ) where

import qualified Data.Kind as Kind
import Data.Singletons (SingI)

import Lorentz.Constraints
import Lorentz.Value
import Michelson.Printer (printTypedContract)
import Michelson.Typed (Instr(..), T(..), ToT, ToTs, Value'(..))

-- | Alias for instruction which hides inner types representation via 'T'.
newtype (inp :: [Kind.Type]) :-> (out :: [Kind.Type]) =
  I { unI :: Instr (ToTs inp) (ToTs out) }
  deriving (Show, Eq)
infixr 1 :->

-- | For use outside of Lorentz.
compileLorentz :: (inp :-> out) -> Instr (ToTs inp) (ToTs out)
compileLorentz = unI

type ContractOut st = '[([Operation], st)]
type Contract cp st = '[(cp, st)] :-> ContractOut st

-- | Version of 'compileLorentz' specialized to instruction corresponding to
-- contract code.
compileLorentzContract
  :: forall cp st.
     (NoOperation cp, NoOperation st, NoBigMap cp, CanHaveBigMap st)
  => Contract cp st -> Instr '[ToT (cp, st)] '[ToT ([Operation], st)]
compileLorentzContract = compileLorentz

printLorentzContract
  :: forall cp st.
      ( SingI (ToT cp), SingI (ToT st)
      , NoOperation cp, NoOperation st, NoBigMap cp, CanHaveBigMap st
      )
  => Contract cp st -> LText
printLorentzContract = printTypedContract . compileLorentzContract

type (&) (a :: Kind.Type) (b :: [Kind.Type]) = a ': b
infixr 2 &

(#) :: (a :-> b) -> (b :-> c) -> a :-> c
I l # I r = I (l `Seq` r)

type Lambda i o = '[i] :-> '[o]

instance IsoValue (Lambda inp out) where
  type ToT (Lambda inp out) = 'TLambda (ToT inp) (ToT out)
  toVal = VLam . unI
  fromVal (VLam l) = I l
