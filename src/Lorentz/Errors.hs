{-# LANGUAGE DerivingStrategies, DeriveAnyClass #-}

-- We want to make sure 'failUsingArg' is used with sane argument.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Advanced errors.
module Lorentz.Errors
  ( -- * Haskell to 'Value' conversion
    IsError (..)
  , customErrorToVal
  , customErrorFromVal

  , deriveCustomError

  , UnspecifiedError (..)

    -- * Instructions
  , failUsing
  , failUsingArg
  , FailUsingArg
  , failUnexpected
  ) where

import Data.Singletons (SingI(..))
import qualified Language.Haskell.TH as TH
import Data.Vinyl.Derived (Label)
import Data.Constraint (Dict (..))
import Fmt ((+|), (+||), (|+), (||+))
import GHC.TypeLits (ErrorMessage(..), KnownSymbol, TypeError, symbolVal)

import Lorentz.Base
import Lorentz.Instr
import Lorentz.Value
import Lorentz.Constraints
import Michelson.Text
import Michelson.Typed.Haskell
import Michelson.Typed.Instr
import Michelson.Typed.Scope
import Michelson.Typed.Sing
import Michelson.Typed.Value
import Util.Typeable

----------------------------------------------------------------------------
-- IsError
----------------------------------------------------------------------------

type KnownErrorT t =
  ( Typeable t
  , SingI t
  , HasNoOp t
  , HasNoBigMap t
  )

-- | Haskell type representing error.
class IsError e where

  -- | Converts a Haskell error into @Value@ representation.
  errorToVal :: e -> (forall t. KnownErrorT t => Value t -> r) -> r

  -- | Converts a @Value@ into Haskell error.
  errorFromVal :: (Typeable t, SingI t) => Value t -> Either Text e

-- | Implementation of 'errorToVal' via 'IsoValue'.
isoErrorToVal
  :: (IsoValue e, KnownValue e, HasNoOp (ToT e), HasNoBigMap (ToT e))
  => e -> (forall t. KnownErrorT t => Value t -> r) -> r
isoErrorToVal e cont = cont $ toVal e

-- | Implementation of 'errorFromVal' via 'IsoValue'.
isoErrorFromVal
  :: (Typeable t, Typeable (ToT e), IsoValue e)
  => Value t -> Either Text e
isoErrorFromVal e = fromVal <$> gcastE e

-- | Implementation of 'errorToVal' for custom errors.
customErrorToVal
  :: (LooseSumC e, HasCallStack)
  => e
  -> (forall t. KnownErrorT t => Value t -> r)
  -> r
customErrorToVal e cont =
  case toTaggedVal e of
    (tag, SomeValue (datVal :: Value t)) ->
      -- Tags come from constructors names, so we can assume
      -- the event of weird chars occurrence to be quite improbable
      let tag' = mkMTextUnsafe tag
      in case (opAbsense (sing @t), bigMapAbsense (sing @t)) of
        (Just Dict, Just Dict) -> cont $ VPair (VC (CvString tag'), datVal)

        -- We could check this at type-level, but this would require
        -- specializing 'Michelson.Typed.LooseSum' to errors.
        -- We can do so, or assume that no one will ever try to put 'Operation'
        -- to error datatypes:
        (Nothing, _) -> error "Operation in constructor data"
        (_, Nothing) -> error "BigMap in constructor data"

-- | Implementation of 'errorFromVal' for custom errors.
customErrorFromVal
  :: forall t e.
      (SingI t, LooseSumC e)
  => Value t -> Either Text e
customErrorFromVal v = case (v, sing @t) of
  (VPair (VC (CvString tag), datVal), STPair _ _) ->
    case fromTaggedVal (toText tag, SomeValue datVal) of
      ComposeOk e ->
        Right e
      ComposeCtorNotFound ->
        Left $ "Unknown error constructor " +| tag |+ ""
      ComposeFieldTypeMismatch got expected ->
        Left $ "Error data type mismatch, expected " +|| expected ||+
                                            ", got " +|| got ||+ ""
  _ -> Left $ "Expected a (tag, dat) pair representing error"

-- | Derive 'IsError' instance for given type.
--
-- This will also forbid deriving 'IsoValue' instance for that type to avoid
-- having multiple different Michelson representations.
deriveCustomError :: TH.Name -> TH.Q [TH.Dec]
deriveCustomError name =
  [d|
    instance IsError $ty where
      errorToVal = customErrorToVal
      errorFromVal = customErrorFromVal

    instance TypeError ('Text "No IsoValue instance for " ':<>: 'ShowType $ty) =>
        IsoValue $ty where
      type ToT $ty = TypeError ('Text "No IsoValue instance for " ':<>: 'ShowType $ty)
      toVal = error "impossible"
      fromVal = error "impossible"
  |]
  where
    ty = pure (TH.ConT name)

-- Instances
----------------------------------------------------------------------------

instance IsError MText where
  errorToVal = isoErrorToVal
  errorFromVal = isoErrorFromVal

instance TypeError ('Text "Use representative error messages") => IsError () where
  errorToVal _ _ = error "impossible"
  errorFromVal = error "impossible"

-- | Use this type as replacement for @()@ when you __really__ want to leave
-- error cause unspecified.
data UnspecifiedError = UnspecifiedError
  deriving stock Generic
  deriving anyclass IsoValue

instance IsError UnspecifiedError where
  errorToVal = isoErrorToVal
  errorFromVal = isoErrorFromVal

----------------------------------------------------------------------------
-- Instructions
----------------------------------------------------------------------------

-- | Signature of 'userFailWith'.
type FailUsingArg e name fieldTy s s'
  = ( KnownSymbol name, IsError e
    , CtorHasOnlyField name e fieldTy, Each [Typeable, SingI] '[ToT fieldTy]
    , HasCallStack
    )
  => Label name -> fieldTy : s :-> s'

-- | Fail with the given Haskell value.
failUsing
  :: IsError e
  => e -> s :-> t
failUsing err = errorToVal err $ \eval -> I $ PUSH eval `Seq` FAILWITH

-- | Fail with given error, picking argument for error from the top
-- of the stack.
--
-- If your error constructor does not carry an argument, use 'failUsing'
-- function instead.
-- Consider the following practice: once error datatype for your contract
-- is defined, create a specialization of this function to the error type.
failUsingArg
  :: forall err name fieldTy s s'.
     FailUsingArg err name fieldTy s s'
failUsingArg _ =
  push (mkMTextUnsafe ctor) #
  pair #
  failWith
  where
    ctor = case symbolVal (Proxy @name) of
      'c' : other -> toText other
      other -> error $ "Bad label provided: " +| other |+ ""

-- | Fail, providing a reference to the place in the code where
-- this function is called.
--
-- Like 'error' in Haskell code, this instruction is for internal errors only.
failUnexpected :: HasCallStack => MText -> s :-> t
failUnexpected msg =
  failUsing $ [mt|Unexpected failure: |] <> msg <> [mt|\n|]
           <> mkMTextCut (toText $ prettyCallStack callStack)
