-- | Module, providing data types and functions for conversion from
-- instruction and value representation from @Michelson.Type@ module
-- to strictly-typed GADT-based representation from @Advanced.Value@ module.
--
-- This conversion is labeled as type check because that's what we are obliged
-- to do on our way.
--
-- Type check algorithm relies on the property of Michelson language that each
-- instruction on a given input stack type produces a definite output stack
-- type.
-- Michelson contract defines concrete types for storage and parameter, from
-- which input stack type is deduced. Then this type is being combined with
-- each subsequent instruction, producing next stack type after each
-- application.
--
-- Function @typeCheck@ takes list of instructions and returns value of type
-- @Instr op inp out@ along with @IT inp@ and @IT out@ all wrapped into
-- @SomeInstr op@ data type. This wrapping is done to satsify Haskell type
-- system (which has no support for dependent types).
-- Functions @typeCheckI@, @typeCheckV@ behave similarly.
--
-- When a recursive call is made within @typeCheck@, @typeCheckI@ or
-- @typeCheckV@, result of a call is unwrapped from @SomeInstr@ and type
-- information from @IT inp@ and @IT out@ is being used to assert that
-- recursive call returned instruction of expected type
-- (error is thrown otherwise).

module Advanced.TypeCheck
    (
      typeCheck
    , typeCheckI
    , typeCheckV
    , IT (..)
    , SomeIT (..)
    , SomeInstr (..)
    , SomeVal (..)
    ) where

import qualified Text.Show
import Data.Typeable (eqT, (:~:)(..), typeRep)
import Data.Singletons (SingI(sing))

import Advanced.Type (fromSingT, Sing (..), CT(..), T(..), fromMType, withSomeSingT)
import Advanced.CValue (CVal (..))
import Advanced.Value (Val (..), Instr (..))
import Advanced.Arith (ArithOp(..), Add)

import qualified Michelson.Types as M

-- | Data type holding type information for stack.
--
-- This data type is used along with instruction data type @Instr@
-- to carry information about its input and output stack types.
--
-- That is, if there is value @instr :: Instr op inp out@, along with this
-- @instr@ one may carry @inpIT :: IT inp@ and @outIT :: IT out@ which will
-- contain whole information about input and output stack types for @instr@.
--
-- Data type @IT@ is very similar to @Data.Vinyl.Rec@,
-- but is specialized for a particular purpose.
-- In particular, definition of @IT (t1 ': t2 ': ... tn ': '[])@ requires
-- constraints @(Typeable t1, Typeable t2, ..., Typeable tn)@ as well as
-- constraints @(Typeable '[ t1 ], Typeable '[ t1, t2 ], ...)@.
-- These applications of @Typeable@ class are required for convenient usage
-- of type encoded by @IT ts@ with some functions from @Data.Typeable@.
--
-- Data type @IT@ is a heterogenuous list of singletons which is due to its
-- main motivation to be used as representation of @Instr@ type data
-- for pattern-matching.
data IT (ts :: [T])  where
  INil :: IT '[]
  (::&) :: (Typeable xs, Typeable x) => Sing x -> IT xs -> IT (x ': xs)

instance Show (IT ts) where
  show INil = "[]"
  show (r ::& rs) = "[ " <> showDo (r ::& rs) <> " ]"
    where
      showDo :: IT (t ': ts_) -> String
      showDo (a ::& (b ::& c)) = show (fromSingT a) <> ", " <> showDo (b ::& c)
      showDo (a ::& INil) = show (fromSingT a)

infixr 7 ::&

-- | No-argument type wrapper for @IT@ data type.
data SomeIT where
  SomeIT :: Typeable ts => IT ts -> SomeIT

-- | Data type holding both instruction and
-- type representations of instruction's input and output.
--
-- Intput and output stack types are wrapped inside the type and @Typeable@
-- constraints are provided to allow convenient unwrapping.
data SomeInstr op where
  (:::) :: (Typeable inp, Typeable out) => Instr op inp out -> (IT inp, IT out) -> SomeInstr op

instance Show op => Show (SomeInstr op) where
  show (i ::: (inp, out)) = show i <> " :: " <> show inp <> " -> " <> show out

-- | Data type, holding strictly-typed Michelson value along with its
-- type singleton.
data SomeVal op where
    (::::) :: Typeable t => Val op t -> Sing t -> SomeVal op

-- | Helper function to construct instructions for binary arithmetic
-- operations.
arithImpl
  :: ( Typeable ('T_c (ArithResT aop n m))
     , SingI (ArithResT aop n m)
     , Typeable ('T_c (ArithResT aop n m) ': s)
     )
  => IT ('T_c n ': 'T_c m ': s)
  -> Instr op ('T_c n ': 'T_c m ': s) ('T_c (ArithResT aop n m) ': s)
  -> SomeInstr op
arithImpl i@(_ ::& _ ::& rs) op = op ::: (i, sing ::& rs)

-- | Function @typeCheck@ converts non-empty list of Michelson instructions
-- given in representation from @Michelson.Type@ module to representation
-- in strictly typed GADT.
--
-- Types are checked along the way which is neccessary to construct a
-- strictly typed value.
--
-- As a second argument, @typeCheck@ accepts input stack type representation.
typeCheck
  :: forall op.
     NonEmpty M.Instr
  -> SomeIT
  -> Either Text (SomeInstr op)
typeCheck (a :| []) t = typeCheckI a t
typeCheck (p_ :| (r : rs)) (SomeIT (a :: IT a)) = do
    p ::: ((_ :: IT a'), (b :: IT b)) <- typeCheckI @op p_ (SomeIT a)
    q ::: ((_ :: IT b'), c) <- typeCheck @op (r :| rs) (SomeIT b)
    Refl <- eqT' @a @a'
    Refl <- eqT' @b @b'
    pure $ (Seq p q) ::: (a, c)

-- | Function @eqT'@ is a simple wrapper around @Data.Typeable.eqT@ suited
-- for use within @Either Text a@ applicative.
eqT' :: forall (a_ :: [T]) (b_ :: [T]) . (Typeable a_, Typeable b_) => Either Text (a_ :~: b_)
eqT' = maybe (Left $
                "Unexpected condition in type checker: types not equal: "
                  <> show (typeRep (Proxy @a_))
                  <> " /= "
                  <> show (typeRep (Proxy @b_))
                  ) pure eqT

-- | Function @typeCheckI@ converts a single Michelson instruction
-- given in representation from @Michelson.Type@ module to representation
-- in strictly typed GADT.
--
-- As a second argument, @typeCheckI@ accepts input stack type representation.
--
-- Type checking algorithm pattern-matches on given instruction, input stack
-- type and constructs strictly typed GADT value, checking necessary type
-- equalities when neccessary.
--
-- If there was no match on a given pair of instruction and input stack,
-- that is interpreted as input of wrong type and type check finishes with
-- error.
typeCheckI :: forall op. M.Instr -> SomeIT -> Either Text (SomeInstr op)
typeCheckI M.DROP (SomeIT i@(_ ::& rs)) = pure (DROP ::: (i, rs))
typeCheckI M.SWAP (SomeIT i@(a ::& b ::& rs)) =
  pure (SWAP ::: (i, b ::& a ::& rs))
typeCheckI (M.PUSH _ mt mval) (SomeIT i) = do
  val :::: t <- typeCheckV mval (fromMType mt)
  pure $ PUSH val ::: (i, t ::& i)
typeCheckI (M.NIL _ _ mt) (SomeIT i) = do
  withSomeSingT (fromMType mt) $ \t ->
    pure $ NIL ::: (i, ST_list t ::& i)
typeCheckI (M.CONS _) (SomeIT i@((a :: Sing a) ::& ST_list (_ :: Sing a') ::& rs)) = do
  Refl <- eqT' @'[a] @'[a']
  pure $ CONS ::: (i, ST_list a ::& rs)
typeCheckI (M.ITER (i1 : ir)) (SomeIT i@(ST_list (e :: Sing e) ::& (rs :: IT rs))) = do
  -- ^ case `M.ITER _ []` is wrong typed (as it is required to at least drop an element)
  subI ::: ((_ :: IT i), (_ :: IT o)) <- typeCheck @op (fmap M.unOp $ i1 :| ir) (SomeIT (e ::& rs))
  Refl <- eqT' @i @(e ': rs)
  Refl <- eqT' @o @rs
  pure (ITER subI ::: (i, rs))
typeCheckI (M.PAIR _ _ _ _) (SomeIT i@(a ::& b ::& rs)) = pure (PAIR ::: (i, ST_pair a b ::& rs))
typeCheckI (M.CDR _ _) (SomeIT i@(ST_pair _ b ::& rs)) = pure (CDR ::: (i, b ::& rs))
typeCheckI (M.DIP []) (SomeIT i@(_ ::& _)) = pure (DIP Nop ::: (i, i))
typeCheckI (M.DIP (i1 : ir)) (SomeIT i@(a ::& (s :: IT s))) = do
  subI ::: ((_ :: IT s'), t) <- typeCheck @op (fmap M.unOp $ i1 :| ir) (SomeIT s)
  Refl <- eqT' @s @s'
  pure (DIP subI ::: (i, a ::& t))
typeCheckI (M.ADD _) (SomeIT i@(ST_c ST_int ::& ST_c ST_int ::& _)) = pure $ arithImpl @Add i ADD
typeCheckI (M.ADD _) (SomeIT i@(ST_c ST_int ::& ST_c ST_nat ::& _)) = pure $ arithImpl @Add i ADD
typeCheckI (M.ADD _) (SomeIT i@(ST_c ST_nat ::& ST_c ST_int ::& _)) = pure $ arithImpl @Add i ADD
typeCheckI (M.ADD _) (SomeIT i@(ST_c ST_nat ::& ST_c ST_nat ::& _)) = pure $ arithImpl @Add i ADD
typeCheckI instr (SomeIT t) = Left $ "Error checking expression " <> show instr <> " against type " <> show t

-- | Function @typeCheckV@ converts a single Michelson value
-- given in representation from @Michelson.Type@ module to representation
-- in strictly typed GADT.
--
-- As a second argument, @typeCheckV@ accepts expected type of value.
--
-- Type checking algorithm pattern-matches on parse value representation,
-- expected type @t@ and constructs @Val op t@ value.
--
-- If there was no match on a given pair of value and expected type,
-- that is interpreted as input of wrong type and type check finishes with
-- error.
typeCheckV :: M.Value M.Op -> T -> Either Text (SomeVal op)
typeCheckV (M.ValueInt i) (T_c T_int) = pure $ VC (CvInt i) :::: ST_c ST_int
typeCheckV v t = Left $ "Error checking value " <> show v <> " against type " <> show t

