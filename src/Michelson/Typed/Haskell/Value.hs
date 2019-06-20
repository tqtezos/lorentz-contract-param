{-# LANGUAGE DerivingStrategies #-}

-- | Conversions between haskell types/values and Michelson ones.
module Michelson.Typed.Haskell.Value
  ( IsoCValue (..)
  , IsoValue (..)
  , GIsoValue (GValueType)
  , ToTs
  , ToT'
  , ToTs'
  , SomeIsoValue (..)
  , AnyIsoValue (..)
  , IsComparable

  , ContractAddr (..)
  , BigMap (..)
  ) where

import qualified Data.Kind as Kind
import Data.Default (Default)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics ((:*:)(..), (:+:)(..))
import qualified GHC.Generics as G
import Named (NamedF(..))

import Michelson.Typed.Aliases
import Michelson.Typed.CValue
import Michelson.Typed.T
import Michelson.Text
import Michelson.Typed.Value
import Tezos.Address (Address)
import Tezos.Core (Mutez, Timestamp)
import Tezos.Crypto (KeyHash, PublicKey, Signature)

----------------------------------------------------------------------------
-- Comparable values isomorphism
----------------------------------------------------------------------------

-- | Isomorphism between Michelson primitive values and plain Haskell types.
class IsoCValue a where
  -- | Type function that converts a regular Haskell type into a comparable type
  -- (which has kind @CT@).
  type ToCT a :: CT

  -- | Converts a single Haskell value into @CVal@ representation.
  toCVal :: a -> CValue (ToCT a)

  -- | Converts a @CVal@ value into a single Haskell value.
  fromCVal :: CValue (ToCT a) -> a

instance IsoCValue Integer where
  type ToCT Integer = 'CInt
  toCVal = CvInt
  fromCVal (CvInt i) = i

instance IsoCValue Natural where
  type ToCT Natural = 'CNat
  toCVal = CvNat
  fromCVal (CvNat i) = i

instance IsoCValue MText where
  type ToCT MText = 'CString
  toCVal = CvString
  fromCVal (CvString s) = s

instance DoNotUseTextError => IsoCValue Text where
  type ToCT Text = DoNotUseTextError
  toCVal = error "impossible"
  fromCVal _ = error "impossible"

instance IsoCValue Bool where
  type ToCT Bool = 'CBool
  toCVal = CvBool
  fromCVal (CvBool b) = b

instance IsoCValue ByteString where
  type ToCT ByteString = 'CBytes
  toCVal = CvBytes
  fromCVal (CvBytes b) = b

instance IsoCValue Mutez where
  type ToCT Mutez = 'CMutez
  toCVal = CvMutez
  fromCVal (CvMutez m) = m

instance IsoCValue Address where
  type ToCT Address = 'CAddress
  toCVal = CvAddress
  fromCVal (CvAddress a) = a

instance IsoCValue KeyHash where
  type ToCT KeyHash = 'CKeyHash
  toCVal = CvKeyHash
  fromCVal (CvKeyHash k) = k

instance IsoCValue Timestamp where
  type ToCT Timestamp = 'CTimestamp
  toCVal = CvTimestamp
  fromCVal (CvTimestamp t) = t

----------------------------------------------------------------------------
-- Complex values isomorphism
----------------------------------------------------------------------------

-- | Isomorphism between Michelson values and plain Haskell types.
--
-- Default implementation of this typeclass converts ADTs to Michelson
-- "pair"s and "or"s.
class IsoValue a where
  -- | Type function that converts a regular Haskell type into a @T@ type.
  type ToT a :: T
  type ToT a = GValueType (G.Rep a)

  -- | Converts a Haskell structure into @Value@ representation.
  toVal :: a -> Value (ToT a)
  default toVal
    :: (Generic a, GIsoValue (G.Rep a), ToT a ~ GValueType (G.Rep a))
    => a -> Value (ToT a)
  toVal = gToValue . G.from

  -- | Converts a @Value@ into Haskell type.
  fromVal :: Value (ToT a) -> a
  default fromVal
    :: (Generic a, GIsoValue (G.Rep a), ToT a ~ GValueType (G.Rep a))
    => Value (ToT a) -> a
  fromVal = G.to . gFromValue

-- | Type function to convert a Haskell stack type to @T@-based one.
type family ToTs (ts :: [Kind.Type]) :: [T] where
  ToTs '[] = '[]
  ToTs (x ': xs) = ToT x ': ToTs xs

-- | Overloaded version of 'ToT' to work on Haskell and @T@ types.
type family ToT' (t :: k) :: T where
  ToT' (t :: T) = t
  ToT' (t :: Kind.Type) = ToT t

-- | Overloaded version of 'ToTs' to work on Haskell and @T@ stacks.
type family ToTs' (t :: [k]) :: [T] where
  ToTs' (t :: [T]) = t
  ToTs' (a :: [Kind.Type]) = ToTs a

-- | Hides some Haskell value put in line with Michelson 'Value'.
data SomeIsoValue where
  SomeIsoValue :: (Typeable a, IsoValue a) => a -> SomeIsoValue

-- | Any Haskell value which can be converted to Michelson 'Value'.
newtype AnyIsoValue = AnyIsoValue (forall a. IsoValue a => a)

-- | A useful property which holds for all 'CT' types.
type IsComparable c = ToT c ~ 'Tc (ToCT c)

-- Instances
----------------------------------------------------------------------------

instance IsoValue Integer where
  type ToT Integer = 'Tc (ToCT Integer)
  toVal = VC . toCVal
  fromVal (VC x) = fromCVal x

instance IsoValue Natural where
  type ToT Natural = 'Tc (ToCT Natural)
  toVal = VC . toCVal
  fromVal (VC x) = fromCVal x

instance IsoValue MText where
  type ToT MText = 'Tc (ToCT MText)
  toVal = VC . toCVal
  fromVal (VC x) = fromCVal x

instance DoNotUseTextError => IsoValue Text where
  type ToT Text = DoNotUseTextError
  toVal = error "impossible"
  fromVal = error "impossible"

instance IsoValue Bool where
  type ToT Bool = 'Tc (ToCT Bool)
  toVal = VC . toCVal
  fromVal (VC x) = fromCVal x

instance IsoValue ByteString where
  type ToT ByteString = 'Tc (ToCT ByteString)
  toVal = VC . toCVal
  fromVal (VC x) = fromCVal x

instance IsoValue Mutez where
  type ToT Mutez = 'Tc (ToCT Mutez)
  toVal = VC . toCVal
  fromVal (VC x) = fromCVal x

instance IsoValue KeyHash where
  type ToT KeyHash = 'Tc (ToCT KeyHash)
  toVal = VC . toCVal
  fromVal (VC x) = fromCVal x

instance IsoValue Timestamp where
  type ToT Timestamp = 'Tc (ToCT Timestamp)
  toVal = VC . toCVal
  fromVal (VC x) = fromCVal x

instance IsoValue Address where
  type ToT Address = 'Tc (ToCT Address)
  toVal = VC . toCVal
  fromVal (VC x) = fromCVal x

instance IsoValue PublicKey where
  type ToT PublicKey = 'TKey
  toVal = VKey
  fromVal (VKey x) = x

instance IsoValue Signature where
  type ToT Signature = 'TSignature
  toVal = VSignature
  fromVal (VSignature x) = x

instance IsoValue ()

instance IsoValue a => IsoValue [a] where
  type ToT [a] = 'TList (ToT a)
  toVal = VList . map toVal
  fromVal (VList x) = map fromVal x

instance IsoValue a => IsoValue (Maybe a) where
  type ToT (Maybe a) = 'TOption (ToT a)
  toVal = VOption . fmap toVal
  fromVal (VOption x) = fmap fromVal x

instance (IsoValue l, IsoValue r) => IsoValue (Either l r)

instance (IsoValue a, IsoValue b) => IsoValue (a, b)

instance (Ord c, IsoCValue c) => IsoValue (Set c) where
  type ToT (Set c) = 'TSet (ToCT c)
  toVal = VSet . Set.map toCVal
  fromVal (VSet x) = Set.map fromCVal x

instance (Ord k, IsoCValue k, IsoValue v) => IsoValue (Map k v) where
  type ToT (Map k v) = 'TMap (ToCT k) (ToT v)
  toVal = VMap . Map.mapKeys toCVal . Map.map toVal
  fromVal (VMap x) = Map.map fromVal $ Map.mapKeys fromCVal x

instance IsoValue Operation where
  type ToT Operation = 'TOperation
  toVal = VOp
  fromVal (VOp x) = x


deriving newtype instance IsoValue a => IsoValue (Identity a)
deriving newtype instance IsoValue a => IsoValue (NamedF Identity a name)
deriving newtype instance IsoValue a => IsoValue (NamedF Maybe a name)

instance (IsoValue a, IsoValue b, IsoValue c) => IsoValue (a, b, c)
instance (IsoValue a, IsoValue b, IsoValue c, IsoValue d)
       => IsoValue (a, b, c, d)
instance (IsoValue a, IsoValue b, IsoValue c, IsoValue d, IsoValue e)
       => IsoValue (a, b, c, d, e)
instance (IsoValue a, IsoValue b, IsoValue c, IsoValue d, IsoValue e,
          IsoValue f)
       => IsoValue (a, b, c, d, e, f)
instance (IsoValue a, IsoValue b, IsoValue c, IsoValue d, IsoValue e,
          IsoValue f, IsoValue g)
       => IsoValue (a, b, c, d, e, f, g)

-- Types used specifically for conversion
----------------------------------------------------------------------------

-- | Since @Contract@ name is used to designate contract code, lets call
-- analogy of 'TContract' type as follows.
newtype ContractAddr (cp :: Kind.Type) =
  ContractAddr { unContractAddress :: Address }

instance IsoValue (ContractAddr cp) where
  type ToT (ContractAddr cp) = 'TContract (ToT cp)
  toVal = VContract . unContractAddress
  fromVal (VContract a) = ContractAddr a

newtype BigMap k v = BigMap { unBigMap :: Map k v }
  deriving stock (Eq, Show)
  deriving newtype (Default, Semigroup, Monoid)

instance (Ord k, IsoCValue k, IsoValue v) => IsoValue (BigMap k v) where
  type ToT (BigMap k v) = 'TBigMap (ToCT k) (ToT v)
  toVal = VBigMap . Map.mapKeys toCVal . Map.map toVal . unBigMap
  fromVal (VBigMap x) = BigMap $ Map.map fromVal $ Map.mapKeys fromCVal x

-- Generic magic
----------------------------------------------------------------------------

-- | Implements ADT conversion to Michelson value.
--
-- Thanks to Generic, Michelson representation will
-- be a balanced tree; this reduces average access time in general case.
--
-- A drawback of such approach is that, in theory, in new GHC version
-- generified representation may change; however, chances are small and
-- I (martoon) believe that contract versions will change much faster anyway.
class GIsoValue (x :: Kind.Type -> Kind.Type) where
  type GValueType x :: T
  gToValue :: x p -> Value (GValueType x)
  gFromValue :: Value (GValueType x) -> x p

instance GIsoValue x => GIsoValue (G.M1 t i x) where
  type GValueType (G.M1 t i x) = GValueType x
  gToValue = gToValue . G.unM1
  gFromValue = G.M1 . gFromValue

instance (GIsoValue x, GIsoValue y) => GIsoValue (x :+: y) where
  type GValueType (x :+: y) = 'TOr (GValueType x) (GValueType y)
  gToValue = VOr . \case
    L1 x -> Left (gToValue x)
    R1 y -> Right (gToValue y)
  gFromValue (VOr e) = case e of
    Left vx -> L1 (gFromValue vx)
    Right vy -> R1 (gFromValue vy)

instance (GIsoValue x, GIsoValue y) => GIsoValue (x :*: y) where
  type GValueType (x :*: y) = 'TPair (GValueType x) (GValueType y)
  gToValue (x :*: y) = VPair (gToValue x, gToValue y)
  gFromValue (VPair (vx, vy)) = gFromValue vx :*: gFromValue vy

instance GIsoValue G.U1 where
  type GValueType G.U1 = 'TUnit
  gToValue G.U1 = VUnit
  gFromValue VUnit = G.U1

instance IsoValue a => GIsoValue (G.Rec0 a) where
  type GValueType (G.Rec0 a) = ToT a
  gToValue = toVal . G.unK1
  gFromValue = G.K1 . fromVal
