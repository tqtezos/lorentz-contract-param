{-# LANGUAGE QuantifiedConstraints #-}

-- | Type families from 'Michelson.Typed.Polymorphic' lifted to Haskell types.
module Lorentz.Polymorphic
  ( MemOpHs (..)
  , MapOpHs (..)
  , IterOpHs (..)
  , SizeOpHs
  , UpdOpHs (..)
  , GetOpHs (..)
  , ConcatOpHs
  , SliceOpHs
  , EDivOpHs (..)

  , IsoMapOpRes
  ) where

import qualified Data.Kind as Kind

import Michelson.Typed
import Tezos.Core (Mutez)

-- | Lifted 'MemOpKey'.
class ( MemOp (ToT c)
      , ToT (MemOpKeyHs c) ~ 'Tc (MemOpKey (ToT c))
      ) => MemOpHs c where
  type MemOpKeyHs c :: Kind.Type

instance IsComparable e => MemOpHs (Set e) where
  type MemOpKeyHs (Set e) = e

instance IsComparable k => MemOpHs (Map k v) where
  type MemOpKeyHs (Map k v) = k

instance IsComparable k => MemOpHs (BigMap k v) where
  type MemOpKeyHs (BigMap k v) = k

-- | A useful property which holds for reasonable 'MapOp' instances.
--
-- It's a separate thing from 'MapOpHs' because it mentions @b@ type parameter.
type family IsoMapOpRes c b where
  IsoMapOpRes c b = ToT (MapOpResHs c b) ~ MapOpRes (ToT c) (ToT b)

-- | Lifted 'MapOp'.
class ( MapOp (ToT c)
      , ToT (MapOpInpHs c) ~ MapOpInp (ToT c)
      , ToT (MapOpResHs c ()) ~ MapOpRes (ToT c) (ToT ())
      ) => MapOpHs c where
  type MapOpInpHs c :: Kind.Type
  type MapOpResHs c :: Kind.Type -> Kind.Type

instance IsComparable k => MapOpHs (Map k v) where
  type MapOpInpHs (Map k v) = (k, v)
  type MapOpResHs (Map k v) = Map k

instance MapOpHs [e] where
  type MapOpInpHs [e] = e
  type MapOpResHs [e] = []

-- | Lifted 'IterOp'.
class ( IterOp (ToT c)
      , ToT (IterOpElHs c) ~ IterOpEl (ToT c)
      ) => IterOpHs c where
  type IterOpElHs c :: Kind.Type

instance IsComparable k => IterOpHs (Map k v) where
  type IterOpElHs (Map k v) = (k, v)

instance IterOpHs [e] where
  type IterOpElHs [e] = e

instance IsComparable e => IterOpHs (Set e) where
  type IterOpElHs (Set e) = e

-- | Lifted 'SizeOp'.
--
-- This could be just a constraint alias, but to avoid 'T' types appearance in
-- error messages we make a full type class with concrete instances.
class SizeOp (ToT c) => SizeOpHs c

instance SizeOpHs Text
instance SizeOpHs ByteString
instance SizeOpHs (Set a)
instance SizeOpHs [a]
instance SizeOpHs (Map k v)

-- | Lifted 'UpdOp'.
class ( UpdOp (ToT c)
      , ToT (UpdOpKeyHs c) ~ 'Tc (UpdOpKey (ToT c))
      , ToT (UpdOpParamsHs c) ~ UpdOpParams (ToT c)
      ) => UpdOpHs c where
  type UpdOpKeyHs c :: Kind.Type
  type UpdOpParamsHs c :: Kind.Type

instance IsComparable k => UpdOpHs (Map k v) where
  type UpdOpKeyHs (Map k v) = k
  type UpdOpParamsHs (Map k v) = Maybe v

instance IsComparable k => UpdOpHs (BigMap k v) where
  type UpdOpKeyHs (BigMap k v) = k
  type UpdOpParamsHs (BigMap k v) = Maybe v

instance IsComparable a => UpdOpHs (Set a) where
  type UpdOpKeyHs (Set a) = a
  type UpdOpParamsHs (Set a) = Bool

-- | Lifted 'GetOp'.
class ( GetOp (ToT c)
      , ToT (GetOpKeyHs c) ~ 'Tc (GetOpKey (ToT c))
      , ToT (GetOpValHs c) ~ GetOpVal (ToT c)
      ) => GetOpHs c where
  type GetOpKeyHs c :: Kind.Type
  type GetOpValHs c :: Kind.Type

instance IsComparable k => GetOpHs (Map k v) where
  type GetOpKeyHs (Map k v) = k
  type GetOpValHs (Map k v) = v

instance IsComparable k => GetOpHs (BigMap k v) where
  type GetOpKeyHs (BigMap k v) = k
  type GetOpValHs (BigMap k v) = v

-- | Lifted 'ConcatOp'.
class ConcatOp (ToT c) => ConcatOpHs c

instance ConcatOpHs Text
instance ConcatOpHs ByteString

-- | Lifted 'SliceOp'.
class SliceOp (ToT c) => SliceOpHs c

instance SliceOpHs Text
instance SliceOpHs ByteString

-- | Lifted 'EDivOp'.
class ( EDivOp (ToCT n) (ToCT m)
      , IsComparable n, IsComparable m
      , ToT (EDivOpResHs n m) ~ 'Tc (EDivOpRes (ToCT n) (ToCT m))
      , ToT (EModOpResHs n m) ~ 'Tc (EModOpRes (ToCT n) (ToCT m))
      ) => EDivOpHs n m where
  type EDivOpResHs n m :: Kind.Type
  type EModOpResHs n m :: Kind.Type

instance EDivOpHs Integer Integer where
  type EDivOpResHs Integer Integer = Integer
  type EModOpResHs Integer Integer = Natural

instance EDivOpHs Integer Natural where
  type EDivOpResHs Integer Natural = Integer
  type EModOpResHs Integer Natural = Natural

instance EDivOpHs Natural Integer where
  type EDivOpResHs Natural Integer = Integer
  type EModOpResHs Natural Integer = Natural

instance EDivOpHs Natural Natural where
  type EDivOpResHs Natural Natural = Natural
  type EModOpResHs Natural Natural = Natural

instance EDivOpHs Mutez Mutez where
  type EDivOpResHs Mutez Mutez = Natural
  type EModOpResHs Mutez Mutez = Mutez

instance EDivOpHs Mutez Natural where
  type EDivOpResHs Mutez Natural = Mutez
  type EModOpResHs Mutez Natural = Mutez
