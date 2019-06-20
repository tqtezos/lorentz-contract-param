{-# LANGUAGE DeriveAnyClass, DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Conversion between 'UStore' in Haskell and Michelson representation.
module Lorentz.UStore.Haskell
  ( UStoreContent
  , UStoreConversible
  , mkUStore
  , ustoreDecompose
  , ustoreDecomposeFull
  ) where

import qualified Data.Kind as Kind
import qualified Data.List as L
import qualified Data.Map as Map
import Data.Singletons (demote)
import Fmt ((+|), (+||), (|+), (||+))
import GHC.Generics ((:*:)(..), (:+:)(..))
import qualified GHC.Generics as G
import GHC.TypeLits (ErrorMessage(..), KnownSymbol, TypeError, symbolVal)

import Lorentz.Constraints
import Lorentz.UStore.Common
import Lorentz.UStore.Types
import Michelson.Interpret.Pack
import Michelson.Interpret.Unpack
import Michelson.Text
import Michelson.Typed.Haskell.Value
import Michelson.Typed.Scope

-- | 'UStore' content represented as key-value pairs.
type UStoreContent = [(ByteString, ByteString)]

-- | Make 'UStore' from separate @big_map@s and fields.
mkUStore
  :: (Generic template, UStoreConversible template)
  => template -> UStore template
mkUStore = UStore . BigMap . mkUStoreRec

-- | Decompose 'UStore' into separate @big_map@s and fields.
--
-- Since this function needs to @UNPACK@ content of @UStore@ to actual
-- keys and values, you have to provide 'UnpackEnv'.
--
-- Along with resulting value, you get a list of @UStore@ entries which
-- were not recognized as belonging to any submap or field according to
-- @UStore@'s template - this should be empty unless @UStore@ invariants
-- were violated.
ustoreDecompose
  :: forall template.
     (Generic template, UStoreConversible template)
  => UnpackEnv -> UStore template -> Either Text (UStoreContent, template)
ustoreDecompose ue = storeDecomposeRec ue . Map.toList . unBigMap . unUStore

-- | Like 'ustoreDecompose', but requires all entries from @UStore@ to be
-- recognized.
ustoreDecomposeFull
  :: forall template.
     (Generic template, UStoreConversible template)
  => UnpackEnv -> UStore template -> Either Text template
ustoreDecomposeFull ue ustore = do
  (remained, res) <- ustoreDecompose ue ustore
  unless (null remained) $
    Left $ "Unrecognized entries in UStore: " +|| remained ||+ ""
  return res

-- | Recursive template traversal for 'mkUStore'.
mkUStoreRec
  :: (Generic template, UStoreConversible template)
  => template -> Map ByteString ByteString
mkUStoreRec = gUstoreToVal . G.from

-- | Recursive template traversal for 'ustoreDecompose'.
storeDecomposeRec
  :: forall template.
     (Generic template, UStoreConversible template)
  => UnpackEnv -> UStoreContent -> Either Text (UStoreContent, template)
storeDecomposeRec = fmap (second G.to) ... gUstoreFromVal

-- | Given template can be converted to 'UStore' value.
class GUStoreConversible (G.Rep template) => UStoreConversible template
instance GUStoreConversible (G.Rep template) => UStoreConversible template

-- | Generic traversal for 'mkUStore' and 'ustoreDecompose'.
class GUStoreConversible (template :: Kind.Type -> Kind.Type) where
  -- | Convert generic value to internal 'UStore' representation.
  gUstoreToVal :: template p -> Map ByteString ByteString

  -- | Parse internal 'UStore' representation into generic Haskell value of
  -- 'UStore', also returning unparsed entries.
  gUstoreFromVal
    :: UnpackEnv
    -> UStoreContent
    -> Either Text (UStoreContent, template p)

instance GUStoreConversible x => GUStoreConversible (G.D1 i x) where
  gUstoreToVal = gUstoreToVal . G.unM1
  gUstoreFromVal = fmap (second G.M1) ... gUstoreFromVal

instance GUStoreConversible x => GUStoreConversible (G.C1 i x) where
  gUstoreToVal = gUstoreToVal . G.unM1
  gUstoreFromVal = fmap (second G.M1) ... gUstoreFromVal

instance TypeError ('Text "Unexpected sum type in UStore template") =>
         GUStoreConversible (x :+: y) where
  gUstoreToVal = error "impossible"
  gUstoreFromVal = error "impossible"

instance TypeError ('Text "UStore template should have one constructor") =>
         GUStoreConversible G.V1 where
  gUstoreToVal = error "impossible"
  gUstoreFromVal = error "impossible"

instance (GUStoreConversible x, GUStoreConversible y) =>
         GUStoreConversible (x :*: y) where
  gUstoreToVal (x :*: y) = gUstoreToVal x <> gUstoreToVal y
  gUstoreFromVal unpackEnv entries = do
    (entries', res1) <- gUstoreFromVal unpackEnv entries
    (entries'', res2) <- gUstoreFromVal unpackEnv entries'
    return (entries'', res1 :*: res2)

instance GUStoreConversible G.U1 where
  gUstoreToVal G.U1 = mempty
  gUstoreFromVal _ entries = pure (entries, G.U1)

-- | Case of nested template.
instance {-# OVERLAPPABLE #-}
         (Generic template, UStoreConversible template) =>
         GUStoreConversible (G.S1 i (G.Rec0 template)) where
  gUstoreToVal = mkUStoreRec . G.unK1 . G.unM1
  gUstoreFromVal = fmap (second $ G.M1 . G.K1) ... storeDecomposeRec

-- | Case of '|~>'.
instance ( Each [IsoValue, KnownValue, NoOperation, NoBigMap] [k, v]
         , KnownSymbol fieldName, Ord k
         ) =>
         GUStoreConversible (G.S1 ('G.MetaSel ('Just fieldName) _1 _2 _3)
                                  (G.Rec0 (k |~> v))) where
  gUstoreToVal (G.M1 (G.K1 (UStoreSubMap m))) =
    forbiddenOp @(ToT k) $ forbiddenBigMap @(ToT k) $
    forbiddenOp @(ToT v) $ forbiddenBigMap @(ToT v) $
      mconcat
        [ one ( packValue' $ toVal (fieldNameToMText @fieldName, k)
              , packValue' $ toVal v
              )
        | (k, v) <- Map.toList m
        ]

  gUstoreFromVal unpackEnv allEntries = do
    (unrecognized, res) <- foldM parseEntry (mempty, mempty) allEntries
    return (unrecognized, G.M1 . G.K1 $ UStoreSubMap res)
    where
    parseEntry
      :: (UStoreContent, Map k v)
      -> (ByteString, ByteString)
      -> Either Text (UStoreContent, Map k v)
    parseEntry (entries, !acc) entry@(key, val) =
      forbiddenOp @(ToT k) $ forbiddenBigMap @(ToT k) $
      forbiddenOp @(ToT v) $ forbiddenBigMap @(ToT v) $

        case unpackValue' unpackEnv key of
          Left _ -> Right (entry : entries, acc)
          Right (fromVal -> (name :: MText, keyValue :: k))
            | toText name /= toText (symbolVal $ Proxy @fieldName) ->
                Right (entry : entries, acc)
            | otherwise ->
              case unpackValue' unpackEnv val of
                Left err ->
                  Left $ "Failed to parse UStore value for " +|
                        demote @(ToT k) |+ " |~> " +| demote @(ToT v) |+
                        ": " +| err |+ ""
                Right (fromVal -> valValue) ->
                  Right (entries, Map.insert keyValue valValue acc)

-- | Case of 'UStoreField'.
instance ( Each [IsoValue, KnownValue, NoOperation, NoBigMap] '[v]
         , KnownSymbol fieldName
         ) =>
         GUStoreConversible (G.S1 ('G.MetaSel ('Just fieldName) _1 _2 _3)
                                  (G.Rec0 (UStoreField v))) where
  gUstoreToVal (G.M1 (G.K1 (UStoreField val))) =
    forbiddenOp @(ToT v) $ forbiddenBigMap @(ToT v) $
      one ( packValue' $ toVal (fieldNameToMText @fieldName)
          , packValue' $ toVal val
          )

  gUstoreFromVal unpackEnv entries =
    forbiddenOp @(ToT v) $ forbiddenBigMap @(ToT v) $
      let key = packValue' $ toVal (fieldNameToMText @fieldName)
      in case L.partition ((== key) . fst) entries of
          ([], _) ->
            Left $ "Failed to find field in UStore: " +|
                  fieldNameToMText @fieldName |+ ""
          ([(_, val)], otherEntries) ->
            case unpackValue' unpackEnv val of
              Left err ->
                Left $ "Failed to parse UStore value for field " +|
                      demote @(ToT v) |+ ": " +| err |+ ""
              Right (fromVal -> valValue) ->
                Right (otherEntries, G.M1 . G.K1 $ UStoreField valValue)
          (_ : _ : _, _) ->
            error "UStore content contained multiple entries with the same key"


-- Examples
----------------------------------------------------------------------------

data MyStoreTemplate = MyStoreTemplate
  { ints :: Integer |~> ()
  , flag :: UStoreField Bool
  }
  deriving stock (Generic)

data MyStoreTemplateBig = MyStoreTemplateBig
  { templ :: MyStoreTemplate
  , bytes :: ByteString |~> ByteString
  }
  deriving stock (Generic)

_storeSample :: UStore MyStoreTemplate
_storeSample = mkUStore
  MyStoreTemplate
  { ints = UStoreSubMap $ one (1, ())
  , flag = UStoreField False
  }

_storeSampleBig :: UStore MyStoreTemplateBig
_storeSampleBig = mkUStore $
  MyStoreTemplateBig
    MyStoreTemplate
      { ints = UStoreSubMap $ one (1, ())
      , flag = UStoreField False
      }
    (UStoreSubMap $ one ("a", "b"))
