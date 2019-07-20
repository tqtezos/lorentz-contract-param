{-# LANGUAGE DeriveDataTypeable, DerivingStrategies #-}

-- | Michelson types represented in untyped model.

module Michelson.Untyped.Type
  ( Type (..)
  , Comparable (..)
  , compToType
  , typeToComp
  , T (..)
  , CT (..)
  , pattern Tint
  , pattern Tnat
  , pattern Tstring
  , pattern Tbytes
  , pattern Tmutez
  , pattern Tbool
  , pattern Tkey_hash
  , pattern Ttimestamp
  , pattern Taddress
  , tint
  , tnat
  , tstring
  , tbytes
  , tmutez
  , tbool
  , tkeyHash
  , ttimestamp
  , taddress
  , toption
  , tpair
  , tor
  , tyint
  , tynat
  , tyunit
  , tybool
  , typair
  , tyor
  , isAtomicType
  , isKey
  , isSignature
  , isComparable
  , isMutez
  , isKeyHash
  , isBool
  , isString
  , isInteger
  , isTimestamp
  , isNat
  , isInt
  , isBytes
  ) where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Data (Data(..))
import Formatting.Buildable (Buildable(build))
import Prelude hiding ((<$>))
import Text.PrettyPrint.Leijen.Text (Doc, align, parens, softbreak, (<$>), (<+>))

import Michelson.Printer.Util (Prettier(..), RenderDoc(..), buildRenderDoc, wrapInParens)
import Michelson.Untyped.Annotation (Annotation(..), FieldAnn, TypeAnn, noAnn)

-- Annotated type
data Type
  = Type T TypeAnn
  | TypeParameter
  -- ^ Implicit Parameter type which can be used in contract code
  | TypeStorage
  -- ^ Implicit Storage type which can be used in contract code
  deriving (Eq, Show, Data, Generic)

instance RenderDoc Comparable where
  renderDoc (Comparable ct ta) = renderDoc ct <+> renderDoc ta

instance RenderDoc (Prettier Type) where
  renderDoc (Prettier w) = case w of
    (Type t ta) -> renderType t False (Just ta) Nothing
    TypeParameter -> "Parameter"
    TypeStorage -> "Storage"

instance RenderDoc Type where
  renderDoc (Type t ta) = renderType t True (Just ta) Nothing
  renderDoc TypeParameter = "Parameter"
  renderDoc TypeStorage = "Storage"

instance RenderDoc T where
  renderDoc t = renderType t True Nothing Nothing

-- Ordering between different kinds of annotations is not significant,
-- but ordering among annotations of the same kind is. Annotations
-- of a same kind must be grouped together.
-- (prim @v :t %x arg1 arg2 ...)
-- these are equivalent
-- PAIR :t @my_pair %x %y
-- PAIR %x %y :t @my_pair
renderType :: T -> Bool -> Maybe TypeAnn -> Maybe FieldAnn -> Doc
renderType t forceSingleLine mta mfa =
  let rta = case mta of Just ta -> renderDoc ta; Nothing -> ""
      rfa = case mfa of Just fa -> renderDoc fa; Nothing -> ""
      renderer type_ mfa' =
        case type_ of
          Type tt ta -> renderType tt forceSingleLine (Just ta) mfa'
          tt -> renderDoc tt
      renderBranches d1 d2 =
        if forceSingleLine
        then (d1 <+> d2)
        else align $ softbreak <> (d1 <$> d2)
  in
  case t of
    Tc ct             -> wrapInParens $ renderDoc ct :| [rta, rfa]
    TKey              -> wrapInParens $ "key"  :| [rta, rfa]
    TUnit             -> wrapInParens $ "unit" :| [rta, rfa]
    TSignature        -> wrapInParens $ "signature" :| [rta, rfa]
    TOperation        -> wrapInParens $ "operation" :| [rta, rfa]
    TOption fa1 (Type t1 ta1) ->
      parens ("option" <+> rta <+> rfa
              <+> renderType t1 forceSingleLine (Just ta1) (Just fa1))
    TOption _fa1 t1 ->
      parens ("option" <+> rta <+> rfa <+> renderDoc t1)
    TList (Type t1 ta1)       ->
      parens ("list" <+> rta <+> rfa <+> renderType t1 forceSingleLine (Just ta1) Nothing)
    TList t1                  ->
      parens ("list" <+> rta <+> rfa <+> renderDoc t1)
    TSet (Comparable ct1 ta1) ->
      parens ("set" <+> rta <+> rfa <+> renderType (Tc ct1) forceSingleLine (Just ta1) Nothing)
    TContract (Type t1 ta1)   ->
      parens ("contract" <+> rta <+> rfa <+> renderType t1 forceSingleLine (Just ta1) Nothing)
    TContract t1              ->
      parens ("contract" <+> rta <+> rfa <+> renderDoc t1)

    TPair fa1 fa2 t1 t2 ->
      parens ("pair" <+> rta <+> rfa <+>
               (renderBranches
                 (renderer t1 (Just fa1)) (renderer t2 (Just fa2))))

    TOr fa1 fa2 t1 t2 ->
      parens ("or" <+> rta <+> rfa <+>
               (renderBranches
                  (renderer t1 (Just fa1)) (renderer t2 (Just fa2))))

    TLambda t1 t2 ->
      parens ("lambda" <+> rta <+> rfa <+>
               (renderBranches
                 (renderer t1 Nothing) (renderer t2 Nothing)))

    TMap (Comparable ct1 ta1) t2 ->
      parens ("map" <+> rta <+> rfa <+>
               (renderBranches
                  (renderType (Tc ct1) forceSingleLine (Just ta1) Nothing)
                  (renderer t2 Nothing)))

    TBigMap (Comparable ct1 ta1) t2 ->
      parens ("big_map" <+> rta <+> rfa <+>
               (renderBranches
                  (renderType (Tc ct1) forceSingleLine (Just ta1) Nothing)
                  (renderer t2 Nothing)))

instance RenderDoc CT where
  renderDoc = \case
    CInt       -> "int"
    CNat       -> "nat"
    CString    -> "string"
    CMutez     -> "mutez"
    CBool      -> "bool"
    CKeyHash   -> "key_hash"
    CTimestamp -> "timestamp"
    CBytes     -> "bytes"
    CAddress   -> "address"

instance Buildable Type where
  build = buildRenderDoc

-- Annotated Comparable Sub-type
data Comparable = Comparable CT TypeAnn
  deriving (Eq, Show, Data, Generic)

instance Buildable Comparable where
  build = buildRenderDoc

compToType :: Comparable -> Type
compToType (Comparable ct tn) = Type (Tc ct) tn

typeToComp :: Type -> Maybe Comparable
typeToComp (Type (Tc ct) tn) = Just $ Comparable ct tn
typeToComp _ = Nothing

-- Michelson Type
data T =
    Tc CT
  | TKey
  | TUnit
  | TSignature
  | TOption FieldAnn Type
  | TList Type
  | TSet Comparable
  | TOperation
  | TContract Type
  | TPair FieldAnn FieldAnn Type Type
  | TOr FieldAnn FieldAnn Type Type
  | TLambda Type Type
  | TMap Comparable Type
  | TBigMap Comparable Type
  deriving (Eq, Show, Data, Generic)

instance Buildable T where
  build = buildRenderDoc

-- Comparable Sub-Type
data CT =
    CInt
  | CNat
  | CString
  | CBytes
  | CMutez
  | CBool
  | CKeyHash
  | CTimestamp
  | CAddress
  deriving (Eq, Ord, Show, Data, Enum, Bounded, Generic)

instance Buildable CT where
  build = buildRenderDoc

pattern Tint :: T
pattern Tint <- Tc CInt

pattern Tnat :: T
pattern Tnat <- Tc CNat

pattern Tstring :: T
pattern Tstring <- Tc CString

pattern Tbytes :: T
pattern Tbytes <- Tc CBytes

pattern Tmutez :: T
pattern Tmutez <- Tc CMutez

pattern Tbool :: T
pattern Tbool <- Tc CBool

pattern Tkey_hash :: T
pattern Tkey_hash <- Tc CKeyHash

pattern Ttimestamp :: T
pattern Ttimestamp <- Tc CTimestamp

pattern Taddress :: T
pattern Taddress <- Tc CAddress

tint :: T
tint = Tc CInt

tnat :: T
tnat = Tc CNat

tstring :: T
tstring = Tc CString

tbytes :: T
tbytes = Tc CBytes

tmutez :: T
tmutez = Tc CMutez

tbool :: T
tbool = Tc CBool

tkeyHash :: T
tkeyHash = Tc CKeyHash

ttimestamp :: T
ttimestamp = Tc CTimestamp

taddress :: T
taddress = Tc CAddress

toption :: Type -> T
toption t = TOption noAnn t

tpair :: Type -> Type -> T
tpair l r = TPair noAnn noAnn l r

tor :: Type -> Type -> T
tor l r = TOr noAnn noAnn l r

tyint :: Type
tyint = Type tint noAnn

tynat :: Type
tynat = Type tnat noAnn

tyunit :: Type
tyunit = Type TUnit noAnn

tybool :: Type
tybool = Type tbool noAnn

typair :: Type -> Type -> Type
typair l r = Type (tpair l r) noAnn

tyor :: Type -> Type -> Type
tyor l r = Type (tor l r) noAnn

isAtomicType :: Type -> Bool
isAtomicType t@(Type _ (Annotation "")) =
    isComparable t || isKey t || isUnit t || isSignature t || isOperation t
isAtomicType _ = False

isKey :: Type -> Bool
isKey (Type TKey _) = True
isKey _              = False

isUnit :: Type -> Bool
isUnit (Type TUnit _) = True
isUnit _               = False

isSignature :: Type -> Bool
isSignature (Type TSignature _) = True
isSignature _                    = False

isOperation :: Type -> Bool
isOperation (Type TOperation _) = True
isOperation _                    = False

isComparable :: Type -> Bool
isComparable (Type (Tc _) _) = True
isComparable _ = False

isMutez :: Type -> Bool
isMutez (Type (Tc CMutez) _) = True
isMutez _ = False

isTimestamp :: Type -> Bool
isTimestamp (Type (Tc CTimestamp) _) = True
isTimestamp _ = False

isKeyHash :: Type -> Bool
isKeyHash (Type (Tc CKeyHash) _) = True
isKeyHash _ = False

isBool  :: Type -> Bool
isBool (Type (Tc CBool) _) = True
isBool _ = False

isString  :: Type -> Bool
isString (Type (Tc CString) _) = True
isString _ = False

isInteger :: Type -> Bool
isInteger a = isNat a || isInt a || isMutez a || isTimestamp a

isNat  :: Type -> Bool
isNat (Type (Tc CNat) _) = True
isNat _ = False

isInt  :: Type -> Bool
isInt (Type (Tc CInt) _) = True
isInt _ = False

isBytes :: Type -> Bool
isBytes (Type (Tc CBytes) _) = True
isBytes _ = False

----------------------------------------------------------------------------
-- JSON serialization
----------------------------------------------------------------------------

deriveJSON defaultOptions ''Type
deriveJSON defaultOptions ''Comparable
deriveJSON defaultOptions ''T
deriveJSON defaultOptions ''CT
