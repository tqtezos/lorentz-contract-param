{-# LANGUAGE DataKinds, GADTs #-}

-- | Module, providing @Notes t@ data type, which holds annotations for a
-- given type @t@.
--
-- Annotation type @Notes t@ is a tree, each leaf is either a star (@*@) or a
-- constructor holding some annotation data for a given type @t@.
-- Star corresponds to the case when given Michelson type contains no
-- annotations.
--
-- This module also provides type class 'Converge' along with some
-- utility functions which are used to combine two annotations trees
-- `a` and `b` into a new one `c` in such a way that `c` can be obtained from
-- both `a` and `b` by replacing some @*@ leafs with type or/and field
-- annotations.

module Michelson.Typed.Annotation
  ( Notes (..)
  , Notes' (..)
  , converge
  , convergeAnns
  , notesCase
  , isStar
  , mkNotes
  , orAnn
  ) where

import Data.Default (Default(..))

import Michelson.Typed.T (T(..))
import Michelson.Untyped.Annotation (Annotation, FieldAnn, TypeAnn, unifyAnn)

-- | Data type, holding annotation data for a given Michelson type @t@
-- or @*@ in case no data is provided for the tree.
--
-- There is a little semantical duplication between data type constructors.
-- Semantics behind 'NStar' constructor are exactly same as semantics behind
-- 'N' constructor with relevant 'Notes'' constructor be given all default
-- values (which means all annotations are empty).
--
-- Constructor 'NStar' is given as a tiny optimization to allow handling
-- no-annotation case completely for free (see 'converge' and 'mkNotes'
-- functions).
data Notes t = N (Notes' t) | NStar

-- | Helper function for work with 'Notes' data type.
--
-- @
--  notesCase f g notes
-- @
--
-- is equivalent to
--
-- @
--  case notes of
--    NStar -> f
--    N v -> g v
-- @
--
notesCase :: r -> (Notes' t -> r) -> Notes t -> r
notesCase a _ NStar = a
notesCase _ f (N b) = f b

-- | Data type, holding annotation data for a given Michelson type @t@.
--
-- Each constructor corresponds to exactly one constructor of 'T'
-- and holds all type and field annotations that can be attributed to a
-- Michelson type corrspoding to @t@.
data Notes' t where
  NTc         :: TypeAnn -> Notes' ('Tc ct)
  NTKey       :: TypeAnn -> Notes' 'TKey
  NTUnit      :: TypeAnn -> Notes' 'TUnit
  NTSignature :: TypeAnn -> Notes' 'TSignature
  NTOption    :: TypeAnn -> FieldAnn -> Notes t -> Notes' ('TOption t)
  NTList      :: TypeAnn -> Notes t -> Notes' ('TList t)
  NTSet       :: TypeAnn -> TypeAnn -> Notes' ('TSet ct)
  NTOperation :: TypeAnn -> Notes' 'TOperation
  NTContract  :: TypeAnn -> Notes t -> Notes' ('TContract t)
  NTPair      :: TypeAnn -> FieldAnn -> FieldAnn
               -> Notes p -> Notes q -> Notes' ('TPair p q)
  NTOr        :: TypeAnn -> FieldAnn -> FieldAnn
               -> Notes p -> Notes q -> Notes' ('TOr p q)
  NTLambda    :: TypeAnn -> Notes p -> Notes q -> Notes' ('TLambda p q)
  NTMap       :: TypeAnn -> TypeAnn -> Notes v -> Notes' ('TMap k v)
  NTBigMap   :: TypeAnn -> TypeAnn -> Notes v -> Notes' ('TBigMap k v)

-- | Check whether given annotations object is @*@.
isStar :: Notes t -> Bool
isStar NStar = True
isStar _ = False

isDef :: (Eq t, Default t) => t -> Bool
isDef = (== def)

-- | Checks whether given notes @n@ can be immediately converted to star
-- and returns either @NStar@ or @N n@.
--
-- Given @n :: Notes' t@ can be immediately converted to star iff all nested
-- @(sn :: Notes t) == NStar@ and for each annotation @an@: @an == def@.
mkNotes :: Notes' t -> Notes t
mkNotes (NTOption tn fn ns) | isStar ns && isDef tn && isDef fn   = NStar
mkNotes (NTList tn ns)      | isStar ns && isDef tn               = NStar
mkNotes (NTSet tn en)       | isDef tn && isDef en                = NStar
mkNotes (NTContract tn ns)  | isStar ns && isDef tn               = NStar
mkNotes (NTPair tn fn1 fn2 ns1 ns2)
  | isStar ns1 && isStar ns2 && isDef tn && isDef fn1 && isDef fn2 = NStar
mkNotes (NTOr tn fn1 fn2 ns1 ns2)
  | isStar ns1 && isStar ns2 && isDef tn && isDef fn1 && isDef fn2 = NStar
mkNotes (NTLambda tn ns1 ns2)
  | isStar ns1 && isStar ns2 && isDef tn                           = NStar
mkNotes (NTMap tn kn vns)
  | isStar vns && isDef tn && isDef kn                             = NStar
mkNotes (NTc t) | isDef t                                         = NStar
mkNotes (NTKey t) | isDef t                                       = NStar
mkNotes (NTUnit t) | isDef t                                      = NStar
mkNotes (NTSignature t) | isDef t                                 = NStar
mkNotes (NTOperation t) | isDef t                                 = NStar
mkNotes n = N n

orAnn :: Annotation t -> Annotation t -> Annotation t
orAnn a b = bool a b (a == def)

-- | Combines two annotations trees `a` and `b` into a new one `c`
-- in such a way that `c` can be obtained from both `a` and `b` by replacing
-- some @*@ leafs with type or/and field annotations.
converge' :: Notes' t -> Notes' t -> Either Text (Notes' t)
converge' (NTc a) (NTc b) = NTc <$> convergeAnns a b
converge' (NTKey a) (NTKey b) = NTKey <$> convergeAnns a b
converge' (NTUnit a) (NTUnit b) = NTUnit <$> convergeAnns a b
converge' (NTSignature a) (NTSignature b) =
    NTSignature <$> convergeAnns a b
converge' (NTOption a f n) (NTOption b g m) =
  NTOption <$> convergeAnns a b <*> convergeAnns f g <*> converge n m
converge' (NTList a n) (NTList b m) =
  NTList <$> convergeAnns a b <*> converge n m
converge' (NTSet a n) (NTSet b m) =
  NTSet <$> convergeAnns a b <*> convergeAnns n m
converge' (NTOperation a) (NTOperation b) =
  NTOperation <$> convergeAnns a b
converge' (NTContract a n) (NTContract b m) =
  NTContract <$> convergeAnns a b <*> converge n m
converge' (NTPair a pF qF pN qN) (NTPair b pG qG pM qM) =
  NTPair <$> convergeAnns a b <*> convergeAnns pF pG
          <*> convergeAnns qF qG <*> converge pN pM <*> converge qN qM
converge' (NTOr a pF qF pN qN) (NTOr b pG qG pM qM) =
  NTOr <$> convergeAnns a b <*> convergeAnns pF pG <*> convergeAnns qF qG
          <*> converge pN pM <*> converge qN qM
converge' (NTLambda a pN qN) (NTLambda b pM qM) =
  NTLambda <$> convergeAnns a b <*> converge pN pM <*> converge qN qM
converge' (NTMap a kN vN) (NTMap b kM vM) =
  NTMap <$> convergeAnns a b <*> convergeAnns kN kM <*> converge vN vM
converge' (NTBigMap a kN vN) (NTBigMap b kM vM) =
  NTBigMap <$> convergeAnns a b <*> convergeAnns kN kM <*> converge vN vM

-- | Same as 'converge'' but works with 'Notes' data type.
converge :: Notes t -> Notes t -> Either Text (Notes t)
converge NStar a = pure a
converge a NStar = pure a
converge (N a) (N b) = N <$> converge' a b

-- | Converge two type or field notes (which may be wildcards).
convergeAnns
  :: Show (Annotation tag)
  => Annotation tag -> Annotation tag -> Either Text (Annotation tag)
convergeAnns a b = maybe (Left $ "Annotations do not converge: "
                            <> show a <> " /= " <> show b)
                          pure $ unifyAnn a b
