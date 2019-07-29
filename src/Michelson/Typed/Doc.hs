{-# LANGUAGE DerivingStrategies, TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Renderable documentation injected to contract code.
module Michelson.Typed.Doc
  ( DocItem (..)
  , docItemPosition
  , DocItemId (..)
  , DocItemPlacementKind (..)
  , DocItemRef (..)
  , SomeDocItem (..)
  , SomeDocDefinitionItem (..)
  , DocBlock
  , DocGrouping
  , ContractDoc (..)
  , cdContentsL
  , cdDefinitionsL
  , cdDefinitionIdsL
  , docBlockToMarkdown
  , contractDocToMarkdown

  , DName (..)
  , DDescription (..)
  , DVersion (..)
  , DError (..)
  ) where

import qualified Data.Map as M
import qualified Data.Map.Merge.Strict as M
import qualified Data.Set as S
import qualified Data.Text.Lazy as LT
import Data.Typeable (cast, typeRep)
import Fmt (Buildable, Builder, build, fmt, (+|), (+||), (|+), (||+))
import qualified Text.Show

import Util.Instances ()
import Util.Lens
import Util.Markdown

-- | A piece of documentation describing one property of a thing,
-- be it a name or description of a contract, or an error throwable
-- by given endpoint.
--
-- Items of the same type appear close to each other in a rendered documentation
-- and form a /section/.
--
-- Doc items are later injected into a contract code via a dedicated nop-like
-- instruction. Normally doc items which belong to one section appear in
-- resulting doc in the same order in which they appeared in the contract.
class (Typeable d, KnownNat (DocItemPosition d)) => DocItem d where
  -- | Position of this item in the resulting documentation;
  -- the smaller the value, the higher the section with this element
  -- will be placed.
  --
  -- Documentation structure is not necessarily flat.
  -- If some doc item consolidates a whole documentation block within it,
  -- this block will have its own placement of items independent from outer parts
  -- of the doc.
  type DocItemPosition d = (pos :: Nat) | pos -> d

  -- | When multiple items of the same type belong to one section, how
  -- this section will be called.
  --
  -- If not provided, section will contain just untitled content.
  docItemSectionName :: Maybe Text

  -- | Description of a section.
  --
  -- Can be used to mention some common things about all elements of this section.
  -- Markdown syntax is permitted here.
  docItemSectionDescription :: Maybe Builder
  docItemSectionDescription = Nothing

  -- | Defines where given doc item should be put. There are two options:
  -- 1. Inline right here (default behaviour);
  -- 2. Put into definitions section.
  type DocItemPlacement d :: DocItemPlacementKind
  type DocItemPlacement d = 'DocItemInlined

  -- | Defines a function which constructs an unique identifier of given doc item,
  -- if it has been decided to put the doc item into definitions section.
  --
  -- Identifier should be unique both among doc items of the same type and items
  -- of other types. Thus, consider using "typeId-contentId" pattern.
  docItemRef :: d -> DocItemRef (DocItemPlacement d)
  default docItemRef
    :: (DocItemPlacement d ~ 'DocItemInlined)
    => d -> DocItemRef (DocItemPlacement d)
  docItemRef _ = DocItemNoRef

  -- | Render given doc item to Markdown, preferably one line,
  -- optionally with header.
  --
  -- Accepts the smallest allowed level of header.
  -- (Using smaller value than provided one will interfere with existing
  -- headers thus delivering mess).
  docItemToMarkdown :: HeaderLevel -> d -> Builder

  -- | All doc items which this doc item refers to.
  --
  -- They will automatically be put to definitions as soon as given doc item
  -- is detected.
  docItemDependencies :: d -> [SomeDocDefinitionItem]
  docItemDependencies _ = []

  -- | This function accepts doc items put under the same section in the order
  -- in which they appeared in the contract and returns their new desired order.
  -- It's also fine to use this function for filtering or merging doc items.
  --
  -- Default implementation
  -- * leaves inlined items as is;
  -- * for items put to definitions, lexicographically sorts them by their id.
  docItemsOrder :: [d] -> [d]
  docItemsOrder = \case
    [] -> []
    docItems@(someDocItem : _) -> case docItemRef someDocItem of
      DocItemNoRef -> docItems
      DocItemRef _ -> docItemsOrderById docItems

-- | Get doc item position at term-level.
docItemPosition :: forall d. DocItem d => DocItemPos
docItemPosition = DocItemPos $ natVal (Proxy @(DocItemPosition d))

-- | Render an item into Markdown block with all required adjustments.
docItemToMarkdownFull :: DocItem d => HeaderLevel -> d -> Builder
docItemToMarkdownFull l d =
  manchor <> docItemToMarkdown l d <> "\n\n"
  where
    manchor = case docItemRef d of
      DocItemRef (DocItemId docItemId) -> mdAnchor docItemId
      DocItemNoRef -> ""

-- | Order items by their 'docItemId'.
docItemsOrderById
  :: forall d. (DocItem d, DocItemPlacement d ~ 'DocItemInDefinitions)
  => [d] -> [d]
docItemsOrderById docItems =
  let getDocItemId :: d -> DocItemId
      getDocItemId d = case docItemRef d of { DocItemRef di -> di }
  in sortOn getDocItemId docItems

-- | Some unique identifier of a doc item.
--
-- All doc items which should be refer-able need to have this identifier.
newtype DocItemId = DocItemId Text
  deriving stock (Eq, Ord, Show)

-- | Position of all doc items of some type.
newtype DocItemPos = DocItemPos Natural
  deriving stock (Eq, Ord, Show)
  deriving newtype (Buildable)

-- | Where do we place given doc item.
data DocItemPlacementKind
  = DocItemInlined
    -- ^ Placed in the document content itself.
  | DocItemInDefinitions
    -- ^ Placed in dedicated definitions section; can later be referenced.

-- | Defines an identifier which given doc item can be referenced with.
data DocItemRef (p :: DocItemPlacementKind) where
  DocItemRef :: DocItemId -> DocItemRef 'DocItemInDefinitions
  DocItemNoRef :: DocItemRef 'DocItemInlined

-- | Hides some documentation item.
data SomeDocItem where
  SomeDocItem :: DocItem d => d -> SomeDocItem

-- | Hides some documentation item which is put to "definitions" section.
data SomeDocDefinitionItem where
  SomeDocDefinitionItem
    :: (DocItem d, DocItemPlacement d ~ 'DocItemInDefinitions)
    => d -> SomeDocDefinitionItem

-- | To automatically derive @instance Show Michelson.Typed.Instr@ later.
instance Show SomeDocItem where
  show _ = "<doc item>"

-- | A map from positions to document elements.
--
-- Note that each value in this map keeps a list of doc items, all of which
-- have the same type (since each doc item type is forced to have unique position).
type DocBlock = Map DocItemPos (NonEmpty SomeDocItem)

-- | Make sure several 'SomeDocItem's have the same type inside.
unifyDocItemsUnsafe
  :: HasCallStack
  => NonEmpty SomeDocItem
  -> (forall d. DocItem d => NonEmpty d -> r)
  -> r
unifyDocItemsUnsafe (SomeDocItem (firstDocItem :: d) :| docItems) cont =
  let
    dis = docItems <&> \(SomeDocItem di) ->
      cast di ?: error (castErr di)
  in cont $ firstDocItem :| dis
  where
    castErr :: Typeable d' => d' -> Text
    castErr (_ :: d') =
      "Non-homogenious doc block: found two doc items of different types: "
      +|| typeRep (Proxy @d) ||+ " and " +|| typeRep (Proxy @d') ||+ ""

-- | Render a documentation block.
docBlockToMarkdown :: HeaderLevel -> DocBlock -> Builder
docBlockToMarkdown hl block =
  mconcat $ M.elems block <&> \items@(SomeDocItem (_ :: di) :| _) ->
    let sectionName = docItemSectionName @di
        (sectionNameFull, headerLevelDelta) =
          case sectionName of
            Nothing -> ("", id)
            Just sn -> (mdHeader hl (build sn), nextHeaderLevel)
        sectionDesc = docItemSectionDescription @di
        sectionDescFull =
          case sectionDesc of
            Nothing -> ""
            Just sd -> sd <> "\n\n"
        content =
          unifyDocItemsUnsafe items $ \dis ->
            mconcat $ docItemsOrder (toList dis) <&> \di ->
              docItemToMarkdownFull (headerLevelDelta hl) di
    in sectionNameFull <> sectionDescFull <> content

-- | A function which groups a piece of doc under one doc item.
type DocGrouping = DocBlock -> SomeDocItem

instance Show DocGrouping where
  show _ = "<doc grouping>"

-- | Keeps documentation gathered for some piece of contract code.
--
-- Used for building documentation of a contract.
data ContractDoc = ContractDoc
  { cdContents :: DocBlock
    -- ^ All inlined doc items.
  , cdDefinitions :: DocBlock
    -- ^ Definitions used in document.
    --
    -- Usually you put some large and repetitive descriptions here.
    -- This differs from the document content in that
    -- it contains sections which are always at top-level,
    -- disregard the nesting.
    --
    -- All doc items which define 'docItemId' method go here, and only they.
  , cdDefinitionIds :: Set DocItemId
    -- ^ We remember all already used identifiers to avoid repetitions.
  }

makeLensesWith postfixLFields ''ContractDoc

-- | Contract documentation assembly primarily relies on this instance.
instance Semigroup ContractDoc where
  cd1 <> cd2 = ContractDoc
    { cdContents =
        M.merge
          M.preserveMissing M.preserveMissing
          (M.zipWithMatched $ \_k l r -> l <> r)
          (cdContents cd1) (cdContents cd2)
    , cdDefinitions =
        M.merge
          M.preserveMissing M.preserveMissing
          (M.zipWithMatched $ \_k (l :| ls) rs ->
             let removeDups = filter $ not . (`isDefinedIn` cdDefinitionIds cd1)
             in l :| ls <> removeDups (toList rs)
          )
          (cdDefinitions cd1) (cdDefinitions cd2)
    , cdDefinitionIds =
        S.union (cdDefinitionIds cd1) (cdDefinitionIds cd2)
    }
    where
      isDefinedIn :: SomeDocItem -> Set DocItemId -> Bool
      isDefinedIn (SomeDocItem di) defs =
        case docItemRef di of
          DocItemNoRef -> False
          DocItemRef docItemId -> docItemId `S.member` defs

instance Monoid ContractDoc where
  mempty = ContractDoc
    { cdContents = M.empty
    , cdDefinitions = M.empty
    , cdDefinitionIds = S.empty
    }

-- | Render given contract documentation to markdown document.
contractDocToMarkdown :: ContractDoc -> LText
contractDocToMarkdown ContractDoc{..} =
  let
    contents =
      docBlockToMarkdown (HeaderLevel 1) cdContents |+ "\n\n"
    definitions
      | null cdDefinitions = ""
      | otherwise =
        "# Definitions\n\n" +| docBlockToMarkdown (HeaderLevel 2) cdDefinitions
    total = fmt (contents <> definitions)
  in LT.strip total <> "\n"

----------------------------------------------------------------------------
-- Basic doc items
----------------------------------------------------------------------------

-- | Give a name to document block.
data DName = DName Text DocBlock

instance DocItem DName where
  type DocItemPosition DName = 1
  docItemSectionName = Nothing
  docItemToMarkdown lvl (DName name doc) =
    mdHeader lvl (build name) <>
    docBlockToMarkdown (nextHeaderLevel lvl) doc

-- | Description of something.
data DDescription = DDescription Text

instance DocItem DDescription where
  type DocItemPosition DDescription = 10
  docItemSectionName = Nothing
  docItemToMarkdown _ (DDescription txt) = build txt

-- | Specify version if given contract.
data DVersion = DVersion Natural

instance DocItem DVersion where
  type DocItemPosition DVersion = 3
  docItemSectionName = Nothing
  docItemToMarkdown _ (DVersion ver) =
    mdSubsection "Version" (build ver)

-- | An error thrown by a contract.
data DError = DError

instance DocItem DError where
  type DocItemPosition DError = 100
  docItemSectionName = Just "Errors"
  docItemToMarkdown _ (DError) = ""
