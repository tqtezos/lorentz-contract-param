module Michelson.Printer.Util
  ( RenderDoc(..)
  , printDoc
  , renderOps
  , renderOpsList
  , spaces
  , wrapInParens
  , buildRenderDoc
  ) where

import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Builder (Builder)
import Text.PrettyPrint.Leijen.Text
  (Doc, SimpleDoc, braces, displayB, displayT, hcat, isEmpty, parens, punctuate, renderOneLine,
  semi, space, vcat, (<+>))

-- | Generalize converting a type into a
-- Text.PrettyPrint.Leijen.Text.Doc. Used to pretty print Michelson code
-- and define Fmt.Buildable instances.
class RenderDoc a where
  renderDoc :: a -> Doc
  -- | Whether a value can be represented in Michelson code.
  -- Normally either all values of some type are renderable or not renderable.
  -- However, in case of instructions we have extra instructions which should
  -- not be rendered.
  -- Note: it's not suficcient to just return 'mempty' for such instructions,
  -- because sometimes we want to print lists of instructions and we need to
  -- ignore them complete (to avoid putting redundant separators).
  isRenderable :: a -> Bool
  isRenderable _ = True

-- | Convert 'Doc' to 'Text' with a line width of 80.
printDoc :: Doc -> LT.Text
printDoc = displayT . doRender

-- | Generic way to render the different op types that get passed
-- to a contract.
renderOps :: (RenderDoc op) => Bool -> NonEmpty op -> Doc
renderOps oneLine = renderOpsList oneLine . toList

spacecat :: NonEmpty Doc -> Doc
spacecat = foldr (<+>) mempty

renderOpsList :: (RenderDoc op) => Bool -> [op] -> Doc
renderOpsList oneLine ops =
  braces $ cat' $ punctuate semi (renderDoc <$> filter isRenderable ops)
  where
    cat' = if oneLine then maybe "" spacecat . nonEmpty else vcat

-- | Create a specific number of spaces.
spaces :: Int -> Doc
spaces x = hcat $ replicate x space

-- | Wrap documents in parentheses if there are two or more in the list.
wrapInParens :: NonEmpty Doc -> Doc
wrapInParens ds =
  if (length $ filter (not . isEmpty) (toList ds)) > 1
    then parens $ foldr (<+>) mempty ds
    else foldr (<+>) mempty ds

-- | Turn something that is instance of `RenderDoc` into a `Builder`.
-- It's formatted the same way as `printDoc` formats docs.
buildRenderDoc :: RenderDoc a => a -> Builder
buildRenderDoc = displayB . doRender . renderDoc

doRender :: Doc -> SimpleDoc
doRender = renderOneLine
