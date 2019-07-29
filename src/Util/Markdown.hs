-- | A small Markdown eDSL.
module Util.Markdown
  ( HeaderLevel (..)
  , nextHeaderLevel
  , mdHeader
  , mdSubsection
  , mdBold
  , mdItalic
  , mdTicked
  , mdLocalRef
  , mdAnchor
  , mdSeparator
  , mdSpoiler
  ) where

import Fmt (Builder, build, (+|), (|+))

-- | Level of header, starting from 1.
newtype HeaderLevel = HeaderLevel Int

nextHeaderLevel :: HeaderLevel -> HeaderLevel
nextHeaderLevel (HeaderLevel l) = HeaderLevel (l + 1)

mdHeader :: HeaderLevel -> Builder -> Builder
mdHeader (HeaderLevel lvl) text =
  mconcat (replicate lvl "#") +| " " +| text |+ "\n\n"

mdSubsection :: Builder -> Builder -> Builder
mdSubsection name txt = "**" <> name <> ":** " <> txt

mdBold :: Builder -> Builder
mdBold x = "**" <> x <> "**"

mdItalic :: Builder -> Builder
mdItalic x = "*" <> x <> "*"

mdTicked :: Builder -> Builder
mdTicked x = "`" +| x |+ "`"

mdEscapeAnchorS :: String -> Builder
mdEscapeAnchorS = \case
  [] -> ""
  ' ' : s -> "-" <> mdEscapeAnchorS s
  '(' : s -> "lparen" <> mdEscapeAnchorS s
  ')' : s -> "rparen" <> mdEscapeAnchorS s
  '[' : s -> "lbracket" <> mdEscapeAnchorS s
  ']' : s -> "rbracket" <> mdEscapeAnchorS s
  '{' : s -> "lbrace" <> mdEscapeAnchorS s
  '}' : s -> "rbrace" <> mdEscapeAnchorS s
  ',' : s -> "comma" <> mdEscapeAnchorS s
  ';' : s -> "semicolon" <> mdEscapeAnchorS s
  ':' : s -> "colon" <> mdEscapeAnchorS s
  '#' : s -> "hash" <> mdEscapeAnchorS s
  c : s -> build (toText [c]) <> mdEscapeAnchorS s

-- | Turn text into valid anchor. Human-readability is not preserved.
mdEscapeAnchor :: Text -> Builder
mdEscapeAnchor = mdEscapeAnchorS . toString

mdLocalRef :: Builder -> Text -> Builder
mdLocalRef txt anchor = "[" <> txt <> "](#" <> mdEscapeAnchor anchor <> ")"

mdAnchor :: Text -> Builder
mdAnchor name = "<a name=\"" <> mdEscapeAnchor name <> "\"></a>\n\n"

mdSeparator :: Builder
mdSeparator = "---\n\n"

-- | Text which is hidden until clicked.
mdSpoiler :: Builder -> Builder -> Builder
mdSpoiler name contents =
  mconcat $ intersperse "\n"
    [ "<details>"
    , "  <summary>" <> mdBold (build name) <> "</summary>"
    , contents
    , "</details>"
    , "<p>"
    ]
