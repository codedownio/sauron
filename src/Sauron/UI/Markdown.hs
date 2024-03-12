
module Sauron.UI.Markdown (
  markdownToWidgets
  ) where

import Brick
import Commonmark hiding (str)
import Commonmark.Extensions
import Commonmark.Pandoc
import Data.String.Interpolate
import Relude
import qualified Text.Pandoc.Builder as B


markdownToWidgets :: Text -> Widget n
markdownToWidgets t = case parseCommonmarkWith gfmExtensions (tokenize "source" t) :: Maybe (Either ParseError (Cm () B.Blocks)) of
  Nothing -> strWrap [i|Parse error.|]
  Just (Left err) -> strWrap [i|Parse error: '#{err}'.|]
  Just (Right (Cm (B.Many bs))) -> vBox $ case fmap renderBlock (toList bs) of
    (x:xs) -> x : fmap (padTop (Pad 1)) xs
    x -> x

renderBlock :: B.Block -> Widget n
renderBlock (B.Para inlines) = hBox (fmap renderInline inlines)
renderBlock b = strWrap [i|UNHANDLED: #{b}|]

renderInline :: B.Inline -> Widget n
renderInline (B.Str t) = str (toString t)
renderInline B.Space = str " "
renderInline B.SoftBreak = str "\n"
renderInline x = str [i|UNHANDLED: #{x}|]
