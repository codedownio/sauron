
module Sauron.UI.Markdown (
  markdownToWidgetsWithWidth
  ) where

import Brick
import Brick.Widgets.Skylighting (highlight)
import Commonmark hiding (str)
import Commonmark.Extensions
import Commonmark.Pandoc
import Data.String.Interpolate
import qualified Data.Text as T
import Relude
import Sauron.UI.AttrMap
import Sauron.UI.Markdown.Wrapping
import Sauron.UI.Markdown.Table
import qualified Skylighting as Sky
import qualified Skylighting.Core as SkyCore
import qualified Text.Pandoc.Builder as B


markdownToWidgetsWithWidth :: Int -> Text -> Widget n
markdownToWidgetsWithWidth width t =
  case parseCommonmarkWith (defaultSyntaxSpec <> gfmExtensions) (tokenize "source" t) :: Maybe (Either ParseError (Cm () B.Blocks)) of
    Nothing -> strWrap [i|Parse error.|]
    Just (Left err) -> strWrap [i|Parse error: '#{err}'.|]
    Just (Right (Cm (B.Many bs))) ->
      vBox $ case fmap (renderBlock Nothing width) (toList bs) of
        (x:xs) -> x : fmap (padTop (Pad 1)) xs
        x -> x

renderBlock :: forall n. Maybe (Widget n) -> Int -> B.Block -> Widget n
renderBlock maybePrefix width (B.Para inlines) =
  case inlines of
    (B.Str headerLevel : B.Space : rest) | T.all (== '#') headerLevel && not (T.null headerLevel) -> do
      let headerAttr = getHeaderAttr (T.length headerLevel)
      let inlinesToRender = if showHeaderSymbols then inlines else rest
      withAttr headerAttr $ renderWrappedParagraph maybePrefix width inlinesToRender
    _ -> renderWrappedParagraph maybePrefix width inlines
renderBlock maybePrefix width (B.Plain inlines) = renderWrappedParagraph maybePrefix width inlines
renderBlock maybePrefix width (B.Header level _ inlines) =
  withAttr (getHeaderAttr level) $ renderWrappedParagraph maybePrefix width inlines
renderBlock maybePrefix width (B.CodeBlock (_, classes, _) codeContent) =
  case maybePrefix of
    Nothing -> renderHighlightedCodeBlock width classes codeContent
    Just prefix ->
      let codeLines = T.lines codeContent
          renderLine line = hBox [prefix, withAttr codeBlockText $ txt line]
      in vBox $ map renderLine codeLines
renderBlock maybePrefix width (B.OrderedList _ items) =
  vBox $ zipWith (\n blocks -> go n blocks) [1..] items
  where
    go :: Int -> [B.Block] -> Widget n
    go n blocks =
      let itemPrefix = str [i|#{n}. |]
          content = vBox $ map (renderBlock maybePrefix (width - 3)) blocks
          fullPrefix = case maybePrefix of
                        Nothing -> itemPrefix
                        Just prefix -> hBox [prefix, itemPrefix]
      in hBox [fullPrefix, content]
renderBlock maybePrefix width (B.BulletList items) =
  vBox $ map go items
  where
    go :: [B.Block] -> Widget n
    go blocks =
      let bulletPrefix = str "• "
          content = vBox $ map (renderBlock maybePrefix (width - 2)) blocks
          fullPrefix = case maybePrefix of
                        Nothing -> bulletPrefix
                        Just prefix -> hBox [prefix, bulletPrefix]
      in hBox [fullPrefix, content]
renderBlock _ width (B.BlockQuote blocks) =
  -- Nested blockquotes get double prefix
  let quotePrefix = Just (withAttr italicText $ str "│ ")
  in vBox $ map (renderBlock quotePrefix (width - 2)) blocks
renderBlock maybePrefix width B.HorizontalRule =
  case maybePrefix of
    Nothing -> withAttr horizontalRuleAttr $ str (replicate width '─')
    Just prefix -> hBox [prefix, withAttr horizontalRuleAttr $ str (replicate (width - 2) '─')]
renderBlock maybePrefix width (B.Table _ _ _ thead tbody _) =
  case maybePrefix of
    Nothing -> renderTableWith renderBlock width thead tbody
    Just prefix -> hBox [prefix, renderTableWith renderBlock (width - 2) thead tbody]
renderBlock maybePrefix _ b =
  case maybePrefix of
    Nothing -> strWrap [i|UNHANDLED BLOCK: #{b}|]
    Just prefix -> hBox [prefix, strWrap [i|UNHANDLED BLOCK: #{b}|]]


showHeaderSymbols :: Bool
showHeaderSymbols = False

getHeaderAttr :: Int -> AttrName
getHeaderAttr level = case level of
  1 -> boldText        -- # = most intense
  2 -> boldText        -- ## = still bold
  3 -> italicText      -- ### = italic
  4 -> underlineText   -- #### = underlined
  _ -> boldText        -- fallback


-- Render code block with syntax highlighting
renderHighlightedCodeBlock :: Int -> [Text] -> Text -> Widget n
renderHighlightedCodeBlock _width classes codeContent =
  case getLanguage classes of
    Nothing -> withAttr codeBlockText $ strWrap (toString codeContent)  -- Use the ORIGINAL working approach
    Just lang ->
      case Sky.lookupSyntax lang Sky.defaultSyntaxMap of
        Nothing -> withAttr codeBlockText $ strWrap (toString codeContent)  -- Use the ORIGINAL working approach
        Just syntax ->
          case SkyCore.tokenize (SkyCore.TokenizerConfig Sky.defaultSyntaxMap False) syntax codeContent of
            Left _ -> withAttr codeBlockText $ strWrap (toString codeContent)  -- Use the ORIGINAL working approach
            Right _ ->
              -- Use the actual highlight function from brick-skylighting but constrain it properly
              hLimit 80 $ vBox [highlight syntax codeContent]
  where
    -- Extract language from classes (first class is typically the language)
    getLanguage :: [Text] -> Maybe Text
    getLanguage [] = Nothing
    getLanguage (x:_) = Just x
