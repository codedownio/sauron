
module Sauron.UI.Markdown (
  markdownToWidgetsWithWidth
  ) where

import Brick
import Brick.Widgets.Skylighting (renderRawSource)
import Commonmark hiding (str)
import Commonmark.Extensions
import Commonmark.Pandoc
import Control.Monad.Writer
import Data.List (partition)
import Data.String.Interpolate
import qualified Data.Text as T
import Relude
import Sauron.Types (DetailsExpanded(..))
import Sauron.UI.AttrMap
import Sauron.UI.Markdown.Html (preprocessHtml)
import Sauron.UI.Markdown.Table
import Sauron.UI.Markdown.Wrapping (FootnoteM, renderWrappedParagraphM)
import qualified Skylighting as Sky
import qualified Skylighting.Core as SkyCore
import qualified Text.Pandoc.Builder as B


allExtensions :: SyntaxSpec Maybe (Cm () B.Inlines) (Cm () B.Blocks)
allExtensions = defaultSyntaxSpec <> gfmExtensions <> emojiSpec

markdownToWidgetsWithWidth :: DetailsExpanded -> Int -> Text -> Widget n
markdownToWidgetsWithWidth detailsExpanded width (T.replace "\r\n" "\n" -> t) = -- TODO: do this T.replace on the initial fetch for perf
  case parseCommonmarkWith allExtensions (tokenize "source" t) :: Maybe (Either ParseError (Cm () B.Blocks)) of
    Nothing -> strWrap [i|Parse error.|]
    Just (Left err) -> strWrap [i|Parse error: '#{err}'.|]
    Just (Right (Cm (B.Many bs))) -> vBox $ contentWidgets ++ [footnoteWidget]
      where
        preprocessedBlocks = preprocessHtml (toList bs)
        (renderedBlocks, footnoteBlocks) = runWriter $ traverse (renderBlockM detailsExpanded Nothing width) preprocessedBlocks
        contentWidgets = case renderedBlocks of
                           (x:xs) -> x : fmap (padTop (Pad 1)) xs
                           x -> x
        footnoteWidget = if null footnoteBlocks
                         then emptyWidget
                         else vBox [
                           padTop (Pad 1) $ withAttr horizontalRuleAttr $ str (replicate width '─'),
                           padTop (Pad 1) $ vBox $ zipWith (renderFootnoteRef detailsExpanded width) [1..] footnoteBlocks
                         ]

renderBlockM :: DetailsExpanded -> Maybe (Widget n) -> Int -> B.Block -> FootnoteM (Widget n)
renderBlockM detailsExpanded maybePrefix width block = case block of
  B.Para inlines -> renderWrappedParagraphM maybePrefix width inlines
  B.Plain inlines -> renderWrappedParagraphM maybePrefix width inlines
  B.Header level _ inlines -> withAttr (getHeaderAttr level) <$> renderWrappedParagraphM maybePrefix width inlines
  B.Div (_, classes, _) blocks
    | "details" `elem` classes -> renderDetailsBlockM detailsExpanded maybePrefix width blocks
  other -> pure $ renderBlock detailsExpanded maybePrefix width other

renderFootnoteRef :: DetailsExpanded -> Int -> Int -> [B.Block] -> Widget n
renderFootnoteRef detailsExpanded width num blocks =
  let numberPrefix = withAttr hashNumberAttr $ str ("[" <> show num <> "] ")
      renderedContent = vBox $ map (renderBlock detailsExpanded Nothing (width - 4)) blocks
  in hBox [numberPrefix, renderedContent]

renderBlock :: forall n. DetailsExpanded -> Maybe (Widget n) -> Int -> B.Block -> Widget n
renderBlock _detailsExpanded maybePrefix width (B.Para inlines) =
  case inlines of
    (B.Str headerLevel : B.Space : rest) | T.all (== '#') headerLevel && not (T.null headerLevel) ->
      let headerAttr = getHeaderAttr (T.length headerLevel)
          inlinesToRender = if showHeaderSymbols then inlines else rest
      in withAttr headerAttr $ fst $ runWriter $ renderWrappedParagraphM maybePrefix width inlinesToRender
    _ -> fst $ runWriter $ renderWrappedParagraphM maybePrefix width inlines
  where
    showHeaderSymbols = False
renderBlock _detailsExpanded maybePrefix width (B.Plain inlines) = fst $ runWriter $ renderWrappedParagraphM maybePrefix width inlines
renderBlock _detailsExpanded maybePrefix width (B.Header level _ inlines) =
  withAttr (getHeaderAttr level) $ fst $ runWriter $ renderWrappedParagraphM maybePrefix width inlines
renderBlock _detailsExpanded _maybePrefix width (B.CodeBlock (_, classes, _) codeContent) =
  renderHighlightedCodeBlock width classes codeContent
renderBlock detailsExpanded maybePrefix width (B.OrderedList _ items) =
  vBox $ zipWith (\n blocks -> go n blocks) [1..] items
  where
    go :: Int -> [B.Block] -> Widget n
    go n blocks =
      let itemPrefix = str [i|#{n}. |]
          content = vBox $ map (renderBlock detailsExpanded maybePrefix (width - 3)) blocks
          fullPrefix = case maybePrefix of
                        Nothing -> itemPrefix
                        Just prefix -> hBox [prefix, itemPrefix]
      in hBox [fullPrefix, content]
renderBlock detailsExpanded maybePrefix width (B.BulletList items) =
  vBox $ map go items
  where
    go :: [B.Block] -> Widget n
    go blocks =
      let bulletPrefix = str "• "
          content = vBox $ map (renderBlock detailsExpanded maybePrefix (width - 2)) blocks
          fullPrefix = case maybePrefix of
                        Nothing -> bulletPrefix
                        Just prefix -> hBox [prefix, bulletPrefix]
      in hBox [fullPrefix, content]
renderBlock detailsExpanded _ width (B.BlockQuote blocks) =
  -- Nested blockquotes get double prefix
  let quotePrefix = Just (withAttr italicText $ str "│ ")
  in vBox $ map (renderBlock detailsExpanded quotePrefix (width - 2)) blocks
renderBlock _detailsExpanded maybePrefix width B.HorizontalRule =
  case maybePrefix of
    Nothing -> withAttr horizontalRuleAttr $ str (replicate width '─')
    Just prefix -> hBox [prefix, withAttr horizontalRuleAttr $ str (replicate (width - 2) '─')]
renderBlock detailsExpanded maybePrefix width (B.Table _ _ _ thead tbody _) =
  case maybePrefix of
    Nothing -> renderTableWith (renderBlock detailsExpanded) width thead tbody
    Just prefix -> hBox [prefix, renderTableWith (renderBlock detailsExpanded) (width - 2) thead tbody]
renderBlock _detailsExpanded maybePrefix _width (B.RawBlock _ content) =
  let widget = if T.null (T.strip content)
               then emptyWidget
               else txtWrap content
  in case maybePrefix of
       Nothing -> widget
       Just prefix -> hBox [prefix, widget]
renderBlock detailsExpanded maybePrefix width (B.Div (_, classes, _) blocks)
  | "details" `elem` classes =
    fst $ runWriter $ renderDetailsBlockM detailsExpanded maybePrefix width blocks
renderBlock detailsExpanded maybePrefix width (B.Div _ blocks) =
  vBox $ map (renderBlock detailsExpanded maybePrefix width) blocks
renderBlock _detailsExpanded maybePrefix width (B.LineBlock lineGroups) =
  vBox $ map (\inlines -> fst $ runWriter $ renderWrappedParagraphM maybePrefix width (toList inlines)) lineGroups
renderBlock _detailsExpanded maybePrefix _ b =
  case maybePrefix of
    Nothing -> strWrap [i|UNHANDLED BLOCK: #{b}|]
    Just prefix -> hBox [prefix, strWrap [i|UNHANDLED BLOCK: #{b}|]]

-- * Details/summary rendering

renderDetailsBlockM :: DetailsExpanded -> Maybe (Widget n) -> Int -> [B.Block] -> FootnoteM (Widget n)
renderDetailsBlockM detailsExpanded maybePrefix width blocks = do
  let (summaryBlocks, contentBlocks) = partition isSummaryDiv blocks
  let summaryRawText = case summaryBlocks of
        (B.Div _ [B.Plain [B.Str t]] : _) -> t
        (B.Div _ [B.Para [B.Str t]] : _) -> t
        _ -> "Details"
  let summaryInlines = parseInlineMarkdown summaryRawText
  let expanded = detailsExpanded == DetailsExpanded
  let indicator = if expanded then "▼ " else "▶ "
  summaryWidget <- renderWrappedParagraphM Nothing (width - 2) summaryInlines
  let headerWidget = hBox [withAttr boldText $ str indicator, summaryWidget]
  let prefixedHeader = case maybePrefix of
        Nothing -> headerWidget
        Just prefix -> hBox [prefix, headerWidget]
  if expanded
    then do
      contentWidgets <- traverse (renderBlockM detailsExpanded maybePrefix width) contentBlocks
      pure $ vBox (prefixedHeader : map (padLeft (Pad 2)) contentWidgets)
    else pure prefixedHeader

isSummaryDiv :: B.Block -> Bool
isSummaryDiv (B.Div (_, classes, _) _) = "summary" `elem` classes
isSummaryDiv _ = False

-- | Parse text as inline markdown (with emoji and HTML support).
-- Used for details summary text extracted from raw HTML.
parseInlineMarkdown :: Text -> [B.Inline]
parseInlineMarkdown t =
  case parseCommonmarkWith allExtensions (tokenize "source" t) :: Maybe (Either ParseError (Cm () B.Blocks)) of
    Just (Right (Cm (B.Many bs))) -> case toList (preprocessHtml (toList bs)) of
      (B.Para inlines : _) -> inlines
      (B.Plain inlines : _) -> inlines
      _ -> [B.Str t]
    _ -> [B.Str t]

-- * Helpers

getHeaderAttr :: Int -> AttrName
getHeaderAttr level = case level of
  1 -> boldUnderlineText
  2 -> underlineText
  3 -> boldText
  4 -> italicText
  _ -> boldText

renderHighlightedCodeBlock :: Int -> [Text] -> Text -> Widget n
renderHighlightedCodeBlock _width classes codeContent = fromMaybe (withAttr codeBlockText $ txtWrap codeContent) $ do
  lang <- case classes of
    [] -> Nothing
    (x:_) -> Just x
  syntax <- Sky.lookupSyntax lang Sky.defaultSyntaxMap
  case SkyCore.tokenize (SkyCore.TokenizerConfig Sky.defaultSyntaxMap False) syntax codeContent of
    Left _ -> Nothing
    Right xs -> Just $ renderRawSource txt xs
