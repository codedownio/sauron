
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
renderBlock maybePrefix _ b =
  case maybePrefix of
    Nothing -> strWrap [i|UNHANDLED BLOCK: #{b}|]
    Just prefix -> hBox [prefix, strWrap [i|UNHANDLED BLOCK: #{b}|]]


data StyledWord = StyledWord {
  wordText :: Text
  -- | Stack of attributes (for nested formatting)
  , wordAttrs :: [AttrName]
  } deriving (Show, Eq)

showHeaderSymbols :: Bool
showHeaderSymbols = False

getHeaderAttr :: Int -> AttrName
getHeaderAttr level = case level of
  1 -> boldText        -- # = most intense
  2 -> boldText        -- ## = still bold
  3 -> italicText      -- ### = italic
  4 -> underlineText   -- #### = underlined
  _ -> boldText        -- fallback

renderWrappedParagraph :: Maybe (Widget n) -> Int -> [B.Inline] -> Widget n
renderWrappedParagraph maybePrefix width inlines =
  let lineGroups = splitInlinesOnLineBreaks inlines
  in vBox $ map (renderInlineGroup maybePrefix width) lineGroups
  where
    splitInlinesOnLineBreaks :: [B.Inline] -> [[B.Inline]]
    splitInlinesOnLineBreaks = go []
      where
        go acc [] = if null acc then [] else [reverse acc]
        go acc (B.LineBreak : rest) = reverse acc : go [] rest
        go acc (B.SoftBreak : rest) = reverse acc : go [] rest  -- Treat SoftBreak as line break too
        go acc (x : rest) = go (x : acc) rest

renderInlineGroup :: Maybe (Widget n) -> Int -> [B.Inline] -> Widget n
renderInlineGroup maybePrefix width inlines =
  let styledWords = concatMap (inlineToStyledWords []) inlines
      wrappedLines = wrapStyledWords width styledWords
      renderLineWithPrefix line = case maybePrefix of
                                   Nothing -> padRight Max $ renderStyledWordLine line
                                   Just prefix -> hBox [prefix, padRight Max $ renderStyledWordLine line]
  in case wrappedLines of
       [] -> case maybePrefix of
              Nothing -> emptyWidget
              Just prefix -> hBox [prefix, emptyWidget]
       lines' -> vBox $ map renderLineWithPrefix lines'

-- Convert inline elements to styled words with attribute stacks
inlineToStyledWords :: [AttrName] -> B.Inline -> [StyledWord]
inlineToStyledWords attrs (B.Str t) =
  map (\word -> StyledWord word attrs) (T.words t)
inlineToStyledWords attrs (B.Emph inlines) =
  concatMap (inlineToStyledWords (italicText : attrs)) inlines
inlineToStyledWords attrs (B.Strong inlines) =
  concatMap (inlineToStyledWords (boldText : attrs)) inlines
inlineToStyledWords attrs (B.Underline inlines) =
  concatMap (inlineToStyledWords (underlineText : attrs)) inlines
inlineToStyledWords attrs (B.Strikeout inlines) =
  concatMap (inlineToStyledWords (strikeoutText : attrs)) inlines
inlineToStyledWords attrs (B.Link _ inlines _) =
  concatMap (inlineToStyledWords (underlineText : attrs)) inlines
inlineToStyledWords attrs B.Space = [StyledWord " " attrs]
inlineToStyledWords _ B.SoftBreak = []  -- SoftBreak is handled by word wrapping
inlineToStyledWords _ B.LineBreak = []  -- LineBreak is handled by splitInlinesOnLineBreaks
inlineToStyledWords attrs (B.Code _ t) = [StyledWord t (codeText : attrs)]  -- Handle inline code
inlineToStyledWords _ inline = [StyledWord ("[UNHANDLED: " <> T.pack (show inline) <> "]") []]  -- Debug unknown inlines

wrapStyledWords :: Int -> [StyledWord] -> [[StyledWord]]
wrapStyledWords _ [] = []
wrapStyledWords maxWidth styledWords =
  let (line, rest) = takeWordsUpToWidth maxWidth styledWords
  in line : wrapStyledWords maxWidth rest

takeWordsUpToWidth :: Int -> [StyledWord] -> ([StyledWord], [StyledWord])
takeWordsUpToWidth _ [] = ([], [])
takeWordsUpToWidth maxWidth (w:ws)
  | T.length (wordText w) > maxWidth = ([w], ws)  -- Single word exceeds width
  | otherwise =
      let (taken, remaining) = go (T.length (wordText w)) [w] ws
          -- Check if we should adjust to avoid lone punctuation
          (finalTaken, finalRemaining) = avoidLonePunct taken remaining
      in (reverse finalTaken, finalRemaining)
  where
    go _ acc [] = (acc, [])
    go currentLen acc (word:rest)
      | currentLen + T.length (wordText word) <= maxWidth =
          go (currentLen + T.length (wordText word)) (word:acc) rest
      | otherwise = (acc, word:rest)

    -- Avoid leaving lone punctuation at end of line
    avoidLonePunct taken remaining =
      case taken of
        [] -> (taken, remaining)
        (lastWord:restTaken)
          | isLonePunct (wordText lastWord) && not (null restTaken) && not (null remaining) ->
              -- Move the lone punctuation to the next line
              (restTaken, lastWord:remaining)
          | otherwise -> (taken, remaining)

    isLonePunct :: Text -> Bool
    isLonePunct t = T.length t == 1 && t `elem` ["(", ")", "[", "]", "{", "}", ",", ".", ";", ":", "!", "?"]

-- Render a line of styled words
renderStyledWordLine :: [StyledWord] -> Widget n
renderStyledWordLine [] = emptyWidget
renderStyledWordLine styledWords = hBox $ map renderStyledWord styledWords

-- Render a single styled word with its attribute stack
renderStyledWord :: StyledWord -> Widget n
renderStyledWord (StyledWord text []) = txt text
renderStyledWord (StyledWord text attrs) =
  foldr (\attr widget -> withAttr attr widget) (txt text) attrs

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
