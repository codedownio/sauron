
module Sauron.UI.Markdown (
  markdownToWidgets
  , markdownToWidgetsWithWidth
  ) where

import Brick
import Commonmark hiding (str)
import Commonmark.Extensions
import Commonmark.Pandoc
import Data.String.Interpolate
import Relude
import Sauron.UI.AttrMap
import qualified Text.Pandoc.Builder as B
import qualified Data.Text as T


markdownToWidgets :: Text -> Widget n
markdownToWidgets = markdownToWidgetsWithWidth 100

markdownToWidgetsWithWidth :: Int -> Text -> Widget n
markdownToWidgetsWithWidth width t = 
  case parseCommonmarkWith (defaultSyntaxSpec <> gfmExtensions) (tokenize "source" t) :: Maybe (Either ParseError (Cm () B.Blocks)) of
    Nothing -> strWrap [i|Parse error.|]
    Just (Left err) -> strWrap [i|Parse error: '#{err}'.|]
    Just (Right (Cm (B.Many bs))) -> 
      vBox $ case fmap (renderBlockWithWidth width) (toList bs) of
        (x:xs) -> x : fmap (padTop (Pad 1)) xs
        x -> x

renderBlockWithWidth :: Int -> B.Block -> Widget n
renderBlockWithWidth width (B.Para inlines) = 
  -- Check if this paragraph starts with header-like text
  case inlines of
    (B.Str headerLevel : B.Space : rest) | T.all (== '#') headerLevel && not (T.null headerLevel) ->
      let headerAttr = getHeaderAttr (T.length headerLevel)
          inlinesToRender = if showHeaderSymbols 
                           then inlines 
                           else rest
      in withAttr headerAttr $ renderWrappedParagraphWithWidth width inlinesToRender
    _ -> renderWrappedParagraphWithWidth width inlines
renderBlockWithWidth width (B.Plain inlines) = renderWrappedParagraphWithWidth width inlines
renderBlockWithWidth width (B.Header level _ inlines) = 
  withAttr (getHeaderAttr level) $ renderWrappedParagraphWithWidth width inlines
renderBlockWithWidth _ (B.CodeBlock _ codeText) = 
  withAttr (attrName "code") $ strWrap (toString codeText)  -- Code blocks
renderBlockWithWidth width (B.OrderedList _ items) = 
  vBox $ zipWith (renderOrderedItem width) [1..] items
renderBlockWithWidth width (B.BulletList items) = 
  vBox $ map (renderBulletItem width) items
renderBlockWithWidth _ b = strWrap [i|UNHANDLED BLOCK: #{b}|]

renderOrderedItem :: Int -> Int -> [B.Block] -> Widget n
renderOrderedItem width n blocks = 
  let prefix = str [i|#{n}. |]
      content = vBox $ map (renderBlockWithWidth (width - 3)) blocks
  in hBox [prefix, content]

renderBulletItem :: Int -> [B.Block] -> Widget n  
renderBulletItem width blocks = 
  let prefix = str "• "
      content = vBox $ map (renderBlockWithWidth (width - 2)) blocks
  in hBox [prefix, content]

data StyledWord = StyledWord
  { wordText :: Text
  , wordAttrs :: [AttrName]  -- Stack of attributes (for nested formatting)
  } deriving (Show, Eq)

-- Configuration: set to True to show "##", False to hide them
showHeaderSymbols :: Bool
showHeaderSymbols = False

-- Get appropriate attribute for header level (more intense for bigger headings)
getHeaderAttr :: Int -> AttrName
getHeaderAttr level = case level of
  1 -> boldText        -- # = most intense
  2 -> boldText        -- ## = still bold
  3 -> italicText      -- ### = italic  
  4 -> underlineText   -- #### = underlined
  _ -> boldText        -- fallback

renderWrappedParagraphWithWidth :: Int -> [B.Inline] -> Widget n
renderWrappedParagraphWithWidth width inlines = 
  let lineGroups = splitInlinesOnLineBreaks inlines
  in vBox $ map (renderInlineGroup width) lineGroups

splitInlinesOnLineBreaks :: [B.Inline] -> [[B.Inline]]
splitInlinesOnLineBreaks = go []
  where
    go acc [] = if null acc then [] else [reverse acc]
    go acc (B.LineBreak : rest) = reverse acc : go [] rest
    go acc (B.SoftBreak : rest) = reverse acc : go [] rest  -- Treat SoftBreak as line break too
    go acc (x : rest) = go (x : acc) rest

renderInlineGroup :: Int -> [B.Inline] -> Widget n  
renderInlineGroup width inlines = 
  let styledWords = concatMap (inlineToStyledWords []) inlines
      wrappedLines = wrapStyledWords width styledWords
  in case wrappedLines of
       [] -> emptyWidget
       [line] -> renderStyledWordLine line
       multiLines -> vBox $ map renderStyledWordLine multiLines

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
inlineToStyledWords attrs (B.Code _ t) = [StyledWord t attrs]  -- Handle inline code (no special styling for now)
inlineToStyledWords _ inline = [StyledWord ("[UNHANDLED: " <> T.pack (show inline) <> "]") []]  -- Debug unknown inlines

-- Wrap styled words into lines
wrapStyledWords :: Int -> [StyledWord] -> [[StyledWord]]
wrapStyledWords _ [] = []
wrapStyledWords maxWidth styledWords = 
  let (line, rest) = takeWordsUpToWidth maxWidth styledWords
  in line : wrapStyledWords maxWidth rest

-- Take words up to the specified width
takeWordsUpToWidth :: Int -> [StyledWord] -> ([StyledWord], [StyledWord])
takeWordsUpToWidth _ [] = ([], [])
takeWordsUpToWidth maxWidth (w:ws)
  | T.length (wordText w) > maxWidth = ([w], ws)  -- Single word exceeds width
  | otherwise = 
      let (taken, remaining) = go (T.length (wordText w)) [w] ws
      in (reverse taken, remaining)
  where
    go _ acc [] = (acc, [])
    go currentLen acc (word:rest)
      | currentLen + 1 + T.length (wordText word) <= maxWidth = 
          go (currentLen + 1 + T.length (wordText word)) (word:acc) rest
      | otherwise = (acc, word:rest)

-- Render a line of styled words
renderStyledWordLine :: [StyledWord] -> Widget n
renderStyledWordLine [] = emptyWidget
renderStyledWordLine styledWords = hBox $ map renderStyledWord styledWords

-- Render a single styled word with its attribute stack
renderStyledWord :: StyledWord -> Widget n
renderStyledWord (StyledWord text []) = txt text
renderStyledWord (StyledWord text attrs) = 
  foldr (\attr widget -> withAttr attr widget) (txt text) attrs
