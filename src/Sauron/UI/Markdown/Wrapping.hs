module Sauron.UI.Markdown.Wrapping (
  StyledWord(..)
  , renderWrappedParagraph
  , wrapStyledWords
  , renderStyledWordLine
  , renderStyledWord
  , inlineToStyledWords
  ) where

import Brick
import qualified Data.Text as T
import Relude
import Sauron.UI.AttrMap
import qualified Text.Pandoc.Builder as B

data StyledWord = StyledWord {
  wordText :: Text
  -- | Stack of attributes (for nested formatting)
  , wordAttrs :: [AttrName]
  } deriving (Show, Eq)

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
inlineToStyledWords attrs (B.Span (_, classes, _) inlines)
  | "emoji" `elem` classes = concatMap (inlineToStyledWords attrs) inlines  -- Handle emojis by rendering their content
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
