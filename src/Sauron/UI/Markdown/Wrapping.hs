module Sauron.UI.Markdown.Wrapping (
  StyledWord(..)
  , renderWrappedParagraph
  , renderWrappedParagraphM
  , wrapStyledWords
  , renderStyledWordLine
  , renderStyledWord
  , inlineToStyledWords
  , FootnoteM
  ) where

import Brick
import Control.Monad.Writer
import qualified Data.Text as T
import Relude
import Sauron.UI.AttrMap
import Sauron.UI.Markdown.Footnotes
import qualified Text.Pandoc.Builder as B

data StyledWord = StyledWord {
  wordText :: Text
  -- | Stack of attributes (for nested formatting)
  , wordAttrs :: [AttrName]
  } deriving (Show, Eq)

-- Pure version for backward compatibility (discards footnotes)
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
  let (styledWords, _) = runWriter $ concat <$> traverse (inlineToStyledWords []) inlines
      wrappedLines = wrapStyledWords width styledWords
      renderLineWithPrefix line = case maybePrefix of
                                   Nothing -> padRight Max $ renderStyledWordLine line
                                   Just prefix -> hBox [prefix, padRight Max $ renderStyledWordLine line]
  in case wrappedLines of
       [] -> case maybePrefix of
              Nothing -> emptyWidget
              Just prefix -> hBox [prefix, emptyWidget]
       lines' -> vBox $ map renderLineWithPrefix lines'

-- Convert inline elements to styled words with attribute stacks (monadic version for footnote support)
inlineToStyledWords :: [AttrName] -> B.Inline -> FootnoteM [StyledWord]
inlineToStyledWords attrs inline = case inline of
  B.Note blocks -> do
    footnotes <- listen (pure ())
    let footnoteNum = length (snd footnotes) + 1
    tell [FootnoteRef footnoteNum blocks]
    pure [StyledWord ("[" <> T.pack (show footnoteNum) <> "]") (underlineText : attrs)]
  B.Str t -> pure $ map (\word -> StyledWord word attrs) (T.words t)
  B.Emph inlines -> concat <$> traverse (inlineToStyledWords (italicText : attrs)) inlines
  B.Strong inlines -> concat <$> traverse (inlineToStyledWords (boldText : attrs)) inlines
  B.Underline inlines -> concat <$> traverse (inlineToStyledWords (underlineText : attrs)) inlines
  B.Strikeout inlines -> concat <$> traverse (inlineToStyledWords (strikeoutText : attrs)) inlines
  B.Link _ inlines _ -> concat <$> traverse (inlineToStyledWords (underlineText : attrs)) inlines
  B.Space -> pure [StyledWord " " attrs]
  B.SoftBreak -> pure []
  B.LineBreak -> pure []
  B.Code _ t -> pure [StyledWord t (codeText : attrs)]
  B.Span (_, classes, _) inlines
    | "emoji" `elem` classes -> concat <$> traverse (inlineToStyledWords attrs) inlines
  _ -> pure [StyledWord ("[UNHANDLED: " <> T.pack (show inline) <> "]") []]

-- Monadic versions for footnote support
type FootnoteM = WriterT [FootnoteRef] Identity

renderWrappedParagraphM :: Maybe (Widget n) -> Int -> [B.Inline] -> FootnoteM (Widget n)
renderWrappedParagraphM maybePrefix width inlines = do
  let lineGroups = splitInlinesOnLineBreaks inlines
  renderedGroups <- traverse (renderInlineGroupM maybePrefix width) lineGroups
  pure $ vBox renderedGroups
  where
    splitInlinesOnLineBreaks :: [B.Inline] -> [[B.Inline]]
    splitInlinesOnLineBreaks = go []
      where
        go acc [] = if null acc then [] else [reverse acc]
        go acc (B.LineBreak : rest) = reverse acc : go [] rest
        go acc (B.SoftBreak : rest) = reverse acc : go [] rest
        go acc (x : rest) = go (x : acc) rest

renderInlineGroupM :: Maybe (Widget n) -> Int -> [B.Inline] -> FootnoteM (Widget n)
renderInlineGroupM maybePrefix width inlines = do
  styledWords <- concat <$> traverse (inlineToStyledWords []) inlines
  let wrappedLines = wrapStyledWords width styledWords
      renderLineWithPrefix line = case maybePrefix of
                                   Nothing -> padRight Max $ renderStyledWordLine line
                                   Just prefix -> hBox [prefix, padRight Max $ renderStyledWordLine line]
  pure $ case wrappedLines of
    [] -> case maybePrefix of
           Nothing -> emptyWidget
           Just prefix -> hBox [prefix, emptyWidget]
    lines' -> vBox $ map renderLineWithPrefix lines'

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
