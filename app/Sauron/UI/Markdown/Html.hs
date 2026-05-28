module Sauron.UI.Markdown.Html (
  preprocessHtml
  ) where

import qualified Data.Char as C
import qualified Data.Text as T
import Relude
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Format(..))


-- | Preprocess HTML in the Pandoc AST.
-- Handles inline HTML tags (bold, italic, etc.) and block-level HTML (details/summary).
preprocessHtml :: [B.Block] -> [B.Block]
preprocessHtml = preprocessBlocks

-- * Block-level preprocessing

preprocessBlocks :: [B.Block] -> [B.Block]
preprocessBlocks [] = []
preprocessBlocks (B.RawBlock _ content : rest)
  -- Skip HTML comments
  | isHtmlComment content = preprocessBlocks rest
  -- <details>/<summary> blocks
  | hasOpenTag "details" content
  , hasCloseTag "details" content =
    -- Self-contained: <details>...</details> all in one raw block
    B.Div ("", ["details"], []) (summaryBlock : parseRawHtml (extractDetailsBody content))
      : preprocessBlocks rest
  | hasOpenTag "details" content =
    -- Multi-block: collect until </details>
    B.Div ("", ["details"], []) (summaryBlock : preprocessBlocks detailsContent)
      : preprocessBlocks afterClose
  -- Skip stray </details> closing tags
  | hasCloseTag "details" content = preprocessBlocks rest
  -- <hr> tags
  | isHrTag content = B.HorizontalRule : preprocessBlocks rest
  -- Other raw blocks: parse HTML into blocks
  | otherwise = parseRawHtml content ++ preprocessBlocks rest
  where
    summaryBlock = B.Div ("", ["summary"], []) [B.Plain (extractSummaryInlines content)]
    (detailsContent, afterClose) = collectBlocksUntilClose "details" rest
preprocessBlocks (block : rest) = transformBlock block : preprocessBlocks rest

-- | Recursively transform a block, preprocessing HTML inlines within it.
transformBlock :: B.Block -> B.Block
transformBlock (B.Para inlines) = B.Para (preprocessInlines inlines)
transformBlock (B.Plain inlines) = B.Plain (preprocessInlines inlines)
transformBlock (B.Header n attr inlines) = B.Header n attr (preprocessInlines inlines)
transformBlock (B.BulletList items) = B.BulletList (map preprocessBlocks items)
transformBlock (B.OrderedList attrs items) = B.OrderedList attrs (map preprocessBlocks items)
transformBlock (B.BlockQuote blocks) = B.BlockQuote (preprocessBlocks blocks)
transformBlock (B.Div attr blocks) = B.Div attr (preprocessBlocks blocks)
transformBlock (B.LineBlock lls) = B.LineBlock (map preprocessInlines lls)
transformBlock b = b

-- * Inline-level preprocessing

preprocessInlines :: [B.Inline] -> [B.Inline]
preprocessInlines [] = []
preprocessInlines (B.RawInline _ tag : rest)
  -- Skip HTML comments
  | isHtmlComment tag = preprocessInlines rest
  -- Formatting tags: collect content until closing tag
  | Just (False, tagName) <- parseTag tag
  , isFormattingTag tagName
  , Just (content, remaining) <- collectInlinesUntilClose tagName rest
  = wrapInline tagName (preprocessInlines content) ++ preprocessInlines remaining
  -- <br> / <br/> tags
  | Just (False, "br") <- parseTag tag = B.LineBreak : preprocessInlines rest
  -- Strip all other HTML tags (stray closing tags, unknown tags, etc.)
  | otherwise = preprocessInlines rest
preprocessInlines (inline : rest) = recurseInline inline : preprocessInlines rest

-- | Recursively preprocess HTML inlines within container inlines.
recurseInline :: B.Inline -> B.Inline
recurseInline (B.Emph inlines) = B.Emph (preprocessInlines inlines)
recurseInline (B.Strong inlines) = B.Strong (preprocessInlines inlines)
recurseInline (B.Underline inlines) = B.Underline (preprocessInlines inlines)
recurseInline (B.Strikeout inlines) = B.Strikeout (preprocessInlines inlines)
recurseInline (B.Link attr inlines target) = B.Link attr (preprocessInlines inlines) target
recurseInline (B.Image attr inlines target) = B.Image attr (preprocessInlines inlines) target
recurseInline (B.Span attr inlines) = B.Span attr (preprocessInlines inlines)
recurseInline x = x

-- * HTML tag parsing

-- | Parse an HTML tag like "<b>", "</em>", "<br/>", returning (isClosing, tagName).
parseTag :: Text -> Maybe (Bool, Text)
parseTag t = do
  inner <- T.stripPrefix "<" (T.strip t) >>= T.stripSuffix ">"
  let trimmed = T.strip inner
  if T.isPrefixOf "/" trimmed
    then extractTagName (T.strip (T.drop 1 trimmed)) >>= \n -> Just (True, n)
    else extractTagName (fromMaybe trimmed (T.stripSuffix "/" trimmed)) >>= \n -> Just (False, n)
  where
    extractTagName s =
      case T.toLower (T.takeWhile isTagChar s) of
        name | T.null name -> Nothing
             | otherwise -> Just name
    isTagChar c = C.isAlphaNum c || c == '-'

isFormattingTag :: Text -> Bool
isFormattingTag t = t `elem` ["b", "strong", "i", "em", "u", "s", "del", "strike", "code", "sub", "sup"]

-- | Wrap inline content in the appropriate Pandoc formatting node.
wrapInline :: Text -> [B.Inline] -> [B.Inline]
wrapInline "b"      content = [B.Strong content]
wrapInline "strong" content = [B.Strong content]
wrapInline "i"      content = [B.Emph content]
wrapInline "em"     content = [B.Emph content]
wrapInline "u"      content = [B.Underline content]
wrapInline "s"      content = [B.Strikeout content]
wrapInline "del"    content = [B.Strikeout content]
wrapInline "strike" content = [B.Strikeout content]
wrapInline "code"   content = [B.Code ("", [], []) (inlinesToText content)]
wrapInline _        content = content -- sub, sup, unknown: just keep content

-- | Collect inlines until a matching closing tag, handling nesting of the same tag.
collectInlinesUntilClose :: Text -> [B.Inline] -> Maybe ([B.Inline], [B.Inline])
collectInlinesUntilClose tagName = go (0 :: Int) []
  where
    go _ _ [] = Nothing
    go depth acc (inline@(B.RawInline _ t) : rest)
      | Just (True, n) <- parseTag t, n == tagName =
        if depth == 0
        then Just (reverse acc, rest)
        else go (depth - 1) (inline : acc) rest
      | Just (False, n) <- parseTag t, n == tagName =
        go (depth + 1) (inline : acc) rest
    go depth acc (x : rest) = go depth (x : acc) rest

-- | Extract plain text from inline elements (used for <code> content).
inlinesToText :: [B.Inline] -> Text
inlinesToText = T.concat . map go
  where
    go (B.Str t) = t
    go B.Space = " "
    go B.SoftBreak = " "
    go B.LineBreak = "\n"
    go (B.Code _ t) = t
    go (B.Emph inlines) = inlinesToText inlines
    go (B.Strong inlines) = inlinesToText inlines
    go (B.Underline inlines) = inlinesToText inlines
    go (B.Strikeout inlines) = inlinesToText inlines
    go (B.Span _ inlines) = inlinesToText inlines
    go (B.Link _ inlines _) = inlinesToText inlines
    go _ = ""

-- * Block-level HTML helpers

isHtmlComment :: Text -> Bool
isHtmlComment t = "<!--" `T.isPrefixOf` T.strip t

hasOpenTag :: Text -> Text -> Bool
hasOpenTag tagName content = ("<" <> tagName) `T.isInfixOf` T.toLower content

hasCloseTag :: Text -> Text -> Bool
hasCloseTag tagName content = ("</" <> tagName <> ">") `T.isInfixOf` T.toLower content

isHrTag :: Text -> Bool
isHrTag content = T.toLower (T.strip content) `elem` ["<hr>", "<hr/>", "<hr />"]

-- | Extract summary inlines from a raw block containing <details> and <summary>.
-- Returns the raw text as a single Str for later re-parsing through commonmark
-- (which handles emoji shortcodes, etc.).
extractSummaryInlines :: Text -> [B.Inline]
extractSummaryInlines content =
  maybe [B.Str "Details"] (\t -> [B.Str t]) (extractSummaryRawText content)

extractSummaryRawText :: Text -> Maybe Text
extractSummaryRawText content = do
  let (_, afterOpen) = T.breakOn "<summary" content
  guard (not $ T.null afterOpen)
  let afterTag = T.drop 1 $ T.dropWhile (/= '>') afterOpen
  let (summaryText, closeTag) = T.breakOn "</summary>" afterTag
  guard (not $ T.null closeTag)
  let trimmed = T.strip summaryText
  guard (not $ T.null trimmed)
  pure trimmed

-- | Convert raw HTML text into Pandoc inlines, tokenizing tags and text.
htmlTextToInlines :: Text -> [B.Inline]
htmlTextToInlines t
  | T.null t = []
  | "<!--" `T.isPrefixOf` t =
    T.breakOn "-->" t & snd & T.drop 3 & htmlTextToInlines
  | T.isPrefixOf "<" t =
    case T.break (== '>') t of
      (_, rest) | T.null rest -> [B.Str t]
      (beforeClose, rest) ->
        B.RawInline (Format "html") (beforeClose <> ">") : htmlTextToInlines (T.drop 1 rest)
  | otherwise =
    case T.break (== '<') t of
      (text, rest) -> textToInlines (not $ T.null rest) text ++ htmlTextToInlines rest

-- | Convert a text fragment (no HTML tags) into Str and Space inlines,
-- preserving leading/trailing whitespace as Space nodes.
textToInlines :: Bool -> Text -> [B.Inline]
textToInlines hasMore t = case T.words t of
  [] | hasLeading -> [B.Space]
     | otherwise -> []
  ws -> (if hasLeading then (B.Space :) else id)
        ((if hasTrailing then (++ [B.Space]) else id)
         (intersperse B.Space (map B.Str ws)))
  where
    hasLeading = not (T.null t) && C.isSpace (T.head t)
    hasTrailing = hasMore && not (T.null t) && C.isSpace (T.last t)

-- | Extract the body content from a self-contained details block
-- (text between </summary> and </details>).
extractDetailsBody :: Text -> Text
extractDetailsBody content =
  T.breakOn "</summary>" content
    & snd
    & (\rest -> if T.null rest then T.drop 1 $ T.dropWhile (/= '>') content else T.drop (T.length "</summary>") rest)
    & T.breakOn "</details>"
    & fst

-- | Parse raw HTML text into Pandoc blocks.
-- Handles <ul>, <ol>, and plain text/inline HTML.
parseRawHtml :: Text -> [B.Block]
parseRawHtml = go . T.strip
  where
    go t
      | T.null t = []
      | hasOpenTag "ul" t = parseHtmlList B.BulletList "ul" t
      | hasOpenTag "ol" t = parseHtmlList (B.OrderedList (1, B.DefaultStyle, B.DefaultDelim)) "ol" t
      | otherwise =
        case preprocessInlines (htmlTextToInlines t) of
          [] -> []
          inlines -> [B.Para inlines]

    parseHtmlList wrap tagName t =
      case T.breakOn ("<" <> tagName) (T.toLower t) of
        (_, match) | T.null match -> go t
        (before, _) ->
          case T.breakOn ("</" <> tagName <> ">") afterOpen of
            (content, rest) ->
              case extractLiItems content of
                [] -> go (T.strip remaining)
                items -> wrap items : go (T.strip remaining)
              where remaining = T.drop (T.length ("</" <> tagName <> ">")) rest
          where afterOpen = T.drop 1 $ T.dropWhile (/= '>') $ T.drop (T.length before) t

-- | Extract <li> items from HTML list content.
extractLiItems :: Text -> [[B.Block]]
extractLiItems = go
  where
    go t = case T.breakOn "<li" (T.toLower t) of
      (_, match) | T.null match -> []
      (before, _) ->
        case T.breakOn "</li>" afterTag of
          (itemContent, closeAndRest) ->
            [B.Plain (preprocessInlines (htmlTextToInlines itemContent))]
              : go (if T.null closeAndRest then "" else T.drop (T.length "</li>") closeAndRest)
        where afterTag = T.drop 1 $ T.dropWhile (/= '>') $ T.drop (T.length before) t

-- | Collect blocks until a closing tag is found, handling nesting.
collectBlocksUntilClose :: Text -> [B.Block] -> ([B.Block], [B.Block])
collectBlocksUntilClose tagName = go (0 :: Int) []
  where
    go _ acc [] = (reverse acc, [])
    go depth acc (block@(B.RawBlock _ content) : rest)
      | hasCloseTag tagName content =
        if depth == 0
        then (reverse acc, rest)
        else go (depth - 1) (block : acc) rest
      | hasOpenTag tagName content = go (depth + 1) (block : acc) rest
    go depth acc (b : rest) = go depth (b : acc) rest
