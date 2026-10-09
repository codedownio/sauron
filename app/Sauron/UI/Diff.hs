{-# LANGUAGE TupleSections #-}

module Sauron.UI.Diff (
  renderFileDiff
  , renderFileStats
  , renderPatch
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Skylighting (attrNameForTokenType)
import qualified Data.Text as T
import GitHub
import qualified Graphics.Vty as V
import Relude
import Sauron.UI.AttrMap
import Sauron.UI.Diff.Words (DiffSegment(..), wordDiffSegments)
import Sauron.UI.Syntax
import qualified Skylighting.Core as SkyCore


-- | A changed file as a bordered box: filename with +/- counts, then the patch.
renderFileDiff :: File -> Widget n
renderFileDiff (File {fileFilename, fileAdditions, fileDeletions, filePatch}) =
  border $ padRight Max $ vBox [
    hBox [
      withAttr normalAttr $ str $ toString fileFilename,
      str " ",
      renderFileStats fileAdditions fileDeletions
      ],
    str "",
    case filePatch of
      Nothing -> str ""
      Just patch -> renderPatch fileFilename patch
  ]

renderFileStats :: Int -> Int -> Widget n
renderFileStats additions deletions = hBox [
  withAttr greenCheckAttr $ str $ "+" <> show additions,
  str " ",
  withAttr redXAttr $ str $ "-" <> show deletions
  ]

-- | A unified-diff patch, with the code normally syntax-highlighted (using the
-- filename to pick the syntax). Added/removed lines get a two-character gutter: the
-- +/- sign, then a green/red bar that reads as a vertical line along changed sections.
-- A changed line is tinted with a faint red/green across its whole width, and is
-- word-diffed against the line that replaced it so the words that actually changed get a
-- stronger shade of the same colour.
renderPatch :: Text -> Text -> Widget n
renderPatch filename patch = vBox $ concatMap renderGroup $ patchGroups $ T.lines patch
  where
    renderGroup (ChangeBlock removed added) =
      zipWith removedLine removed (segmentsFor fst)
      <> zipWith addedLine added (segmentsFor snd)
      where
        wordDiffs = zipWith wordDiffSegments removed added
        segmentsFor side = map (fmap side) wordDiffs <> repeat Nothing
    renderGroup (PatchLine line)
      | T.isPrefixOf "@@" line = [hBox [str "  ", withAttr hashAttr $ str $ toString line]]
      | T.isPrefixOf " " line = [hBox [str "  ", renderCodeLine filename Nothing (T.drop 1 line)]]
      | otherwise = [hBox [str "  ", withAttr normalAttr $ str $ toString line]]

    removedLine = changedLine diffRemovedGutterAttr "-┃" diffRemovedLineBgAttr diffRemovedBgAttr
    addedLine = changedLine diffAddedGutterAttr "+┃" diffAddedLineBgAttr diffAddedBgAttr

    -- The line's tint goes behind everything in the row, including the empty space to the
    -- right of the code, which is what the trailing fill is for.
    changedLine gutterAttr gutter lineBgAttr wordBgAttr content segments =
      withBackgroundOf lineBgAttr $ hBox [
        withAttr gutterAttr $ str gutter
        , renderCodeLine filename ((wordBgAttr,) <$> segments) content
        , withAttr lineBgAttr $ vLimit 1 $ fill ' '
        ]

-- | A run of removed lines together with the added lines that follow it: the pairs of
-- lines a word diff can be run on. The +/- signs are stripped from them.
data PatchGroup = ChangeBlock [Text] [Text] | PatchLine Text

patchGroups :: [Text] -> [PatchGroup]
patchGroups [] = []
patchGroups allLines@(line : rest)
  | isRemovedLine line || isAddedLine line =
      ChangeBlock (map (T.drop 1) removed) (map (T.drop 1) added) : patchGroups remaining
  | otherwise = PatchLine line : patchGroups rest
  where
    (removed, afterRemoved) = span isRemovedLine allLines
    (added, remaining) = span isAddedLine afterRemoved

isRemovedLine :: Text -> Bool
isRemovedLine line = T.isPrefixOf "-" line && not (T.isPrefixOf "---" line)

isAddedLine :: Text -> Bool
isAddedLine line = T.isPrefixOf "+" line && not (T.isPrefixOf "+++" line)

-- | One line of a patch's code, syntax-highlighted when the filename tells us the
-- syntax. Where a word diff found which parts of the line changed, those get the given
-- attribute's background, with the syntax colours still on top of it.
renderCodeLine :: Text -> Maybe (AttrName, [DiffSegment]) -> Text -> Widget n
renderCodeLine filename changed content =
  hBox $ map renderPiece $ zipSegments False syntaxPieces diffPieces
  where
    syntaxPieces = case syntaxForFilename filename >>= tokenizeLine of
      Nothing -> [(Nothing, content)]
      Just tokens -> [(Just tokenType, tokenText) | (tokenType, tokenText) <- tokens]

    tokenizeLine syntax = case SkyCore.tokenize (SkyCore.TokenizerConfig syntaxMap False) syntax content of
      Left _ -> Nothing
      Right sourceLines -> Just (concat sourceLines)

    diffPieces = [(diffSegmentChanged segment, diffSegmentText segment) | segment <- maybe [] snd changed]

    renderPiece (tokenType, isChanged, pieceText) =
      background $ maybe id (withAttr . attrNameForTokenType) tokenType $ txt pieceText
      where
        background = case changed of
          Just (changedAttr, _) | isChanged -> withBackgroundOf changedAttr
          _ -> id

-- | Walk two segmentations of the same text in step, splitting at every boundary in
-- either one, so that each piece that comes out carries a label from both. Pieces past
-- the end of the second segmentation get the fallback label.
zipSegments :: b -> [(a, Text)] -> [(b, Text)] -> [(a, b, Text)]
zipSegments fallback = go
  where
    go [] _ = []
    go as [] = [(a, fallback, t) | (a, t) <- as]
    go as@((a, x) : moreAs) bs@((b, y) : moreBs)
      | T.null x = go moreAs bs
      | T.null y = go as moreBs
      | otherwise = (a, b, T.take n x) : go ((a, T.drop n x) : moreAs) ((b, T.drop n y) : moreBs)
      where n = min (T.length x) (T.length y)

-- | Paint a widget with another attribute's background colour, leaving the foreground
-- (and so the syntax highlighting) alone.
withBackgroundOf :: AttrName -> Widget n -> Widget n
withBackgroundOf name w = Widget (hSize w) (vSize w) $ do
  backColor <- V.attrBackColor <$> lookupAttrName name
  render $ modifyDefAttr (\attr -> attr { V.attrBackColor = backColor }) w
