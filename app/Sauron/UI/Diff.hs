module Sauron.UI.Diff (
  renderFileDiff
  , renderFileStats
  , renderPatch
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Skylighting (renderRawSource)
import qualified Data.Text as T
import GitHub
import Relude
import Sauron.UI.AttrMap
import qualified Skylighting as Sky
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
renderPatch :: Text -> Text -> Widget n
renderPatch filename patch = vBox $ map renderPatchLine (T.lines patch)
  where
    renderPatchLine :: Text -> Widget n
    renderPatchLine line
      | T.isPrefixOf "+" line && not (T.isPrefixOf "+++" line) =
          hBox [
            withAttr greenCheckAttr $ str "+┃",
            renderSyntaxHighlightedLine filename (T.drop 1 line)
          ]
      | T.isPrefixOf "-" line && not (T.isPrefixOf "---" line) =
          hBox [
            withAttr redXAttr $ str "-┃",
            renderSyntaxHighlightedLine filename (T.drop 1 line)
          ]
      | T.isPrefixOf "@@" line =
          hBox [
            str "  ",
            withAttr hashAttr $ str $ toString line
          ]
      | T.isPrefixOf " " line =
          hBox [
            str "  ",
            renderSyntaxHighlightedLine filename (T.drop 1 line)
          ]
      | otherwise =
          hBox [
            str "  ",
            withAttr normalAttr $ str $ toString line
          ]

renderSyntaxHighlightedLine :: Text -> Text -> Widget n
renderSyntaxHighlightedLine filename lineContent =
  case listToMaybe $ Sky.syntaxesByFilename Sky.defaultSyntaxMap (toString filename) of
    Nothing -> str $ toString lineContent
    Just syntax ->
      case SkyCore.tokenize (SkyCore.TokenizerConfig Sky.defaultSyntaxMap False) syntax lineContent of
        Left _ -> str $ toString lineContent
        Right tokens -> renderRawSource txt tokens
