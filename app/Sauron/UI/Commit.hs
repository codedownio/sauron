module Sauron.UI.Commit (
  commitLine,
  commitInner
  ) where

import Brick
import Brick.Widgets.Border
import qualified Data.Text as T
import qualified Data.Vector as V
import GitHub
import Relude
import Sauron.UI.AttrMap


commitLine :: Bool -> Commit -> Widget n
commitLine toggled (Commit {commitSha, commitGitCommit, commitAuthor}) = vBox [line1, line2]
  where
    line1 = hBox [
      withAttr openMarkerAttr $ str (if toggled then "[-] " else "[+] ")
      , withAttr normalAttr $ str $ toString $ gitCommitMessage commitGitCommit
      ]

    line2 = padRight Max $ padLeft (Pad 4) $ hBox [
      str "Commit "
      , withAttr hashAttr $ str $ take 7 $ toString $ untagName commitSha
      , case commitAuthor of
          Just author -> str (" by " <> toString (untagName (simpleUserLogin author)))
          Nothing -> case gitCommitAuthor commitGitCommit of
            gitAuthor -> str (" by " <> toString (gitUserName gitAuthor))
      ]


commitInner :: Commit -> Widget n
commitInner (Commit {commitFiles, commitStats}) = vBox [
  renderCommitStats commitStats
  , str ""
  , renderCommitFiles commitFiles
  ]

renderCommitStats :: Maybe Stats -> Widget n
renderCommitStats Nothing = str "No stats available"
renderCommitStats (Just (Stats {statsAdditions, statsDeletions, statsTotal})) = vBox [
  hBox [
    str "Showing ",
    withAttr normalAttr $ str $ show statsTotal,
    str " changed files with ",
    withAttr greenCheckAttr $ str $ show statsAdditions,
    str " additions and ",
    withAttr redXAttr $ str $ show statsDeletions,
    str " deletions."
    ],
  str ""
  ]

renderCommitFiles :: V.Vector File -> Widget n
renderCommitFiles files
  | V.null files = str "No files changed"
  | otherwise = vBox $ V.toList $ V.imap (\i file -> if i == 0 then renderFile file else vBox [str "", renderFile file]) files
  where
    renderFile :: File -> Widget n
    renderFile (File {fileFilename, fileAdditions, fileDeletions, fileStatus, filePatch}) =
      border $ padRight Max $ vBox [
        renderFileHeader fileFilename fileAdditions fileDeletions fileStatus,
        case filePatch of
          Nothing -> str ""
          Just patch -> renderFilePatch patch
      ]

    renderFileHeader :: Text -> Int -> Int -> Text -> Widget n
    renderFileHeader filename additions deletions _status = vBox [
      hBox [
        withAttr normalAttr $ str $ toString filename,
        str " ",
        renderFileStats additions deletions
        ],
      str ""
      ]

    renderFileStats :: Int -> Int -> Widget n
    renderFileStats additions deletions = hBox [
      withAttr greenCheckAttr $ str $ "+" <> show additions,
      str " ",
      withAttr redXAttr $ str $ "-" <> show deletions
      ]

    renderFilePatch :: Text -> Widget n
    renderFilePatch patch = vBox $ map renderPatchLine (T.lines patch)

    renderPatchLine :: Text -> Widget n
    renderPatchLine line
      | T.isPrefixOf "+" line && not (T.isPrefixOf "+++" line) =
          withAttr greenCheckAttr $ str $ toString line
      | T.isPrefixOf "-" line && not (T.isPrefixOf "---" line) =
          withAttr redXAttr $ str $ toString line
      | T.isPrefixOf "@@" line =
          withAttr hashAttr $ str $ toString line
      | otherwise =
          withAttr normalAttr $ str $ toString line
