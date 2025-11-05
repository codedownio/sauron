{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sauron.UI.Commit (
  commitLine,
  commitInner
  ) where

import Brick
import Brick.Widgets.Border
import Control.Monad
import qualified Data.Text as T
import Data.Time
import qualified Data.Vector as V
import GitHub
import Relude
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Util
import Sauron.UI.Util.TimeDiff


instance ListDrawable Fixed 'SingleCommitT where
  drawLine appState (EntityData {_static=commit, _state, ..}) =
    commitLine (_appNow appState) _toggled commit

  drawInner _appState (EntityData {_state, _ident, ..}) = do
    guard _toggled
    guardFetchedOrHasPrevious _state $ \detailedCommit ->
      return $ commitInner detailedCommit


commitLine :: UTCTime -> Bool -> Commit -> Widget n
commitLine now toggled' (Commit {commitSha, commitGitCommit, commitAuthor}) =
  if toggled' then vBox [line1, line2] else vBox [line1]
  where
    commitMessage = toString $ gitCommitMessage commitGitCommit
    displayMessage = if toggled' then commitMessage else takeWhile (/= '\n') commitMessage

    (authorName, commitTime) = case commitAuthor of
      Just author ->
        let user = gitCommitAuthor commitGitCommit
        in (toString (untagName (simpleUserLogin author)), gitUserDate user)
      Nothing ->
        let user = gitCommitAuthor commitGitCommit
        in (toString (gitUserName user), gitUserDate user)

    timeAgo = timeFromNow (diffUTCTime now commitTime)

    line1 = hBox [
      withAttr openMarkerAttr $ str (if toggled' then "[-] " else "[+] ")
      , withAttr normalAttr $ str displayMessage
      , padLeft Max $ hBox [
          str timeAgo
          , str " • "
          , withAttr usernameAttr $ str authorName
          , str " • "
          , withAttr hashAttr $ str $ take 7 $ toString $ untagName commitSha
        ]
      ]

    line2 = padRight Max $ padLeft (Pad 4) $
      withAttr normalAttr $ str $ if toggled' && '\n' `elem` commitMessage
        then drop 1 $ dropWhile (/= '\n') commitMessage
        else ""


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
