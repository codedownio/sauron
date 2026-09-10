{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sauron.UI.Commit (
  commitLine,
  commitInner
  ) where

import Brick
import Control.Lens
import Control.Monad
import Data.Time
import qualified Data.Vector as V
import GitHub
import Relude
import Sauron.Actions
import Sauron.Event.Helpers
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Diff (renderFileDiff)
import Sauron.UI.Keys
import Sauron.UI.Util
import Sauron.UI.Util.TimeDiff


instance ListDrawable Fixed 'SingleCommitT where
  drawLine appState (EntityData {_static=commit, _state, ..}) =
    commitLine (_appNow appState) _toggled commit

  drawInner _appState (EntityData {_state, _ident, ..}) = do
    guard _toggled
    guardFetchedOrHasPrevious _state $ \detailedCommit ->
      return $ commitInner detailedCommit

  getExtraTopBoxWidgets _app (EntityData {}) =
    [hBox [str "["
          , withAttr hotkeyAttr $ str $ showKey zoomModalKey
          , str "] "
          , withAttr hotkeyMessageAttr $ str "Zoom"
          ]
    ]

  handleHotkey s key (EntityData {})
    | key == zoomModalKey = do
        withFixedElemAndParents s $ \(SomeNode _) (SomeNode variableEl) parents -> do
          refreshOnZoom (s ^. appBaseContext) variableEl parents
          liftIO $ atomically $ writeTVar (_appModalVariable s) (Just (newZoomModalState (SomeNode variableEl) (toList parents)))
        return True
  handleHotkey _ _ _ = return False

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
  | otherwise = vBox $ V.toList $ V.imap (\idx file -> if idx == 0 then renderFileDiff file else vBox [str "", renderFileDiff file]) files
