{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sauron.UI.BranchWithInfo (
  formatCommitTimeText,
  formatPRInfoText,
  formatCheckStatusWithWidth,
  formatAheadBehindWithWidth
) where

import Brick
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import qualified Data.Vector as V
import GitHub
import Lens.Micro
import Relude
import Sauron.Actions.Util (findRepoParent, withGithubApiSemaphore, githubWithLogging)
import Sauron.Event.Helpers (withFixedElemAndParents)
import Sauron.Fetch.Core (makeEmptyElemWithState)
import Sauron.Fetch.Pull (fetchPullComments)
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Keys
import Sauron.UI.Statuses (fetchableQuarterCircleSpinner)
import Sauron.UI.Util.TimeDiff (timeFromNow)
import UnliftIO.Async (async)


instance ListDrawable Fixed 'SingleBranchWithInfoT where
  drawLine appState (EntityData {_static=(branchInfo, columnWidths), _state, ..}) =
    let branchCommit = BranchCommit (fromMaybe "" (branchWithInfoCommitOid branchInfo))
                                   (URL "")  -- placeholder URL
        branch = Branch (branchWithInfoBranchName branchInfo) branchCommit
    in branchLineWithInfo _toggled branch branchInfo columnWidths appState _state

  drawInner _ _ = Nothing

  getExtraTopBoxWidgets _app (EntityData {_static=(branchInfo, _)}) =
    let hasPR = isJust (branchWithInfoAssociatedPR branchInfo)
        keyAttr = if hasPR then hotkeyAttr else disabledHotkeyAttr
        msgAttr = if hasPR then hotkeyMessageAttr else disabledHotkeyMessageAttr
    in [hBox [str "["
             , withAttr keyAttr $ str $ showKey zoomModalKey
             , str "] "
             , withAttr msgAttr $ str "View PR"
             ]]

  handleHotkey s key (EntityData {_static=(branchInfo, _)})
    | key == zoomModalKey, Just pr <- branchWithInfoAssociatedPR branchInfo = do
        withFixedElemAndParents s $ \(SomeNode _) (SomeNode _variableEl) parents -> do
          case findRepoParent parents of
            Just (RepoNode (EntityData {_static=(owner, name)})) ->
              whenJust (prNumber pr) $ \num ->
                liftIO $ void $ async $ flip runReaderT (s ^. appBaseContext) $ do
                  withGithubApiSemaphore (githubWithLogging (issueR owner name (IssueNumber num))) >>= \case
                    Right issue -> do
                      ed <- atomically $ makeEmptyElemWithState (s ^. appBaseContext) issue NotFetched ("/pull/" <> show num) 0
                      fetchPullComments owner name (IssueNumber num) (_state ed)
                      liftIO $ atomically $ writeTVar (_appModalVariable s)
                        (Just (ZoomModalState (SomeNode (SinglePullNode ed)) (toList parents)))
                    Left _err -> return ()
            _ -> return ()
        return True
  handleHotkey _ _ _ = return False

branchLineWithInfo :: Bool -> Branch -> BranchWithInfo -> ColumnWidths -> AppState -> Fetchable (V.Vector Commit) -> Widget n
branchLineWithInfo toggled' (Branch {branchName, branchCommit}) branchData columnWidths appState fetchableState = vBox [line1, line2]
  where

    line1 = hBox [
      withAttr openMarkerAttr $ str (if toggled' then "[-] " else "[+] ")
      , withAttr branchAttr $ txt branchName
      , fetchableQuarterCircleSpinner (_appAnimationCounter appState) fetchableState
      , padLeft Max infoColumns
      ]

    infoColumns = hBox [
      fixedWidth (cwCommitTime columnWidths) $ txt $ formatCommitTimeText (_appNow appState) branchData
      , str spacing
      , fixedWidth (cwCheckStatus columnWidths) $ formatCheckStatus branchData
      , str spacing
      , fixedWidth (cwAheadBehind columnWidths) $ formatAheadBehind branchData
      , str spacing
      , fixedWidth (cwPRInfo columnWidths) $ txt $ formatPRInfoText branchData
      , str spacing
      ]
      where
        fixedWidth w widget = hLimit w $ padRight Max widget
        spacing = "   "

    line2 = padRight Max $ padLeft (Pad 4) $ hBox [
      str "Latest commit "
      , withAttr hashAttr $ str $ take 7 $ toString $ branchCommitSha branchCommit
      ]

    formatCheckStatus :: BranchWithInfo -> Widget n
    formatCheckStatus = fst . formatCheckStatusWithWidth

    formatAheadBehind :: BranchWithInfo -> Widget n
    formatAheadBehind = fst . formatAheadBehindWithWidth

-- * Widget + width helper functions

formatCheckStatusWithWidth :: BranchWithInfo -> (Widget n, Int)
formatCheckStatusWithWidth branchInfo =
  case branchWithInfoCheckStatus branchInfo of
    Nothing -> let text = "No checks" in (str text, length text)
    Just "SUCCESS" -> let widget = hBox [withAttr greenCheckAttr $ str "✓", str " Checks"] in (widget, 8) -- ✓ Checks = 8 chars
    Just "FAILURE" -> let widget = hBox [withAttr redXAttr $ str "✗", str " Failed"] in (widget, 8) -- ✗ Failed = 8 chars
    Just "PENDING" -> let widget = hBox [withAttr queuedAttr $ str "●", str " Running"] in (widget, 9) -- ● Running = 9 chars
    Just status -> let text = toString status in (str text, T.length status)

formatAheadBehindWithWidth :: BranchWithInfo -> (Widget n, Int)
formatAheadBehindWithWidth branchInfo = (widget, T.length totalText)
  where
    ahead = fromMaybe 0 (branchWithInfoAheadBy branchInfo)
    behind = fromMaybe 0 (branchWithInfoBehindBy branchInfo)
    aheadWidget = if ahead > 0
                 then hBox [withAttr greenCheckAttr $ str "↑", str $ " " <> show ahead]
                 else str "↑ 0"
    behindWidget = if behind > 0
                  then hBox [withAttr queuedAttr $ str "↓", str $ " " <> show behind]
                  else str "↓ 0"
    widget = hBox [aheadWidget, str " ", behindWidget]
    -- Calculate width: "↑ X ↓ Y" where X and Y are numbers
    aheadText = if ahead > 0 then "↑ " <> show ahead else "↑ 0"
    behindText = if behind > 0 then "↓ " <> show behind else "↓ 0"
    totalText = aheadText <> " " <> behindText

formatCommitTimeText :: UTCTime -> BranchWithInfo -> Text
formatCommitTimeText currentTime branchInfo =
  case branchWithInfoCommitDate branchInfo of
    Nothing -> "Unknown"
    Just dateStr ->
      case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (toString dateStr) :: Maybe UTCTime of
        Nothing -> T.take 10 dateStr  -- Fallback to raw date
        Just commitTime -> toText $ timeFromNow (diffUTCTime currentTime commitTime)


formatPRInfoText :: BranchWithInfo -> Text
formatPRInfoText branchInfo =
  case branchWithInfoAssociatedPR branchInfo of
    Nothing -> "No PR"
    Just pr -> case prNumber pr of
      Nothing -> "PR: Unknown"
      Just num ->
        let stateText = case prState pr of
              Just "OPEN" -> ""  -- Don't show state for open PRs
              Just "MERGED" -> " (merged)"
              Just "CLOSED" -> " (closed)"
              _ -> ""
        in "PR #" <> show num <> stateText
