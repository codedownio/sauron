{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | The tabbed pull request modal: Conversation / Commits / Checks / Review, like
-- the web UI's tabs.
module Sauron.UI.Modals.PullModal (
  renderPullRequestModal
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Data.String.Interpolate
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Vector as V
import GitHub
import Lens.Micro
import Relude
import Sauron.Event.PullModal (fileViewedState)
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Commit (commitInner, commitLine)
import Sauron.UI.Diff (renderFileStats, renderPatch)
import Sauron.UI.Issue (maxCommentWidth)
import Sauron.UI.Keys (openSelectedKey, showKey)
import Sauron.UI.Modals.Common (modalHeightPercent, modalWidth)
import Sauron.UI.Modals.NodeContent (renderNodeContent)
import Sauron.UI.Modals.ZoomModal (commentModeHotkeys, commentSection, hotkeyWidget)
import Sauron.UI.Pull (checksWidget)


-- | The whole pull request modal: title, tab bar, the selected tab's content, the
-- comment editor when it's focused, and the footer.
renderPullRequestModal :: AppState -> ModalState Fixed -> Widget ClickableName
renderPullRequestModal appState modalState@(PullRequestModalState {
  _pullModalNode=pullNode@(SinglePullNode (EntityData {_static=issue}))
  , _pullModalCommentMode=commentMode
  , _pullModalTab=currentTab
  }) =
  vBox ([
    hCenter $ withAttr boldText $ str title
    , renderTabBar currentTab
    , hBorder
    , padBottom Max $ withVScrollBars OnRight $ withVScrollBarHandles $ viewport ZoomModalContent Vertical $
        contentWidthLimit tabContent
    ]
    <> commentSection appState commentMode
    <> [
    hBorder
    , hCenter $ hBox $ intersperse (str "  ") footerHotkeys
  ])
  & border
  & withDefAttr normalAttr
  & hLimit (modalWidth appState)
  & vLimitPercent modalHeightPercent
  & centerLayer
  where
    Issue {issueNumber=(IssueNumber number), issueTitle} = issue
    title = [i|Pull Request \##{number}: #{issueTitle}|]

    -- Conversation reads like prose, so it's centered at a readable width; the tabs
    -- showing diffs and commit lists use the full modal width
    contentWidthLimit = case currentTab of
      TabConversation -> hCenter . hLimit maxCommentWidth
      _ -> padRight Max

    tabContent = renderTab appState modalState pullNode (renderNodeContent appState (SomeNode pullNode))

    footerHotkeys = case commentMode of
      Just cm -> commentModeHotkeys cm
      Nothing -> [hotkeyWidget k m | (k, m) <- footerHotkeyPairs currentTab]
renderPullRequestModal _ _ = str "Invalid modal state for PullModal"

-- | The tab bar shown under the modal title
renderTabBar :: PullModalTab -> Widget ClickableName
renderTabBar currentTab =
  hCenter $ hBox $ zipWith tabWidget [1 :: Int ..] [minBound .. maxBound]
  where
    tabWidget index tab
      | tab == currentTab = withAttr selectedTabAttr $ str [i| [#{index}] #{tabTitle tab} |]
      | otherwise = hBox [
          str " ["
          , withAttr hotkeyAttr $ str (show index)
          , str "] "
          , withAttr hotkeyMessageAttr $ str (tabTitle tab)
          , str " "
          ]

-- | The content of the selected tab. The conversation tab's content comes from the
-- node's own rendering, so it's passed in.
renderTab :: AppState -> ModalState Fixed -> Node Fixed 'SinglePullT -> Widget ClickableName -> Widget ClickableName
renderTab appState ui (SinglePullNode (EntityData {_state=nodeState, _healthCheckThread})) conversationContent =
  case _pullModalTab ui of
    TabConversation -> conversationContent
    TabChecks -> fromMaybe (str "No checks for this pull request.") $
      checksWidget (_appAnimationCounter appState) _healthCheckThread (pullNodeStateChecks nodeState)
    TabCommits -> commitsTab
    TabReview -> reviewTab
  where
    -- * Commits

    commitsTab = case pullNodeStateCommits nodeState of
      NotFetched -> str "Loading commits..."
      Fetching Nothing -> str "Loading commits..."
      Errored err -> withAttr erroredAttr $ strWrap [i|Failed to fetch commits: #{err}|]
      Fetching (Just commits) -> renderCommits commits
      Fetched commits -> renderCommits commits

    renderCommits commits
      | V.null commits = str "No commits in this pull request."
      | otherwise = padRight Max $ vBox $ V.toList $ V.imap renderCommitRow commits

    renderCommitRow index commit = vBox $ [
      -- Keep the selected commit scrolled into view as the cursor moves
      (if isSelected then visible . withAttr selectedPageAttr else id) $
        padRight Max $ commitLine (_appNow appState) isExpanded commit
      ] <> [padLeft (Pad 4) (padRight Max expandedContent) | isExpanded]
      where
        isSelected = index == _pullModalSelectedCommit ui
        sha = untagName (commitSha commit)
        isExpanded = Set.member sha (_pullModalExpandedCommits ui)
        expandedContent = case M.lookup sha (pullNodeStateCommitDetails nodeState) of
          Just (Fetched detailed) -> commitInner detailed
          Just (Errored err) -> withAttr erroredAttr $ strWrap [i|Failed to fetch commit: #{err}|]
          _ -> withAttr italicText $ str "Loading commit..."

    -- * Review

    reviewTab = case pullNodeStateFiles nodeState of
      NotFetched -> str "Loading changed files..."
      Fetching Nothing -> str "Loading changed files..."
      Errored err -> withAttr erroredAttr $ strWrap [i|Failed to fetch changed files: #{err}|]
      Fetching (Just files) -> renderReview files
      Fetched files -> renderReview files

    renderReview files
      | V.null files = str "No changed files in this pull request."
      | otherwise = vBox [
          hCenter (currentFileLine files)
          , hCenter (fileDots files)
          , hBorder
          , case files V.!? currentFile of
              Nothing -> str "No changed files"
              Just file -> case filePatch file of
                Just patch -> padRight Max $ renderPatch (fileFilename file) patch
                Nothing -> withAttr italicText $ str "No diff available for this file"
          ]

    currentFile = _pullModalCurrentFile ui
    viewedStates = pullNodeStateViewedStates nodeState

    -- One dot per file, on a single line. The current file's dot is bracketed and
    -- viewed files show a check. When the dots don't fit, show a window centered on
    -- the current file with ellipses at the truncated ends, like the numeric pagers.
    fileDots files = Widget Fixed Fixed $ do
      c <- getContext
      let avail = c ^. availWidthL
      let fileCount = V.length files
      -- All dots are 1 char, plus 2 for the current file's brackets and up to 4
      -- for the two ellipsis markers.
      let windowSize = max 1 (avail - 6)
      let renderAll = fileCount + 2 <= avail
      let start = if renderAll
            then 0
            else max 0 $ min (fileCount - windowSize) (currentFile - windowSize `div` 2)
      let end = if renderAll then fileCount else start + windowSize
      render $ hBox $ concat [
        [withAttr pageEllipsesAttr (str "… ") | start > 0]
        , concatMap (fileDot files) [start .. end - 1]
        , [withAttr pageEllipsesAttr (str " …") | end < fileCount]
        ]

    fileDot files ix' =
      if ix' == currentFile
        then [withAttr boldText $ str "[", dotWidget, withAttr boldText $ str "]"]
        else [dotWidget]
      where
        dotWidget = withAttr dotAttr $ str dot
        (dot, dotAttr) = case fileViewedState viewedStates <$> (files V.!? ix') of
          Just FileViewed -> ("✓", greenCheckAttr)
          Just FileDismissed -> ("~", queuedAttr)
          _ -> ("·", notFetchedAttr)

    currentFileLine files = case files V.!? currentFile of
      Nothing -> str " "
      Just file -> hBox [
        withAttr hotkeyMessageAttr $ str [i|File #{currentFile + 1}/#{V.length files}: |]
        , withAttr boldText $ str $ toString $ fileFilename file
        , str " "
        , renderFileStats (fileAdditions file) (fileDeletions file)
        , case fileViewedState viewedStates file of
            FileViewed -> hBox [str "  ", withAttr greenCheckAttr $ str "✓ Viewed"]
            FileDismissed -> hBox [str "  ", withAttr queuedAttr $ str "~ Changed since viewed"]
            FileUnviewed -> str ""
        ]

-- | Tab-specific hotkeys for the modal footer
footerHotkeyPairs :: PullModalTab -> [(String, String)]
footerHotkeyPairs currentTab = tabSpecific <> common
  where
    tabSpecific = case currentTab of
      -- Commenting and merging have their own buttons in the conversation
      TabConversation -> []
      TabCommits -> [("↑/↓", "Select"), ("Enter", "Expand")]
      TabChecks -> []
      TabReview -> [("←/→", "File"), ("v", "Toggle viewed"), ("V", "Viewed & next"), ("u", "Next unviewed")]

    common = [(showKey openSelectedKey, "Open PR"), ("q", "Close modal")]
