module Sauron.UI.Modals.PRReviewModal (
  renderPRReviewModal
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Data.String.Interpolate
import qualified Data.Vector as V
import GitHub
import Lens.Micro
import Relude
import Sauron.Event.PRReviewModal (fileViewedState)
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Diff (renderFileStats, renderPatch)
import Sauron.UI.Modals.CommentModal (modalHeightPercent, modalWidth)


renderPRReviewModal :: AppState -> ModalState Fixed -> Widget ClickableName
renderPRReviewModal app (PRReviewModalState {_reviewIssue=(Issue {issueNumber=(IssueNumber number), issueTitle}), ..}) =
  vBox [
    hCenter $ withAttr boldText $ str [i|Review \##{number}: #{issueTitle}|]
    , hCenter fileDots

    , hBorderWithLabel (padLeftRight 1 currentFileLine)

    , padBottom Max $ withVScrollBars OnRight $ withVScrollBarHandles $
        viewport PRReviewModalContent Vertical $
        case maybeCurrentFile of
          Nothing -> hCenter $ str "No changed files"
          Just file -> case filePatch file of
            Just patch -> padRight Max $ renderPatch (fileFilename file) patch
            Nothing -> hCenter $ withAttr italicText $ str "No diff available for this file"

    , hBorder

    , hCenter $ hBox $ intersperse (str "  ") [
        keyHint "←/→" "File"
        , keyHint "v" "Toggle viewed"
        , keyHint "V" "Viewed & next"
        , keyHint "u" "Next unviewed"
        , keyHint "q" "Close"
        ]
  ]
  & border
  & withDefAttr normalAttr
  & hLimit (modalWidth app)
  & vLimitPercent modalHeightPercent
  & centerLayer
  where
    fileCount = V.length _reviewFiles
    maybeCurrentFile = _reviewFiles V.!? _reviewCurrentFile

    -- One dot per file, on a single line. The current file's dot is bracketed and
    -- viewed files show a check. When the dots don't fit, show a window centered on
    -- the current file with ellipses at the truncated ends, like the numeric pagers.
    fileDots = Widget Fixed Fixed $ do
      c <- getContext
      let avail = c ^. availWidthL
      -- All dots are 1 char, plus 2 for the current file's brackets and up to 4
      -- for the two ellipsis markers.
      let windowSize = max 1 (avail - 6)
      let renderAll = fileCount + 2 <= avail
      let start = if renderAll
            then 0
            else max 0 $ min (fileCount - windowSize) (_reviewCurrentFile - windowSize `div` 2)
      let end = if renderAll then fileCount else start + windowSize
      render $ hBox $ concat [
        [withAttr pageEllipsesAttr (str "… ") | start > 0]
        , concatMap fileDot [start .. end - 1]
        , [withAttr pageEllipsesAttr (str " …") | end < fileCount]
        ]

    fileDot ix' =
      if ix' == _reviewCurrentFile
        then [withAttr boldText $ str "[", dotWidget, withAttr boldText $ str "]"]
        else [dotWidget]
      where
        dotWidget = withAttr dotAttr $ str dot
        (dot, dotAttr) = case fileViewedState _reviewViewedStates <$> (_reviewFiles V.!? ix') of
          Just FileViewed -> ("✓", greenCheckAttr)
          Just FileDismissed -> ("~", queuedAttr)
          _ -> ("·", notFetchedAttr)

    currentFileLine = case maybeCurrentFile of
      Nothing -> str " "
      Just file -> hBox [
        withAttr hotkeyMessageAttr $ str [i|File #{_reviewCurrentFile + 1}/#{fileCount}: |]
        , withAttr boldText $ str $ toString $ fileFilename file
        , str " "
        , renderFileStats (fileAdditions file) (fileDeletions file)
        , case fileViewedState _reviewViewedStates file of
            FileViewed -> hBox [str "  ", withAttr greenCheckAttr $ str "✓ Viewed"]
            FileDismissed -> hBox [str "  ", withAttr queuedAttr $ str "~ Changed since viewed"]
            FileUnviewed -> str ""
        ]
renderPRReviewModal _ _ = str "Invalid modal state for PRReviewModal"

keyHint :: String -> String -> Widget ClickableName
keyHint keys desc = hBox [
  str "["
  , withAttr hotkeyAttr $ str keys
  , str "] "
  , withAttr hotkeyMessageAttr $ str desc
  ]
