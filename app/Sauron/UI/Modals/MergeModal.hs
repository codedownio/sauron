module Sauron.UI.Modals.MergeModal (
  renderMergeModal
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Edit (renderEditor)
import Data.String.Interpolate
import qualified Data.Text as T
import GitHub
import Relude
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Modals.CommentModal (modalWidth)


renderMergeModal :: AppState -> ModalState Fixed -> Widget ClickableName
renderMergeModal app (MergeModalState {_mergeIssue=(Issue {issueNumber=(IssueNumber number), issueTitle}), ..}) =
  vBox [
    hCenter $ withAttr boldText $ str headerText
    , hCenter $ withAttr italicText $ txt issueTitle

    , hBorder

    , padAll 1 $ vBox $ fmap methodRow (zip [1 :: Int ..] [minBound .. maxBound]) <> titleSection

    , hBorder

    , hCenter $ case _mergeSubmissionState of
        SubmittingMerge -> withAttr hotkeyMessageAttr $ str "Merging..."
        _ -> hBox $ intersperse (str "  ") $ [
          keyHint "↑/↓" "Select"
          ]
          <> [keyHint "Tab" "Commit title/message" | _mergeMethod == MergeMethodSquash]
          <> [
          keyHint (if _mergeFocus == MergeFocusBody then "Alt+Enter" else "Enter") "Merge"
          , keyHint "Esc/q" "Cancel"
          ]
  ]
  & border
  & withDefAttr normalAttr
  & hLimit (modalWidth app)
  & centerLayer
  where
    headerText = [i|Merge pull request #{untagName _mergeRepoOwner}/#{untagName _mergeRepoName}\##{number}|]

    methodRow (index, method) = hBox [
      withAttr selectedPageAttr $ str (if isSelected then " > " else "   ")
      , withAttr hotkeyAttr $ str (show index)
      , str ". "
      , vBox [
          withAttr (if isSelected then boldText else normalAttr) $ str (methodName method)
          , withAttr italicText $ strWrap (methodDescription method)
          ]
      ]
      where
        isSelected = method == _mergeMethod

    titleSection = case _mergeMethod of
      MergeMethodSquash -> [
        str " "
        , editorLabel MergeFocusTitle "Commit title"
        , border $ vLimit 1 $ withAttr normalAttr $
            renderEditor (str . toString . T.intercalate "\n") (_mergeFocus == MergeFocusTitle) _mergeCommitTitleEditor
        , str " "
        , editorLabel MergeFocusBody "Commit message"
        , border $ vLimit 6 $ withAttr normalAttr $
            renderEditor (txt . T.unlines) (_mergeFocus == MergeFocusBody) _mergeCommitMessageEditor
        ]
      _ -> []

    editorLabel focus label = hBox [
      withAttr (if _mergeFocus == focus then boldText else italicText) $ str label
      , withAttr italicText $ str "  (empty uses GitHub's default)"
      ]
renderMergeModal _ _ = str "Invalid modal state for MergeModal"

methodName :: MergeMethod -> String
methodName MergeMethodMerge = "Create a merge commit"
methodName MergeMethodSquash = "Squash and merge"
methodName MergeMethodRebase = "Rebase and merge"

methodDescription :: MergeMethod -> String
methodDescription MergeMethodMerge = "All commits from this branch will be added to the base branch via a merge commit."
methodDescription MergeMethodSquash = "The commits from this branch will be combined into one commit on the base branch."
methodDescription MergeMethodRebase = "The commits from this branch will be rebased and added to the base branch."

keyHint :: String -> String -> Widget ClickableName
keyHint keys desc = hBox [
  str "["
  , withAttr hotkeyAttr $ str keys
  , str "] "
  , withAttr hotkeyMessageAttr $ str desc
  ]
