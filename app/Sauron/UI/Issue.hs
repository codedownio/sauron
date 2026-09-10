{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sauron.UI.Issue (
  issueLine
  , issueInner

  -- Common timeline rendering functions
  , renderTimelineItem
  , renderTimelineItemWithAttr
  , renderItemWithBorder
  , renderComment
  , commentTopLabel
  , topLabel

  -- Re-exports from Sauron.UI.Issue.Events
  , consolidateEvents
  , maxCommentWidth
  , TimelineItem(..)

  -- Close/reopen
  , closeReopenAndRefresh

  -- Bottom action buttons and comment box
  , nodeButtonsWidget
  , actionButtonWidget
  , activeCommentMode

  -- Details toggle widget
  , detailsToggleWidget
  ) where

import Brick
import Brick.Forms
import Brick.Widgets.Border (border)
import Control.Lens
import Control.Monad
import Data.String.Interpolate
import qualified Data.Text as T
import Data.Time
import qualified Data.Vector as V
import GitHub
import GitHub.Data.Name
import Relude
import Sauron.Actions
import Sauron.Actions.Util (findRepoParent, findIssuesParent)
import Sauron.Event.CommentModal (openZoomAndComment)
import Sauron.Event.Helpers
import Sauron.Event.Search (ensureNonEmptySearch)
import Sauron.Fetch.Issue (fetchIssueComments)
import Sauron.Mutations.Issue (closeIssue, reopenIssue)
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Issue.Events
import Sauron.UI.Issue.Events.ReviewRequests (renderReviewRequestGroup)
import Sauron.UI.Keys
import Sauron.UI.Markdown
import Sauron.UI.Modals.Common (renderBodyEditor)
import Sauron.UI.Statuses (fetchableQuarterCircleSpinner)
import Sauron.UI.TimelineBorder
import Sauron.UI.Util
import Sauron.UI.Util.TimeDiff
import UnliftIO.Async (async)
import WEditorBrick.WrappingEditor (dumpEditor)


instance ListDrawable Fixed 'SingleIssueT where
  drawLine appState (EntityData {_static=issue, ..}) =
    issueLine (_appNow appState) _toggled issue (_appAnimationCounter appState) _state

  drawInner appState (EntityData {_static=issue, _state, _ident, ..}) = do
    guard _toggled
    guardFetchedOrHasPrevious _state $ \comments ->
      return $ vBox [
        issueInner (_appDetailsExpanded appState) (_appNow appState) issue comments
        , nodeButtonsWidget appState _ident False issue
        ]

  getExtraTopBoxWidgets app (EntityData {_static=issue}) =
    [hBox [str "["
          , withAttr hotkeyAttr $ str $ showKey editSearchKey
          , str "] "
          , withAttr hotkeyMessageAttr $ str "Search"
          ]
    , hBox [str "["
          , withAttr hotkeyAttr $ str $ showKey zoomModalKey
          , str "] "
          , withAttr hotkeyMessageAttr $ str "Zoom"
          ]
    , hBox [str "["
          , withAttr hotkeyAttr $ str $ showKey closeReopenKey
          , str "/"
          , withAttr hotkeyAttr $ str $ showKey commentKey
          , str "] "
          , withAttr hotkeyMessageAttr $ str (if issueState issue == StateOpen then "Close" else "Reopen")
          , str "/"
          , withAttr hotkeyMessageAttr $ str "Comment"
          ]
    , detailsToggleWidget app
    ]

  handleHotkey s key (EntityData {_static=issue})
    | key == editSearchKey = do
        withFixedElemAndParents s $ \_ _ parents -> do
          case findIssuesParent parents of
            Just issuesNode@(PaginatedIssuesNode ed) -> do
              searchText <- liftIO $ atomically $ ensureNonEmptySearch issuesNode
              modify (appForm ?~ (newForm [editTextField id TextForm (Just 1)] searchText, _ident ed))
            _ -> return ()
        return True
    | key == zoomModalKey = do
        withFixedElemAndParents s $ \(SomeNode _) (SomeNode variableEl) parents -> do
          refreshOnZoom (s ^. appBaseContext) variableEl parents
          liftIO $ atomically $ writeTVar (_appModalVariable s) (Just (newZoomModalState (SomeNode variableEl) (toList parents)))
        return True
    | key == commentKey = do
        withFixedElemAndParents s $ \_ _ parents ->
          whenJust (findRepoParent parents) $ \(RepoNode (EntityData {_static=(owner, name)})) ->
            openZoomAndComment s issue False owner name
        return True
    | key == closeReopenKey = do
        liftIO $ void $ async $ do
          withFixedElemAndParents s $ \_ _ parents ->
            whenJust (findRepoParent parents) $ \(RepoNode (EntityData {_static=(owner, name)})) ->
              whenJust (findIssuesParent parents) $ \(PaginatedIssuesNode ed) ->
                closeReopenAndRefresh (s ^. appBaseContext) owner name issue (_children ed)
                  (\(SingleIssueNode e) -> (_static e, _state e))
                  (\(SingleIssueNode e) iss -> SingleIssueNode (e { _static = iss }))
                  fetchIssueComments
        return True
  handleHotkey _ _ _ = return False

issueLine :: UTCTime -> Bool -> Issue -> Int -> Fetchable (V.Vector TimelineEvent) -> Widget n
issueLine now toggled' (Issue {issueNumber=(IssueNumber number), ..}) animationCounter fetchableState = vBox [line1, line2]
  where
    (icon, markerAttr) = subjectStateIcon (if issueState == StateOpen then IssueOpen else IssueClosed)
    line1 = hBox [
      withAttr openMarkerAttr $ str (if toggled' then "[-] " else "[+] ")
      , withAttr markerAttr $ str (icon <> "  ")
      , withAttr normalAttr $ str $ toString issueTitle
      , fetchableQuarterCircleSpinner animationCounter fetchableState
      , padLeft Max $ str (if issueComments > 0 then [i|🗨  #{issueComments}|] else "")
      ]

    line2 = padRight Max $ padLeft (Pad 4) $ hBox [
      withAttr hashAttr $ str "#"
      , withAttr hashNumberAttr $ str $ show number
      , str [i| opened #{timeFromNow (diffUTCTime now issueCreatedAt)} by |]
      , withAttr usernameAttr $ str [i|#{untagName $ simpleUserLogin issueUser}|]
      ]

issueInner :: DetailsExpanded -> UTCTime -> Issue -> V.Vector TimelineEvent -> Widget n
-- issueInner detailsExpanded now issue body cs = vBox [strWrap (show issue), strWrap (show body), strWrap (show cs)]
issueInner detailsExpanded now (Issue {issueUser=(SimpleUser {simpleUserLogin=(N openerUsername)}), ..}) cs =
  allItems
  & zip [0..]
  & fmap (uncurry (renderTimelineItem detailsExpanded now (length allItems)))
  & vBox
  where
    issueDescriptionBody = fromMaybe "*No description provided.*" issueBody

    allItems = (Left (openerUsername, issueDescriptionBody, issueCreatedAt), "")
             : fmap (\item -> (Right item, "")) (consolidateEvents (toList cs))

-- * Util, exported for Pull.hs

renderTimelineItem :: DetailsExpanded -> UTCTime -> Int -> Int -> (Either (Text, Text, UTCTime) TimelineItem, Text) -> Widget n
renderTimelineItem detailsExpanded = renderTimelineItemWithAttr detailsExpanded Nothing

renderTimelineItemWithAttr :: DetailsExpanded -> Maybe AttrName -> UTCTime -> Int -> Int -> (Either (Text, Text, UTCTime) TimelineItem, Text) -> Widget n
renderTimelineItemWithAttr detailsExpanded maybeAttr now totalItems idx (itemType, _extraBody) =
  let pick def attrVariant = maybe def attrVariant maybeAttr
      borderFunc = if totalItems == 1
                   then pick standaloneTimelineBorder standaloneTimelineBorderAttr
                   else if idx == 0
                   then pick firstTimelineBorder firstTimelineBorderAttr
                   else if idx == totalItems - 1
                   then pick lastTimelineBorder lastTimelineBorderAttr
                   else pick middleTimelineBorder middleTimelineBorderAttr
  in case itemType of
    Left (username, descriptionBody, createdAt) -> -- Issue/PR description
      adaptiveWidth $ \w -> borderFunc
        (topLabel username createdAt now)
        (markdownToWidgetsWithWidth detailsExpanded (w - 2) descriptionBody)
    Right item -> renderItemWithBorder detailsExpanded now (idx == totalItems - 1) borderFunc item

topLabel :: Text -> UTCTime -> UTCTime -> Widget n
topLabel username createdAt now =
  (withAttr usernameAttr (str [i|#{username} |]) <+> str [i|opened #{timeFromNow (diffUTCTime now createdAt)}|])
    & padLeftRight 1

renderItemWithBorder :: DetailsExpanded -> UTCTime -> Bool -> (Widget n -> Widget n -> Widget n) -> TimelineItem -> Widget n
renderItemWithBorder detailsExpanded now isLast borderFunc item =
  case item of
    SingleItem (TimelineComment comment) -> renderComment detailsExpanded now borderFunc comment
    SingleItem (TimelineIssueEvent event) -> renderEvent now isLast event
    SingleItem (TimelineCommit commit) -> renderCommitEvent now isLast commit
    SingleItem (TimelineReview rev) -> renderReviewEvent now isLast rev
    LabelGroup rep added removed -> renderLabelGroup now isLast rep added removed
    ReviewRequestGroup rep events -> renderReviewRequestGroup now isLast rep events

renderComment :: DetailsExpanded -> UTCTime -> (Widget n -> Widget n -> Widget n) -> IssueComment -> Widget n
renderComment detailsExpanded now borderFunc (IssueComment {issueCommentUser=(SimpleUser {simpleUserLogin=(N username)}), issueCommentCreatedAt, ..}) =
  adaptiveWidth $ \w -> borderFunc
    (commentTopLabel username commentTime now)
    (markdownToWidgetsWithWidth detailsExpanded (w - 2) issueCommentBody)
  where commentTime = issueCommentCreatedAt

commentTopLabel :: Text -> UTCTime -> UTCTime -> Widget n
commentTopLabel username commentTime now =
  (withAttr usernameAttr (str [i|#{username} |]) <+> str [i|commented #{timeFromNow (diffUTCTime now commentTime)}|])
    & padLeftRight 1

-- * Close/reopen, exported for Pull.hs

closeReopenAndRefresh ::
  (MonadIO m)
  => BaseContext -> Name Owner -> Name Repo -> Issue
  -> TVar [child]
  -> (child -> (Issue, st))
  -> (child -> Issue -> child)
  -> (Name Owner -> Name Repo -> IssueNumber -> st -> ReaderT BaseContext IO ())
  -> m ()
closeReopenAndRefresh bc owner name issue childrenVar getInfo setIssue fetchComments = do
  let action = if issueState issue == StateOpen then closeIssue else reopenIssue
      targetNum = issueNumber issue
      isTarget c = issueNumber (fst (getInfo c)) == targetNum
  liftIO (action bc owner name targetNum) >>= \case
    Right updatedIssue -> do
      mStateVar <- liftIO $ atomically $ do
        nodes <- readTVar childrenVar
        let mState = listToMaybe [snd (getInfo c) | c <- nodes, isTarget c]
        writeTVar childrenVar [if isTarget c then setIssue c updatedIssue else c | c <- nodes]
        return mState
      whenJust mStateVar $ \stateVar ->
        liftIO $ runReaderT (fetchComments owner name targetNum stateVar) bc
    Left _err -> return ()

-- * Bottom action buttons

-- | The comment mode targeting this particular issue/PR, if any
activeCommentMode :: AppState -> Issue -> Maybe CommentMode
activeCommentMode app issue = case _appModal app >>= modalCommentMode of
  Just commentMode | issueId (_commentModeIssue commentMode) == issueId issue -> Just commentMode
  _ -> Nothing

-- | The comment box above the action buttons, like the web UI. It's a placeholder
-- until comment mode focuses it, at which point it becomes a live editor.
--
-- The live editor renders only in the modal, since the editor's widget name is baked
-- into its state and brick requires names to be unique across everything on screen
-- (the modal draws over the main list, which draws the same node). The modal render
-- is the one with a negated ident, per 'renderNodeContent'.
commentBoxWidget :: AppState -> Int -> Issue -> Widget ClickableName
commentBoxWidget app nodeIdent issue = case activeCommentMode app issue of
  Just (CommentMode {_commentModeEditor}) | nodeIdent < 0 ->
    -- Keep the editor in view in the modal's viewport while typing
    visible $ adaptiveWidth $ \w ->
      renderBodyEditor app True w (max 5 (min (length (dumpEditor _commentModeEditor)) 20)) _commentModeEditor
  _ -> adaptiveWidth $ \_ ->
    border $ padRight Max $ withAttr italicText $ str [i|Leave a comment [#{showKey commentKey}]|]

-- | Web-UI-style buttons at the bottom of an open issue/PR (display only — the
-- hotkey shown on each button performs the action). Once something is typed in the
-- comment box the buttons switch to their submit forms, like the web UI.
nodeButtonsWidget :: AppState -> Int -> Bool -> Issue -> Widget ClickableName
nodeButtonsWidget app nodeIdent isPR issue = vBox [
  commentBoxWidget app nodeIdent issue
  , hBox [closeButton, str "  ", commentButton]
  ]
  where
    commentText = case activeCommentMode app issue of
      Just (CommentMode {_commentModeEditor}) ->
        T.strip $ T.intercalate "\n" $ map toText $ dumpEditor _commentModeEditor
      Nothing -> ""
    hasComment = not (T.null commentText)

    closeButton = actionButtonWidget buttonGrayAttr $ closeIcon <> [str closeText]
      where
        closeText | hasComment = [i|#{closeLabel} with comment [Alt+Shift+Enter]|]
                  | otherwise = [i|#{closeLabel} #{closeTarget} [#{showKey closeReopenKey}]|]

    -- Closing an open PR gets the red closed-PR icon, like the web UI's close button
    closeIcon = [withAttr buttonGrayRedIconAttr (str (fst (subjectStateIcon PullClosed) <> " "))
                | isPR && issueState issue == StateOpen]

    commentButton
      | hasComment = actionButtonWidget buttonGreenAttr [str "Comment [Alt+Enter]"]
      -- Nothing to submit yet, so the button is inert and shows how to focus the box
      | otherwise = actionButtonWidget buttonGreenDisabledAttr [str [i|Comment [#{showKey commentKey}]|]]

    closeLabel :: String
    closeLabel = if issueState issue == StateOpen then "Close" else "Reopen"

    closeTarget :: String
    closeTarget = if isPR then "pull request" else "issue"

-- | A filled action button, styled like Brick.Widgets.Dialog's buttons: a flat
-- single-row fill with two spaces of padding on each side.
actionButtonWidget :: AttrName -> [Widget n] -> Widget n
actionButtonWidget fillAttr content =
  withAttr fillAttr $ hBox ([str "  "] <> content <> [str "  "])

-- * Details toggle

detailsToggleWidget :: AppState -> Widget n
detailsToggleWidget app = hBox [
  str "["
  , withAttr hotkeyAttr $ str $ showKey detailsToggleKey
  , str "] "
  , withAttr hotkeyMessageAttr $ str $ case _appDetailsExpanded app of
      DetailsExpanded -> "Collapse details"
      DetailsCollapsed -> "Expand details"
  ]
