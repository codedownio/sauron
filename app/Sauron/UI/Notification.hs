{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sauron.UI.Notification (
  notificationLine
  , notificationInner
  ) where

import Brick
import Control.Monad
import Data.Char (isDigit)
import Data.String.Interpolate
import Data.Time
import qualified Data.Vector as V
import GitHub
import GitHub.Data.Name
import Lens.Micro
import Network.URI (parseURI, uriPath)
import Relude
import Sauron.Actions (refreshLine, refreshOnZoom)
import Sauron.Actions.Util (findNotificationsParent)
import Sauron.Event.Helpers (withFixedElemAndParents)
import Sauron.Mutations.Notification (markNotificationAsDone, markNotificationAsRead)
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Issue (renderTimelineItemWithAttr, consolidateEvents, TimelineItem(..))
import Sauron.UI.Keys (markNotificationDoneKey, markNotificationReadKey, zoomModalKey, showKey)
import Sauron.UI.Statuses (fetchableQuarterCircleSpinner)
import Sauron.UI.Util.TimeDiff (timeFromNow)
import qualified System.FilePath.Posix as FP
import UnliftIO.Async (async)


instance ListDrawable Fixed 'SingleNotificationT where
  drawLine appState (EntityData {_static=notification, ..}) =
    notificationLine (_appNow appState) _toggled notification (_appAnimationCounter appState) (notificationStateContent _state)

  drawInner appState (EntityData {_static=notification, _state, ..}) = do
    guard _toggled
    return $ notificationInner (_appNow appState) notification _state

  getExtraTopBoxWidgets _appState (EntityData {_static=notification}) =
    [hBox [str "["
                , withAttr hotkeyAttr $ str $ showKey markNotificationDoneKey
                , str "] "
                , withAttr hotkeyMessageAttr $ str "Mark done"
                ]
    ]
    ++ (if notificationUnread notification
        then [hBox [str "["
                   , withAttr hotkeyAttr $ str $ showKey markNotificationReadKey
                   , str "] "
                   , withAttr hotkeyMessageAttr $ str "Mark read"
                   ]
             ]
        else []
       )
    ++ [hBox [str "["
             , withAttr hotkeyAttr $ str $ showKey zoomModalKey
             , str "] "
             , withAttr hotkeyMessageAttr $ str "Zoom"
             ]
       ]

  handleHotkey appState key (EntityData {_static=notification})
    | key == zoomModalKey = do
        withFixedElemAndParents appState $ \(SomeNode _) (SomeNode variableEl) parents -> do
          refreshOnZoom (appState ^. appBaseContext) variableEl parents
          liftIO $ atomically $ writeTVar (_appModalVariable appState) (Just (ZoomModalState (SomeNode variableEl) (toList parents)))
        return True
    | key == markNotificationDoneKey = do
        liftIO $ void $ async $ do
          runReaderT (markNotificationAsDone notification) (appState ^. appBaseContext)
          refreshParentNotifications appState
        return True
    | key == markNotificationReadKey && notificationUnread notification = do
        liftIO $ void $ async $ do
          runReaderT (markNotificationAsRead notification) (appState ^. appBaseContext)
          refreshParentNotifications appState
        return True
  handleHotkey _ _ _ = return False

refreshParentNotifications :: MonadIO m => AppState -> m ()
refreshParentNotifications appState =
  withFixedElemAndParents appState $ \_ _ parents ->
    whenJust (findNotificationsParent parents) $ \notificationsNode ->
      liftIO $ void $ refreshLine (appState ^. appBaseContext) notificationsNode parents

notificationLine :: UTCTime -> Bool -> Notification -> Int -> Fetchable a -> Widget n
notificationLine now toggled' (Notification {..}) animationCounter fetchableState = vBox [line1, line2]
  where
    Subject {..} = notificationSubject
    RepoRef {repoRefOwner=(SimpleOwner {..}), ..} = notificationRepo

    line1 = hBox (catMaybes [
      Just $ withAttr openMarkerAttr $ str (if toggled' then "[-] " else "[+] ")
      , Just $ withAttr normalAttr $ str $ toString subjectTitle
      , if notificationUnread then Just (withAttr blueDotAttr $ str " ●") else Nothing
      , Just $ fetchableQuarterCircleSpinner animationCounter fetchableState
      , Just $ padLeft Max $ hBox [
          case subjectType of
            "PullRequest" -> str "🔄"
            "Issue" -> str "◉"
            _ -> str (toString subjectType)
        ]
      ])

    line2 = padRight Max $ padLeft (Pad 4) $ hBox [
      withAttr hashAttr $ str (toString $ untagName simpleOwnerLogin)
      , str "/"
      , withAttr boldText $ str (toString $ untagName repoRefRepo)
      , str [i| • #{reasonText notificationReason}|]
      , case notificationUpdatedAt of
          Just updatedAt -> str [i| • updated #{timeFromNow (diffUTCTime now updatedAt)}|]
          Nothing -> str ""
      ]

reasonText :: NotificationReason -> Text
reasonText reason = case reason of
  ApprovalRequestedReason -> "approval requested"
  AssignReason -> "assigned"
  AuthorReason -> "author"
  CommentReason -> "comment"
  CiActivityReason -> "ci activity"
  InvitationReason -> "invitation"
  ManualReason -> "manual"
  MemberFeatureRequestedReason -> "member feature requested"
  MentionReason -> "mention"
  ReviewRequestedReason -> "review requested"
  SecurityAlertReason -> "security alert"
  SecurityAdvisoryCreditReason -> "security advisory credit"
  StateChangeReason -> "state changed"
  SubscribedReason -> "subscribed"
  TeamMentionReason -> "team mention"

notificationInner :: UTCTime -> Notification -> NotificationState -> Widget n
notificationInner now notification (NotificationState {..}) = vBox (notificationDetails ++ contentWidget)
  where
    contentState = notificationStateContent
    Notification {..} = notification
    Subject {..} = notificationSubject

    latestCommentId :: Maybe Int
    latestCommentId = do
      commentUrl <- subjectLatestCommentURL
      uri <- parseURI (toString $ getUrl commentUrl)
      let segments = FP.splitPath (uriPath uri)
      -- Only extract an ID if the URL is actually a comment URL (contains "comments" segment)
      guard ("comments/" `elem` segments)
      case reverse segments of
        (idStr:_) | all isDigit idStr -> readMaybe idStr
        _ -> Nothing

    notificationDetails = [
      -- Notification status
      hBox [
        withAttr boldText $ str "Status: "
        , if notificationUnread
            then withAttr erroredAttr $ str "🔴 Unread"
            else withAttr greenCheckAttr $ str "✓ Read"
        ]
      ]

    contentWidget = case contentState of
      Fetched (NotificationIssue issue comments) ->
        [padTop (Pad 1) $ renderNotificationContent now issue comments latestCommentId notificationStateAutoScroll]
      Fetched (NotificationPull issue comments) ->
        [padTop (Pad 1) $ renderNotificationContent now issue comments latestCommentId notificationStateAutoScroll]
      Fetched NotificationOther -> []
      Fetching _ -> [padTop (Pad 1) $ str "Loading..."]
      Errored err -> [padTop (Pad 1) $ withAttr erroredAttr $ str [i|Error: #{err}|]]
      NotFetched -> []

renderNotificationContent :: UTCTime -> Issue -> V.Vector (Either IssueEvent IssueComment) -> Maybe Int -> Bool -> Widget n
renderNotificationContent now (Issue {issueUser=(SimpleUser {simpleUserLogin=(N openerUsername)}), ..}) cs maybeHighlightId autoScroll =
  allItems
  & zip [0..]
  & fmap render
  & vBox
  where
    allItems = (Left (openerUsername, fromMaybe "*No description provided.*" issueBody, issueCreatedAt), "")
             : fmap (\item -> (Right item, "")) (consolidateEvents (toList cs))

    render (idx, item) = if isHighlighted && autoScroll then visible widget else widget
      where
        isHighlighted = case (fst item, maybeHighlightId) of
          (_, Nothing) -> False
          (Right (SingleItem (Right (IssueComment {issueCommentId=cid}))), Just hid) -> cid == hid
          _ -> False

        widget = renderTimelineItemWithAttr (if isHighlighted then Just highlightedCommentAttr else Nothing) now (length allItems) idx item
