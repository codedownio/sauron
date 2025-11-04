module Sauron.UI.Notification (
  notificationLine
  , notificationInner
  ) where

import Brick
import Data.String.Interpolate
import Data.Time
import GitHub
import Relude
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Statuses (fetchableQuarterCircleSpinner)
import Sauron.UI.Util.TimeDiff (timeFromNow)

notificationLine :: UTCTime -> Bool -> Notification -> Int -> Fetchable a -> Widget n
notificationLine now toggled' (Notification {..}) animationCounter fetchableState =
  if notificationUnread
    -- then withAttr unreadNotificationAttr $ vBox [line1, line2]
    then vBox [line1, line2]
    else vBox [line1, line2]
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

notificationInner :: UTCTime -> Notification -> Widget n
notificationInner now notification = vBox notificationDetails
  where
    Notification {..} = notification
    Subject {..} = notificationSubject
    RepoRef {..} = notificationRepo
    SimpleOwner {..} = repoRefOwner

    notificationDetails = [
      -- Notification status
      hBox [
        withAttr boldText $ str "Status: "
        , if notificationUnread
            then withAttr erroredAttr $ str "🔴 Unread"
            else withAttr greenCheckAttr $ str "✓ Read"
        ]

      -- Repository info
      , padTop (Pad 1) $ hBox [
          withAttr boldText $ str "Repository: "
          , withAttr hashAttr $ str (toString $ untagName simpleOwnerLogin)
          , str "/"
          , withAttr normalAttr $ str (toString $ untagName repoRefRepo)
        ]

      -- Subject details
      , padTop (Pad 1) $ hBox [
          withAttr boldText $ str "Type: "
          , str (toString subjectType)
        ]

      , padTop (Pad 1) $ hBox [
          withAttr boldText $ str "Reason: "
          , str (toString $ reasonText notificationReason)
        ]

      -- Timestamps
      , case notificationUpdatedAt of
          Just updatedAt -> padTop (Pad 1) $ hBox [
              withAttr boldText $ str "Updated: "
              , str $ timeFromNow (diffUTCTime now updatedAt) ++ " (" ++ show updatedAt ++ ")"
            ]
          Nothing -> emptyWidget

      , case notificationLastReadAt of
          Just lastReadAt -> padTop (Pad 1) $ hBox [
              withAttr boldText $ str "Last read: "
              , str $ timeFromNow (diffUTCTime now lastReadAt) ++ " (" ++ show lastReadAt ++ ")"
            ]
          Nothing -> emptyWidget

      -- URLs
      , case subjectURL of
          Just url -> padTop (Pad 1) $ hBox [
              withAttr boldText $ str "URL: "
              , withAttr usernameAttr $ str (toString $ getUrl url)
            ]
          Nothing -> emptyWidget

      , case subjectLatestCommentURL of
          Just commentUrl -> padTop (Pad 1) $ hBox [
              withAttr boldText $ str "Latest comment: "
              , withAttr usernameAttr $ str (toString $ getUrl commentUrl)
            ]
          Nothing -> emptyWidget
      ]
