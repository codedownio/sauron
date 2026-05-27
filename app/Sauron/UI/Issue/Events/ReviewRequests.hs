module Sauron.UI.Issue.Events.ReviewRequests (
  renderReviewRequestGroup
  ) where

import Brick
import Data.Time
import GitHub
import GitHub.Data.Name
import Graphics.Text.Width (safeWcswidth, safeWctwidth)
import Relude
import Sauron.UI.AttrMap
import Sauron.UI.Event (getEventIconWithColor)
import Sauron.UI.Issue.Events (adaptiveWidth)
import Sauron.UI.Issue.Events.Common (wrapWidgets)
import Sauron.UI.Util.TimeDiff


renderReviewRequestGroup :: UTCTime -> Bool -> IssueEvent -> [IssueEvent] -> Widget n
renderReviewRequestGroup now isLast rep events =
  let actorName :: Text = case issueEventActor rep of
        Just (SimpleUser {simpleUserLogin=(N username)}) -> username
        Nothing -> "ghost"
      timeAgo = timeFromNow (diffUTCTime now (issueEventCreatedAt rep))
      iconWidget = getEventIconWithColor (issueEventEvent rep)
      reviewerNames = mapMaybe (\e -> case issueEventRequestedReviewer e of
        Just (SimpleUser {simpleUserLogin=(N name)}) -> Just name
        Nothing -> Nothing) events
      sizedName n = (safeWctwidth n, withAttr usernameAttr $ str (toString n))
      timeAgoWidget = (safeWcswidth timeAgo, withAttr italicText $ str timeAgo)
      descriptionItems = case reviewerNames of
        [] -> [(7, str "review "), timeAgoWidget]
        [x] -> [(12, str "review from "), sizedName x, (1, str " "), timeAgoWidget]
        xs -> case nonEmpty xs of
          Nothing -> [(7, str "review "), timeAgoWidget]
          Just ne ->
            [(12, str "review from ")]
            ++ intercalate [(2, str ", ")] [[sizedName n] | n <- toList (init ne)]
            ++ [(6, str ", and "), sizedName (last ne), (1, str " "), timeAgoWidget]
      prefixWidth = 1 + 2 + safeWctwidth actorName + 1 + 10  -- icon + spaces + username + space + "requested "
      eventLine = adaptiveWidth $ \w ->
        let availableWidth = w - 4
            wrappedLines = wrapWidgets (availableWidth - prefixWidth) descriptionItems
            prefix = hBox [iconWidget, str "  ", withAttr usernameAttr $ str (toString actorName), str " requested "]
            firstLine = case wrappedLines of
              [] -> prefix
              (l:_) -> hBox [prefix, hBox (map snd l)]
            restLines = case wrappedLines of
              [] -> []
              (_:ls) -> [padLeft (Pad 3) $ hBox (map snd l) | l <- ls]
        in padLeft (Pad 4) $ vBox (firstLine : restLines)
      continuationLine = if isLast
        then emptyWidget
        else adaptiveWidth $ \_ -> padLeft (Pad 4) $ withAttr timelineBorderAttr $ str "│"
  in vBox [eventLine, continuationLine]
