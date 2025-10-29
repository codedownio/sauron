{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

module Sauron.UI.Pull (
  pullLine
  , pullInner
  ) where

import Brick
import Brick.Widgets.Border
import Data.String.Interpolate
import Data.Time
import GitHub
import GitHub.Data.Name
import Relude
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Event (getEventDescription, getEventIconWithColor)
import Sauron.UI.Issue (maxCommentWidth)
import Sauron.UI.Markdown
import Sauron.UI.TimelineBorder
import Sauron.UI.Statuses (fetchableQuarterCircleSpinner)
import Sauron.UI.Util.TimeDiff


pullLine :: UTCTime -> Bool -> Issue -> Int -> Fetchable a -> Widget n
pullLine now toggled (Issue {issueNumber=(IssueNumber number), ..}) animationCounter fetchableState = vBox [line1, line2]
  where
    line1 = hBox [
      withAttr openMarkerAttr $ str (if toggled then "[-] " else "[+] ")
      , withAttr normalAttr $ str $ toString issueTitle
      , fetchableQuarterCircleSpinner animationCounter fetchableState
      , padLeft Max $ str "" -- (if pullComments > 0 then [i|🗨  #{pullComments}|] else "")
      ]

    line2 = padRight Max $ padLeft (Pad 4) $ hBox [
      withAttr hashAttr $ str "#"
      , withAttr hashNumberAttr $ str $ show number
      , str [i| opened #{timeFromNow (diffUTCTime now issueCreatedAt)} by |]
      , withAttr usernameAttr $ str $ [i|#{untagName $ simpleUserLogin issueUser}|]
      ]

pullInner :: UTCTime -> Issue -> Text -> Fetchable (NodeState 'SinglePullT) -> Widget n
pullInner now (Issue {..}) body inner = vBox (firstCell : comments)
  where
    SimpleUser {simpleUserLogin=(N openerUsername)} = issueUser

    firstCell = hLimit maxCommentWidth $ borderWithLabel
      (str [i|#{openerUsername} opened #{timeFromNow (diffUTCTime now issueCreatedAt)}|]
          & padLeftRight 1
      )
      (markdownToWidgetsWithWidth (maxCommentWidth - 2) body)

    comments :: [Widget n]
    comments = case inner of
      Fetched cs ->
        let items = toList cs
        in if null items then [] else verticalLine : renderItemsWithTimeline items
      Fetching maybeCs -> case maybeCs of
        Just cs ->
          let items = toList cs
          in if null items then [verticalLine, strWrap [i|Refreshing comments...|]] 
             else verticalLine : renderItemsWithTimeline items ++ [strWrap [i|Refreshing comments...|]]
        Nothing -> [verticalLine, strWrap [i|Fetching comments...|]]
      Errored err -> [verticalLine, strWrap [i|Failed to fetch comments: #{err}|]]
      NotFetched -> [verticalLine, strWrap [i|Comments not fetched.|]]

    verticalLine = withAttr timelineBorderAttr $ str "    │"  -- 4 spaces then blue vertical line

    renderItemsWithTimeline :: [Either IssueEvent IssueComment] -> [Widget n]
    renderItemsWithTimeline items = 
      let indexedItems = zip [0..] items
          totalItems = length items
      in fmap (\(idx, item) -> renderItemWithBorder idx totalItems item) indexedItems

    renderItemWithBorder :: Int -> Int -> Either IssueEvent IssueComment -> Widget n
    renderItemWithBorder idx totalItems item = 
      case item of
        Right comment -> renderComment idx totalItems comment
        Left event -> renderEvent event

    renderComment :: Int -> Int -> IssueComment -> Widget n
    renderComment idx totalItems (IssueComment {issueCommentUser=(SimpleUser {simpleUserLogin=(N username)}), ..}) = 
      let borderFunc = if totalItems == 1 
                       then firstTimelineBorder
                       else if idx == 0 
                       then firstTimelineBorder
                       else if idx == totalItems - 1 
                       then lastTimelineBorder
                       else middleTimelineBorder
      in hLimit maxCommentWidth $ borderFunc
           (topLabel username)
           (markdownToWidgetsWithWidth (maxCommentWidth - 2) issueCommentBody)

    renderEvent issueEvent =
      let actorName :: Text = case simpleUserLogin (issueEventActor issueEvent) of
            N username -> username
          eventText = getEventDescription (issueEventType issueEvent)
          iconWidget = getEventIconWithColor (issueEventType issueEvent)
          timeAgo = timeFromNow (diffUTCTime now (issueEventCreatedAt issueEvent))
      in hLimit maxCommentWidth $
           padLeftRight 2 $ hBox [
             iconWidget
             , str " "
             , withAttr usernameAttr $ str (toString actorName)
             , str " "
             , str eventText
             , str " "
             , withAttr italicText $ str timeAgo
           ]

    topLabel username = ((withAttr usernameAttr (str [i|#{username} |])) <+> str [i|commented #{timeFromNow (diffUTCTime now issueCreatedAt)}|])
                      & padLeftRight 1
