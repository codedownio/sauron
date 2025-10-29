
module Sauron.UI.Issue (
  issueLine
  , issueInner

  , maxCommentWidth

  -- Common timeline rendering functions
  , renderTimelineItem
  , renderItemWithBorder
  , renderComment
  , renderEvent
  , commentTopLabel
  , topLabel
  ) where

import Brick
import Data.String.Interpolate
import Data.Time
import qualified Data.Vector as V
import GitHub
import GitHub.Data.Name
import Relude
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Event (getEventDescription, getEventIconWithColor)
import Sauron.UI.Markdown
import Sauron.UI.Statuses (fetchableQuarterCircleSpinner)
import Sauron.UI.TimelineBorder
import Sauron.UI.Util.TimeDiff


maxCommentWidth :: Int
maxCommentWidth = 120

issueLine :: UTCTime -> Bool -> Issue -> Int -> Fetchable (V.Vector (Either IssueEvent IssueComment)) -> Widget n
issueLine now toggled (Issue {issueNumber=(IssueNumber number), ..}) animationCounter fetchableState = vBox [line1, line2]
  where
    line1 = hBox [
      withAttr openMarkerAttr $ str (if toggled then "[-] " else "[+] ")
      , withAttr normalAttr $ str $ toString issueTitle
      , fetchableQuarterCircleSpinner animationCounter fetchableState
      , padLeft Max $ str (if issueComments > 0 then [i|🗨  #{issueComments}|] else "")
      ]

    line2 = padRight Max $ padLeft (Pad 4) $ hBox [
      withAttr hashAttr $ str "#"
      , withAttr hashNumberAttr $ str $ show number
      , str [i| opened #{timeFromNow (diffUTCTime now issueCreatedAt)} by |]
      , withAttr usernameAttr $ str $ [i|#{untagName $ simpleUserLogin issueUser}|]
      ]

issueInner :: UTCTime -> Issue -> V.Vector (Either IssueEvent IssueComment) -> Widget n
-- issueInner now issue body cs = vBox [strWrap (show issue), strWrap (show body), strWrap (show cs)]
issueInner now (Issue {issueUser=(SimpleUser {simpleUserLogin=(N openerUsername)}), ..}) cs =
  allItems
  & zip [0..]
  & fmap (uncurry (renderTimelineItem now (length allItems)))
  & vBox
  where
    commentsAndEvents :: [Either IssueEvent IssueComment]
    commentsAndEvents = toList cs

    issueDescriptionBody = fromMaybe "*No description provided.*" issueBody

    -- allItems :: _
    allItems = (Left (openerUsername, issueDescriptionBody, issueCreatedAt), "")
             : fmap (\item -> (Right item, "")) commentsAndEvents

-- * Util, exported for Pull.hs

renderTimelineItem :: UTCTime -> Int -> Int -> (Either (Text, Text, UTCTime) (Either IssueEvent IssueComment), Text) -> Widget n
renderTimelineItem now totalItems idx (itemType, _extraBody) =
  let borderFunc = if totalItems == 1
                   then standaloneTimelineBorder
                   else if idx == 0
                   then firstTimelineBorder
                   else if idx == totalItems - 1
                   then lastTimelineBorder
                   else middleTimelineBorder
  in case itemType of
    Left (username, descriptionBody, createdAt) -> -- Issue/PR description
      hLimit maxCommentWidth $ borderFunc
        (topLabel username createdAt now)
        (markdownToWidgetsWithWidth (maxCommentWidth - 2) descriptionBody)
    Right item -> renderItemWithBorder now borderFunc item

topLabel :: Text -> UTCTime -> UTCTime -> Widget n
topLabel username createdAt now =
  (withAttr usernameAttr (str [i|#{username} |]) <+> str [i|opened #{timeFromNow (diffUTCTime now createdAt)}|])
    & padLeftRight 1

renderItemWithBorder :: UTCTime -> (Widget n -> Widget n -> Widget n) -> Either IssueEvent IssueComment -> Widget n
renderItemWithBorder now borderFunc item =
  case item of
    Right comment -> renderComment now borderFunc comment
    Left event -> renderEvent now event

renderComment :: UTCTime -> (Widget n -> Widget n -> Widget n) -> IssueComment -> Widget n
renderComment now borderFunc (IssueComment {issueCommentUser=(SimpleUser {simpleUserLogin=(N username)}), issueCommentCreatedAt, ..}) =
  hLimit maxCommentWidth $ borderFunc
    (commentTopLabel username commentTime now)
    (markdownToWidgetsWithWidth (maxCommentWidth - 2) issueCommentBody)
  where commentTime = issueCommentCreatedAt

renderEvent :: UTCTime -> IssueEvent -> Widget n
renderEvent now issueEvent =
  let actorName :: Text = case simpleUserLogin (issueEventActor issueEvent) of
        N username -> username
      eventText = getEventDescription (issueEventType issueEvent)
      iconWidget = getEventIconWithColor (issueEventType issueEvent)
      timeAgo = timeFromNow (diffUTCTime now (issueEventCreatedAt issueEvent))
  in hLimit maxCommentWidth $
       padLeft (Pad 4) $ hBox [
         iconWidget
         , str " "
         , withAttr usernameAttr $ str (toString actorName)
         , str " "
         , str eventText
         , str " "
         , withAttr italicText $ str timeAgo
       ]

commentTopLabel :: Text -> UTCTime -> UTCTime -> Widget n
commentTopLabel username commentTime now =
  (withAttr usernameAttr (str [i|#{username} |]) <+> str [i|commented #{timeFromNow (diffUTCTime now commentTime)}|])
    & padLeftRight 1
