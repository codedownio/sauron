
module Sauron.UI.Issue (
  issueLine
  , issueInner

  , maxCommentWidth
  ) where

import Brick
import Brick.Widgets.Border
import Data.String.Interpolate
import Data.Time
import qualified Data.Vector as V
import GitHub
import GitHub.Data.Name
import Relude
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Markdown
import Sauron.UI.Statuses (fetchableQuarterCircleSpinner)
import Sauron.UI.Util.TimeDiff


maxCommentWidth :: Int
maxCommentWidth = 120

issueLine :: UTCTime -> Bool -> Issue -> Int -> Fetchable (V.Vector IssueComment) -> Widget n
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

issueInner :: UTCTime -> Issue -> V.Vector IssueComment -> Widget n
-- issueInner now issue body cs = vBox [strWrap (show issue), strWrap (show body), strWrap (show cs)]
issueInner now (Issue {issueUser=(SimpleUser {simpleUserLogin=(N openerUsername)}), ..}) cs = vBox (addFirst comments)
  where
    addFirst = case issueBody of
      Nothing -> (firstCell "*No description provided.*" :)
      Just body -> (firstCell body :)

    firstCell body = hLimit maxCommentWidth $ borderWithLabel
      (topLabel openerUsername)
      (markdownToWidgetsWithWidth (maxCommentWidth - 2) body)

    comments :: [Widget n]
    comments = fmap renderComment (toList cs)

    renderComment (IssueComment {issueCommentUser=(SimpleUser {simpleUserLogin=(N username)}), issueCommentCreatedAt, ..}) = hLimit maxCommentWidth $ borderWithLabel
      (commentTopLabel username issueCommentCreatedAt)
      (markdownToWidgetsWithWidth (maxCommentWidth - 2) issueCommentBody)

    commentTopLabel username commentTime = (withAttr usernameAttr (str [i|#{username} |]) <+> str [i|commented #{timeFromNow (diffUTCTime now commentTime)}|])
                      & padLeftRight 1

    topLabel username = (withAttr usernameAttr (str [i|#{username} |]) <+> str [i|commented #{timeFromNow (diffUTCTime now issueCreatedAt)}|])
                      & padLeftRight 1
