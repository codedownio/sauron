
module Sauron.UI.Issue (
  issueLine
  , issueInner

  , maxCommentWidth
  ) where

import Brick
import Brick.Widgets.Border
import Data.String.Interpolate
import Data.Time
import GitHub
import GitHub.Data.Name
import Relude
import Sauron.Types hiding (toggled)
import Sauron.UI.AttrMap
import Sauron.UI.Markdown
import Sauron.UI.Util.TimeDiff


maxCommentWidth :: Int
maxCommentWidth = 120

issueLine :: UTCTime -> Bool -> Issue -> Widget n
issueLine now toggled (Issue {issueNumber=(IssueNumber number), ..}) = vBox [line1, line2]
  where
    line1 = hBox [
      withAttr openMarkerAttr $ str (if toggled then "[-] " else "[+] ")
      , withAttr normalAttr $ str $ toString issueTitle
      , padLeft Max $ str (if issueComments > 0 then [i|🗨  #{issueComments}|] else "")
      ]

    line2 = padRight Max $ padLeft (Pad 4) $ hBox [
      withAttr hashAttr $ str "#"
      , withAttr hashNumberAttr $ str $ show number
      , str [i| opened #{timeFromNow (diffUTCTime now issueCreatedAt)} by |]
      , withAttr usernameAttr $ str $ [i|#{untagName $ simpleUserLogin issueUser}|]
      ]

issueInner :: UTCTime -> Issue -> Text -> Fetchable PaginatedItemInner -> Widget n
-- issueInner body = vBox [strWrap (toString body)]
issueInner now (Issue {issueUser=(SimpleUser {simpleUserLogin=(N openerUsername)}), ..}) body inner = vBox (firstCell : comments)
  where
    firstCell = hLimit maxCommentWidth $ borderWithLabel
      ((withAttr usernameAttr (str [i|#{openerUsername} |]) <+> str [i|commented #{timeFromNow (diffUTCTime now issueCreatedAt)}|])
          & padLeftRight 1
      )
      (markdownToWidgetsWithWidth maxCommentWidth body)

    comments :: [Widget n]
    comments = case inner of
      Fetched (PaginatedItemInnerIssue cs) -> fmap renderComment (toList cs)
      _ -> []

    -- TODO: use issueCommentUpdatedAt
    renderComment (IssueComment {issueCommentUser=(SimpleUser {simpleUserLogin=(N username)}), ..}) = hLimit maxCommentWidth $ borderWithLabel
      (str [i|#{username} commented #{timeFromNow (diffUTCTime now issueCommentCreatedAt)}|]
          & padLeftRight 1
      )
      (markdownToWidgetsWithWidth maxCommentWidth issueCommentBody)
