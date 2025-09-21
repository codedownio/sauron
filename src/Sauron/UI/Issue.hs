
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
import Sauron.Types hiding (toggled)
import Sauron.UI.AttrMap
import Sauron.UI.Markdown
import Sauron.UI.Util.TimeDiff
import Sauron.UI.Workflow (fetchableQuarterCircleSpinner)


maxCommentWidth :: Int
maxCommentWidth = 120

issueLine :: UTCTime -> Bool -> Issue -> Int -> Fetchable a -> Widget n
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

issueInner :: UTCTime -> Issue -> Text -> Fetchable (V.Vector IssueComment) -> Widget n
-- issueInner body = vBox [strWrap (toString body)]
issueInner now (Issue {issueUser=(SimpleUser {simpleUserLogin=(N openerUsername)}), ..}) body inner = vBox (firstCell : comments)
  where
    firstCell = hLimit maxCommentWidth $ borderWithLabel
      (topLabel openerUsername)
      (markdownToWidgetsWithWidth (maxCommentWidth - 2) body)

    comments :: [Widget n]
    comments = case inner of
      Fetched cs -> fmap renderComment (toList cs)
      _ -> []

    -- TODO: use issueCommentUpdatedAt
    renderComment (IssueComment {issueCommentUser=(SimpleUser {simpleUserLogin=(N username)}), ..}) = hLimit maxCommentWidth $ borderWithLabel
      (topLabel username)
      (markdownToWidgetsWithWidth (maxCommentWidth - 2) issueCommentBody)

    topLabel username = ((withAttr usernameAttr (str [i|#{username} |])) <+> str [i|commented #{timeFromNow (diffUTCTime now issueCreatedAt)}|])
                      & padLeftRight 1
