
module Sauron.UI.Draw.Issue (
  issueLine
  , issueInner
  ) where

import Brick
import Brick.Widgets.Border
import Commonmark hiding (str)
import Data.String.Interpolate
import Data.Time
import GitHub
import Relude
import Sauron.Types hiding (toggled)
import Sauron.UI.AttrMap
import Sauron.UI.Util.TimeFromNow


issueLine :: UTCTime -> Bool -> Issue -> Widget n
issueLine now toggled (Issue {issueNumber=(IssueNumber number), ..}) = vBox [line1, line2]
  where
    line1 = hBox [
      withAttr openMarkerAttr $ str (if toggled then "[-] " else "[+] ")
      , withAttr normalAttr $ str $ toString issueTitle
      , padLeft Max $ str (if issueComments > 0 then [i|🗨  #{issueComments}|] else "")
      ]

    line2 = padRight Max $ padLeft (Pad 4) $ hBox [
      str ("#" <> show number <> " ")
      , str [i|opened #{timeFromNow (diffUTCTime now issueCreatedAt)} by #{untagName $ simpleUserLogin issueUser}|]
      ]

issueInner :: Text -> Fetchable PaginatedItemInner -> Widget n
-- issueInner body = vBox [strWrap (toString body)]
issueInner body inner = vBox $ fmap border ([strWrap (show parsed)] <> comments)
  where
    parsed :: Either ParseError (Html ())
    parsed = commonmark "issue" body

    comments :: [Widget n]
    comments = case inner of
      Fetched (PaginatedItemInnerIssue cs) -> fmap renderComment (toList cs)
      _ -> []

    renderComment x@(IssueComment {}) = strWrap (show x)
