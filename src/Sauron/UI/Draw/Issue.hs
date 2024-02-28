
module Sauron.UI.Draw.Issue (
  issueLine
  , issueInner
  ) where

import Brick
import Commonmark hiding (str)
import Data.String.Interpolate
import Data.Time
import GitHub
import Relude
import Sauron.UI.AttrMap
import Sauron.UI.Util.TimeFromNow


issueLine :: UTCTime -> Bool -> Issue -> Widget n
issueLine now toggled (Issue {issueNumber=(IssueNumber number), ..}) = hBox [
  withAttr openMarkerAttr $ str (if toggled then "[-] " else "[+] ")
  , str ("#" <> show number <> " ")
  , withAttr normalAttr $ str $ toString issueTitle
  , padLeft Max $ str ([i|#{timeFromNow (diffUTCTime now issueCreatedAt)} by #{untagName $ simpleUserLogin issueUser}|]
                       <> if issueComments > 0 then [i|, 🗨  #{issueComments}|] else "")
  ]


issueInner :: Text -> Widget n
-- issueInner body = vBox [strWrap (toString body)]
issueInner body = vBox [strWrap (show parsed)]
  where
    parsed :: Either ParseError (Html ())
    parsed = commonmark "issue" body
