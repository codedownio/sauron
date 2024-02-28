
module Sauron.UI.Draw.Issue (
  issueLine
  , issueInner
  ) where

import Brick
import Commonmark hiding (str)
import Data.String.Interpolate
import GitHub
import Relude
import Sauron.UI.AttrMap


issueLine :: Bool -> Issue -> Widget n
issueLine toggled (Issue {issueNumber=(IssueNumber number), ..}) = hBox [
  withAttr openMarkerAttr $ str (if toggled then "[-] " else "[+] ")
  , str ("#" <> show number <> " ")
  , withAttr normalAttr $ str $ toString issueTitle
  , padLeft Max (str [i|#{issueCreatedAt} by #{untagName $ simpleUserLogin issueUser}, #{issueComments}|])
  ]


issueInner :: Text -> Widget n
-- issueInner body = vBox [strWrap (toString body)]
issueInner body = vBox [strWrap (show parsed)]
  where
    parsed :: Either ParseError (Html ())
    parsed = commonmark "issue" body
