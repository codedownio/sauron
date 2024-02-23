
module Sauron.UI.Draw.Issue (
  issueInner
  ) where

import Brick
import Commonmark
import Relude

-- issueInner body = vBox [strWrap (toString body)]
issueInner body = vBox [strWrap (show parsed)]
  where
    parsed :: Either ParseError (Html ())
    parsed = commonmark "issue" body
