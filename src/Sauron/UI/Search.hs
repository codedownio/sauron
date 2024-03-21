
module Sauron.UI.Search (
  searchInfo
  ) where

import Brick
import Relude
import Sauron.Types
import Sauron.UI.AttrMap


searchInfo :: Search -> Widget n
searchInfo (SearchText t) = withAttr searchAttr $ str (toString t)
searchInfo SearchNone = withAttr searchAttr $ str "(no search)"
