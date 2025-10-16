
module Sauron.UI.Border (
  borderWithCounts
  ) where

import Brick
import Brick.Widgets.Border
import Data.String.Interpolate
import GitHub hiding (Status)
import GitHub.Data.Name
import Relude
import Sauron.Types
import Sauron.UI.AttrMap


borderWithCounts :: AppState -> Widget n
borderWithCounts (AppState {_appUser=(User {userLogin=(N name), ..})}) = hBorderWithLabel $ padLeftRight 1 $ hBox [
  withAttr usernameAttr $ str [i|#{name} |]
  , str [i|(#{userPublicRepos} public repos, #{userFollowers} followers)|]
  ]
