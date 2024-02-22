
module Sauron.UI.Draw.Border (
  borderWithCounts
  ) where

import Brick
import Brick.Widgets.Border
import Data.String.Interpolate
import GitHub hiding (Status)
import GitHub.Data.Name
import Relude
import Sauron.Types


borderWithCounts :: AppState -> Widget n
borderWithCounts (AppState {_appUser=(User {userLogin=(N name), ..})}) = hBorderWithLabel $ padLeftRight 1 $ hBox [str [i|#{name} (#{userPublicRepos} public repos, #{userFollowers} followers)|]]
