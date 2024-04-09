
module Sauron.UI.Search (
  searchInfo
  ) where

import Brick
import Brick.Forms
import Relude
import Sauron.Types
import Sauron.UI.AttrMap


searchInfo :: AppState -> Int -> Search -> Widget ClickableName
searchInfo appState identifier (SearchText t)
  | _appFormActiveIdentifier appState == Just identifier = renderForm (_appForm appState)
  | otherwise = withAttr searchAttr $ str (toString t)
searchInfo _ _ SearchNone = withAttr searchAttr $ str "(no search)"
