
module Sauron.UI.Search (
  searchInfo
  ) where

import Brick
import Brick.Forms
import Relude
import Sauron.Types
import Sauron.UI.AttrMap


searchInfo :: AppState -> Int -> Search -> Widget ClickableName
searchInfo appState identifier (SearchText t) = case _appForm appState of
  Just (form, formIdentifier) | formIdentifier == identifier -> hLimit 30 $ renderForm form
  _ -> withAttr searchAttr $ str (toString t)
searchInfo _ _ SearchNone = withAttr searchAttr $ str "(no search)"
