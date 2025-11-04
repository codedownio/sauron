{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Sauron.UI.Keys where

import qualified Data.List as L
import qualified Graphics.Vty as V
import Relude

-- Column 1
nextKey = V.KChar 'n'
previousKey = V.KChar 'p'
toggleKeys = [V.KEnter, V.KChar '\t']

-- Column 2
refreshAllKey = V.KChar 'R'
refreshSelectedKey = V.KChar 'r'

browserToHomeKey = V.KChar 'h'
browserToIssuesKey = V.KChar 'i'
browserToPullsKey = V.KChar 'p'
browserToActionsKey = V.KChar 'a'

openSelectedKey = V.KChar 'o'

-- Column 3
nextPageKey = V.KChar 'N'
prevPageKey = V.KChar 'P'
firstPageKey = V.KChar 'F'
lastPageKey = V.KChar 'L'

editSearchKey = V.KChar 's'
commentKey = V.KChar 'c'
zoomModalKey = V.KChar 'z'

exitKey = V.KChar 'q'

-- Other

showKey (V.KChar '\t') = "Tab"
showKey (V.KChar c) = [c]
showKey V.KEnter = "Enter"
showKey _ = "?"

showKeys = L.intercalate "/" . fmap showKey

unKChar :: V.Key -> Char
unKChar (V.KChar c) = c
unKChar V.KLeft = '←'
unKChar V.KRight = '→'
unKChar _ = '?'
