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
exitKey = V.KChar 'q'

-- Column 2
browserToHomeKey = V.KChar 'H'
browserToIssuesKey = V.KChar 'I'
browserToPullsKey = V.KChar 'P'
browserToActionsKey = V.KChar 'A'

nextPageKey = V.KChar 'N'
prevPageKey = V.KChar 'P'
firstPageKey = V.KChar 'F'
lastPageKey = V.KChar 'L'

refreshAllKey = V.KChar 'R'
refreshSelectedKey = V.KChar 'r'

openSelectedKey = V.KChar 'o'

-- Column 3
editSearchKey = V.KChar 's'
commentKey = V.KChar 'c'
closeReopenKey = V.KChar 'C'
cancelWorkflowKey = V.KChar 'C'
zoomModalKey = V.KChar 'z'
newIssueKey = V.KChar 'c'
markNotificationDoneKey = V.KChar 'd'
markNotificationReadKey = V.KChar 'R'

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
