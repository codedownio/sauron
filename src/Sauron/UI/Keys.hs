{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Sauron.UI.Keys where

import qualified Data.List as L
import qualified Graphics.Vty as V
import Relude

-- Column 1
nextKey = V.KChar 'n'
previousKey = V.KChar 'p'
nextFailureKey = V.KChar 'N'
previousFailureKey = V.KChar 'P'
toggleKeys = [V.KEnter, V.KChar '\t']

-- Column 2
refreshAllKey = V.KChar 'R'
refreshSelectedKey = V.KChar 'r'

openSelectedFolderInFileExplorer = V.KChar 'o'

browserToHomeKey = V.KChar 'h'
browserToIssuesKey = V.KChar 'i'
browserToPullsKey = V.KChar 'P'
browserToActionsKey = V.KChar 'a'

-- Column 3
toggleFileLocationsKey = V.KChar 'F'
toggleVisibilityThresholdsKey = V.KChar 'V'
debugKey = V.KChar 'd'
infoKey = V.KChar 'i'
warnKey = V.KChar 'w'
errorKey = V.KChar 'e'
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
