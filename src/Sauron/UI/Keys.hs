{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Sauron.UI.Keys where

import qualified Data.List as L
import qualified Graphics.Vty as V

-- Column 1
nextKey = V.KChar 'n'
previousKey = V.KChar 'p'
nextFailureKey = V.KChar 'N'
previousFailureKey = V.KChar 'P'
closeNodeKey = V.KLeft
openNodeKey = V.KRight
toggleKeys = [V.KEnter, V.KChar '\t']

-- Column 2
cancelAllKey = V.KChar 'C'
cancelSelectedKey = V.KChar 'c'
runAllKey = V.KChar 'R'
runSelectedKey = V.KChar 'r'
clearAllKey = V.KChar 'K'
clearSelectedKey = V.KChar 'k'
openSelectedFolderInFileExplorer = V.KChar 'o'
openTestRootKey = V.KChar 'O'
openTestInEditorKey = V.KChar 't'
openFailureInEditorKey = V.KChar 'f'
openLogsInEditorKey = V.KChar 'l'

-- Column 3
cycleVisibilityThresholdKey = V.KChar 'v'
toggleShowRunTimesKey = V.KChar 'T'
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
