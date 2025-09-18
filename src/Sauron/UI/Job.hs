{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Sauron.UI.Job (
  jobLine
  , jobInner
  ) where

import Brick
import GitHub
import Relude
import Sauron.Types hiding (toggled)
import Sauron.UI.AttrMap

jobLine :: Bool -> Job -> Widget n
jobLine toggled job = hBox [
  withAttr openMarkerAttr $ str (if toggled then "[-] " else "[+] ")
  , withAttr normalAttr $ str $ show job
  , padLeft (Pad 1) $ greenCheck
  ]

jobInner :: Job -> Fetchable NodeState -> Widget n
jobInner job _jobInner = vBox [
  withAttr normalAttr $ str $ show job
  , padTop (Pad 1) $ str "Job details would go here"
  ]

greenCheck = withAttr greenCheckAttr (str "✓")