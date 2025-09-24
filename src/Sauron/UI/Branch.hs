module Sauron.UI.Branch (
  branchLine
  ) where

import Brick
import GitHub
import Relude
import Sauron.UI.AttrMap


branchLine :: Bool -> Branch -> Widget n
branchLine toggled (Branch {branchName, branchCommit}) = vBox [line1, line2]
  where
    line1 = hBox [
      withAttr openMarkerAttr $ str (if toggled then "[-] " else "[+] ")
      , withAttr branchAttr $ str $ toString branchName
      , padLeft Max $ withAttr normalAttr $ str "branch"
      ]

    line2 = padRight Max $ padLeft (Pad 4) $ hBox [
      str "Latest commit "
      , withAttr hashAttr $ str $ take 7 $ toString $ branchCommitSha branchCommit
      ]