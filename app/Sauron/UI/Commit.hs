module Sauron.UI.Commit (
  commitLine
  ) where

import Brick
import GitHub
import Relude
import Sauron.UI.AttrMap


commitLine :: Bool -> Commit -> Widget n
commitLine toggled (Commit {commitSha, commitGitCommit, commitAuthor}) = vBox [line1, line2]
  where
    line1 = hBox [
      withAttr openMarkerAttr $ str (if toggled then "[-] " else "[+] ")
      , withAttr normalAttr $ str $ toString $ gitCommitMessage commitGitCommit
      ]

    line2 = padRight Max $ padLeft (Pad 4) $ hBox [
      str "Commit "
      , withAttr hashAttr $ str $ take 7 $ toString $ untagName commitSha
      , case commitAuthor of
          Just author -> str (" by " <> toString (untagName (simpleUserLogin author)))
          Nothing -> case gitCommitAuthor commitGitCommit of
            gitAuthor -> str (" by " <> toString (gitUserName gitAuthor))
      ]