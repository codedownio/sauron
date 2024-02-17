{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Sauron.UI.Draw.IssueLine (
  issueWidget
  ) where

import Brick
import GitHub hiding (Status)
import Relude
import Sauron.UI.AttrMap


issueWidget :: Issue -> Widget n
issueWidget (Issue {issueNumber=(IssueNumber number), ..}) = hBox [
  str ("#" <> show number <> " ")
  , withAttr normalAttr $ str $ toString issueTitle
  ]

-- To render:
-- issueComments (number)
-- issueState (open or closed)
