
module Sauron.UI.Draw.Workflow (
  workflowInner
  ) where

import Brick
import GitHub
import Relude


workflowInner :: WorkflowRun -> Widget n
workflowInner (WorkflowRun {..}) = vBox [
  strWrap $ toString workflowRunDisplayTitle
  ]
