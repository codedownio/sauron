{-# LANGUAGE GADTs #-}

-- | Rendering a node's own line and inner content inside a modal, shared by the zoom
-- modal and the pull request modal's Conversation tab.
module Sauron.UI.Modals.NodeContent (
  renderNodeContent
  ) where

import Brick
import Lens.Micro
import Relude
import Sauron.Types
import Sauron.UI


renderNodeContent :: AppState -> SomeNode Fixed -> Widget ClickableName
renderNodeContent appState (SomeNode inner) = vBox $ catMaybes [
  if skipLine then Nothing else Just $ drawNodeLine appState inner'
  , fmap (padLeft (Pad paddingAmount)) innerContent
  , if isNothing innerContent then loadingWidget else Nothing
  ]
  where
    inner' = over entityDataL transformEntityData inner
    innerContent = drawNodeInner appState inner'

    loadingWidget = case inner of
      SingleIssueNode {} -> Just $ str "Loading..."
      SinglePullNode {} -> Just $ str "Loading..."
      SingleCommitNode {} -> Just $ str "Loading..."
      SingleJobNode {} -> Just $ str "Loading..."
      _ -> Nothing

    transformEntityData :: EntityData Fixed a -> EntityData Fixed a
    transformEntityData = set toggled True
                        . over ident (\x -> -x) -- Flip the sign so the viewport doesn't collide with one in the main UI

    -- For issues and PRs, skip the list heading line and show content directly
    skipLine = case inner of
      SingleIssueNode {} -> True
      SinglePullNode {} -> True
      _ -> False

    paddingAmount = if skipLine then 0 else 1
