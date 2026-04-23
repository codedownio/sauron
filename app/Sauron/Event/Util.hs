{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Sauron.Event.Util (
  withScroll
  , autoScrollSTM
  , clearAutoScrollTarget
  ) where

import Brick as B
import Brick.Widgets.List
import Data.List (findIndex)
import Data.String.Interpolate
import qualified Data.Text as T
import Lens.Micro
import Relude
import Sauron.Event.Helpers
import Sauron.Types
import Sauron.UI.Statuses (chooseWorkflowStatus)


withScroll :: AppState -> (forall s. ViewportScroll ClickableName -> EventM n s ()) -> EventM n AppState ()
withScroll s action = do
  case listSelectedElement (s ^. appMainList) of
    Just (_, _el@(SomeNode (getEntityData -> EntityData {..}))) -> action $ viewportScroll (InnerViewport [i|viewport_#{_ident}|])
    _ -> return ()

-- | Return an STM action that sets the auto-scroll target on a failed
-- JobLogGroupNode's state TVar. Intended to be composed with the toggle
-- in a single atomically block to avoid races with the fixer thread.
-- Returns the scroll target that was set (if any).
autoScrollSTM :: Node Variable a -> STM (Maybe ScrollTarget)
autoScrollSTM (JobLogGroupNode (EntityData {_static=jobLogGroup, _state=stateVar})) =
  case jobLogGroup of
    JobLogGroup {jobLogGroupStatus = Just status, jobLogGroupChildren = children'}
      | chooseWorkflowStatus status == WorkflowFailed -> do
          let target = case findFirstErrorLine children' of
                Just n  -> ScrollToLine (n + 5)
                Nothing -> ScrollToEnd
          writeTVar stateVar (Just target)
          return (Just target)
    _ -> return Nothing
autoScrollSTM _ = return Nothing

-- | Clear the auto-scroll target for the currently selected node (if it's a
-- JobLogGroupNode). Called when the user manually scrolls.
clearAutoScrollTarget :: MonadIO m => AppState -> m ()
clearAutoScrollTarget s =
  withFixedElemAndParents s $ \_fixedEl (SomeNode variableNode) _parents ->
    case variableNode of
      JobLogGroupNode (EntityData {_state=stateVar}) ->
        liftIO $ atomically $ writeTVar stateVar Nothing
      _ -> return ()

-- | Find the 0-based line index of the first ##[error] line in rendered job log content.
-- Line counting mirrors jobLogGroupInner rendering in UI/Job.hs.
findFirstErrorLine :: [JobLogGroup] -> Maybe Int
findFirstErrorLine = go 0
  where
    go _ [] = Nothing
    go n (JobLogLines {jobLogLinesLines = contents} : rest) =
      case findIndex ("##[error]" `T.isPrefixOf`) contents of
        Just idx -> Just (n + idx)
        Nothing -> go (n + length contents) rest
    go n (JobLogGroup {jobLogGroupChildren = children'} : rest) =
      case go (n + 1) children' of
        Just found -> Just found
        Nothing -> go (n + 1 + countLines children') rest

    -- | Count total rendered lines in a list of job log groups.
    countLines :: [JobLogGroup] -> Int
    countLines = sum . map countGroup
      where
        countGroup (JobLogLines {jobLogLinesLines = ls}) = length ls
        countGroup (JobLogGroup {jobLogGroupChildren = cs}) = 1 + countLines cs
