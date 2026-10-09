{-# LANGUAGE GADTs #-}

-- | Re-fetching what an open modal is showing, for its Refresh hotkey.
module Sauron.Event.Refresh (
  refreshModal
  ) where

import Brick
import Brick.BChan (writeBChan)
import Lens.Micro
import Relude
import Sauron.Actions (fetchOnOpen)
import Sauron.Actions.Util (findJobParent)
import Sauron.Event.PullModal (refetchPullModalTab)
import Sauron.Types
import UnliftIO.Async
import UnliftIO.Concurrent (threadDelay)


-- | Re-fetch whatever the open modal is showing. The pull request modal fetches each of
-- its tabs separately, so the selected tab is refreshed alongside the node itself.
refreshModal :: AppState -> EventM ClickableName AppState ()
refreshModal s = readTVarIO (_appModalVariable s) >>= \case
  Just (ZoomModalState {_zoomModalSomeNode=someNode@(SomeNode node), _zoomModalParents=parents}) -> do
    nodeFetch <- refreshZoomedNode bc node (someNode :| parents)
    spinUntilDone s [nodeFetch]
  Just (PullRequestModalState {_pullModalNode=node, _pullModalParents=parents, _pullModalTab=tab}) -> do
    nodeFetch <- fetchOnOpen bc node (SomeNode node :| parents)
    tabFetch <- refetchPullModalTab s tab
    spinUntilDone s [nodeFetch, tabFetch]
  _ -> return ()
  where
    bc = s ^. appBaseContext

-- | A log group has no data of its own -- what it shows comes from its job's logs -- so
-- refresh the job it belongs to.
refreshZoomedNode :: MonadIO m => BaseContext -> Node Variable a -> NonEmpty (SomeNode Variable) -> m (Async ())
refreshZoomedNode bc (JobLogGroupNode {}) parents = case findJobParent (toList parents) of
  Just job -> fetchOnOpen bc job parents
  Nothing -> liftIO $ async $ return ()
refreshZoomedNode bc node parents = fetchOnOpen bc node parents

-- | Spin the modal's title until the fetches are done. The spinner stays up for a
-- minimum time: a fetch that comes back almost immediately would otherwise move the
-- glyph for a single frame, which reads as a glitch rather than as activity.
spinUntilDone :: AppState -> [Async ()] -> EventM ClickableName AppState ()
spinUntilDone s fetches = do
  modify (appModalRefreshing .~ True)
  liftIO $ void $ async $ do
    concurrently_ (mapM_ (void . waitCatch) fetches) (threadDelay minimumSpinnerDisplayUs)
    writeBChan (eventChan (s ^. appBaseContext)) ModalRefreshFinished

minimumSpinnerDisplayUs :: Int
minimumSpinnerDisplayUs = 600_000
