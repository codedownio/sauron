{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

module Sauron.Event (appEvent) where

import Brick as B
import Brick.Forms
import Brick.Widgets.List
import Control.Monad
import Control.Monad.IO.Unlift
import Data.Function
import Data.String.Interpolate
import qualified Data.Vector as Vec
import GitHub
import qualified Graphics.Vty as V
import Lens.Micro
import Relude hiding (Down, pi)
import Sauron.Actions
import Sauron.Event.Helpers
import Sauron.Event.Paging
import Sauron.Types
import Sauron.UI.Keys
import System.FilePath


appEvent :: AppState -> BrickEvent ClickableName AppEvent -> EventM ClickableName AppState ()
appEvent s (AppEvent (ListUpdate l')) = modify (appMainList %~ listReplace l' (listSelected $ s ^. appMainList))

appEvent _ (AppEvent AnimationTick) = modify (appAnimationCounter %~ (+1))

appEvent s@(_appForm -> Just (form, _formIdentifier)) e = case e of
  VtyEvent (V.EvKey V.KEsc []) -> modify (appForm .~ Nothing)
  VtyEvent (V.EvKey V.KEnter []) -> do
    withFixedElemAndParents s $ \_fixedEl (SomeNode el@(getEntityData -> (EntityData {..}))) parents -> do
      atomically $ do
        writeTVar _search $ SearchText (formState form)
        writeTVar _pageInfo $ PageInfo 1 Nothing Nothing Nothing Nothing
      refresh (s ^. appBaseContext) el parents
    modify (appForm .~ Nothing)
  _ -> zoom (appForm . _Just . _1) $ handleFormEvent e

appEvent s (VtyEvent e) = case e of
  -- Column 1
  V.EvKey c [] | c == nextKey -> modify (appMainList %~ (listMoveBy 1))
  V.EvKey c [] | c == previousKey -> modify (appMainList %~ (listMoveBy (-1)))

  V.EvKey c [] | c `elem` toggleKeys -> modifyToggled s not
  V.EvKey V.KLeft [] -> handleLeftArrow s
  V.EvKey V.KRight [] -> modifyToggled s (const True)

  -- Scrolling in toggled items
  -- Wanted to make these uniformly Ctrl+whatever, but Ctrl+PageUp/PageDown was causing it to get KEsc and exit (?)
  V.EvKey V.KUp [V.MCtrl] -> withScroll s $ \vp -> vScrollBy vp (-1)
  V.EvKey (V.KChar 'p') [V.MCtrl] -> withScroll s $ \vp -> vScrollBy vp (-1)
  V.EvKey V.KDown [V.MCtrl] -> withScroll s $ \vp -> vScrollBy vp 1
  V.EvKey (V.KChar 'n') [V.MCtrl] -> withScroll s $ \vp -> vScrollBy vp 1
  V.EvKey (V.KChar 'v') [V.MMeta] -> withScroll s $ \vp -> vScrollPage vp Up
  V.EvKey (V.KChar 'v') [V.MCtrl] -> withScroll s $ \vp -> vScrollPage vp Down
  V.EvKey V.KHome [V.MCtrl] -> withScroll s $ \vp -> vScrollToBeginning vp
  V.EvKey V.KEnd [V.MCtrl] -> withScroll s $ \vp -> vScrollToEnd vp

  -- V.EvKey (V.KChar 'k') [V.MCtrl] -> vScrollPage (viewportScroll (InnerViewport "viewport_debugging")) Up
  -- V.EvKey (V.KChar 'l') [V.MCtrl] -> vScrollPage (viewportScroll (InnerViewport "viewport_debugging")) Down
  -- V.EvKey (V.KChar 'p') [V.MCtrl] -> vScrollBy (viewportScroll (InnerViewport "viewport_debugging")) 1
  -- V.EvKey (V.KChar 'o') [V.MCtrl] -> vScrollBy (viewportScroll (InnerViewport "viewport_debugging")) (-1)

  -- Column 2
  V.EvKey c [] | c == browserToHomeKey ->
    withRepoParent s $ \(Repo {repoHtmlUrl=(URL url)}) -> openBrowserToUrl (toString url)
  V.EvKey c [] | c == browserToIssuesKey ->
    withRepoParent s $ \(Repo {repoHtmlUrl=(URL url)}) -> openBrowserToUrl (toString url </> "issues")
  V.EvKey c [] | c == browserToPullsKey ->
    withRepoParent s $ \(Repo {repoHtmlUrl=(URL url)}) -> openBrowserToUrl (toString url </> "pulls")
  V.EvKey c [] | c == browserToActionsKey ->
    withRepoParent s $ \(Repo {repoHtmlUrl=(URL url)}) -> openBrowserToUrl (toString url </> "actions")

  V.EvKey c [] | c == refreshSelectedKey -> do
    withFixedElemAndParents s $ \_fixedEl (SomeNode el) parents ->
      refresh (s ^. appBaseContext) el parents
  V.EvKey c [] | c == refreshAllKey -> do
    liftIO $ runReaderT (refreshAll (s ^. appMainListVariable)) (s ^. appBaseContext)

  V.EvKey c [] | c == openSelectedKey ->
    withNthChildAndRepoParent s $ \fixedEl _el repoEl -> case (fixedEl, repoEl) of
      (SomeNode el, RepoNode (EntityData {_state})) -> readTVarIO _state >>= \case
        Fetched (Repo {repoHtmlUrl=(URL url)}) -> openBrowserToUrl (getNodeUrl (toString url) el)
        _ -> return ()

  -- Column 3
  V.EvKey c [] | c == nextPageKey -> tryNavigatePage s goNextPage
  V.EvKey c [] | c == prevPageKey -> tryNavigatePage s goPrevPage
  V.EvKey c [] | c == firstPageKey -> tryNavigatePage s goFirstPage
  V.EvKey c [] | c == lastPageKey -> tryNavigatePage s goLastPage

  V.EvKey c [] | c == editSearchKey -> do
    withNthChildAndPaginationParent s $ \_fixedEl _el (SomeNode (getEntityData -> (EntityData {_ident, _search})), _) _parents -> do
      search' <- readTVarIO _search
      modify (appForm .~ (Just (newForm [ editTextField id TextForm (Just 1) ] (case search' of SearchText t -> t; SearchNone -> ""), _ident)))

  V.EvKey c [] | c `elem` [V.KEsc, exitKey] -> do
    -- Cancel everything and wait for cleanups
    -- liftIO $ mapM_ cancelNode (s ^. appRunTreeBase)
    -- forM_ (s ^. appRunTreeBase) (liftIO . waitForTree)
    halt

  ev -> zoom appMainList $ handleListEvent ev

-- Mouse events
appEvent _s (MouseDown (ListRow _i) V.BScrollUp _ _) = do
  vScrollBy (viewportScroll MainList) (-1)
appEvent _s (MouseDown (ListRow _i) V.BScrollDown _ _) = do
  vScrollBy (viewportScroll MainList) 1
appEvent _s (MouseDown (ListRow n) V.BLeft _ _) = do
  modify (appMainList %~ (listMoveTo n))

appEvent _ _ = return ()

handleLeftArrow :: AppState -> EventM ClickableName AppState ()
handleLeftArrow s = withFixedElemAndParents s $ \_ (SomeNode mle) parents -> do
  liftIO (atomically $ readTVar (_toggled (getEntityData mle))) >>= \case
    True -> liftIO $ atomically $ writeTVar (_toggled (getEntityData mle)) False
    False -> case Relude.reverse (toList parents) of
      _:(SomeNode parent):_ -> do
        expandedList <- gets (^. appMainList)
        forM_ (Vec.findIndex (\(SomeNode el) -> (_ident (getEntityData parent) == _ident (getEntityData el))) (listElements expandedList)) $ \index ->
          modify (appMainList %~ listMoveTo index)
      _ -> return ()

modifyToggled :: MonadIO m => AppState -> (Bool -> Bool) -> m ()
modifyToggled s cb = withFixedElemAndParents s $ \fixedEl (SomeNode item@(getEntityData -> mle)) parents -> do
  isOpen <- liftIO $ atomically $ do
    modifyTVar' (_toggled mle) cb
    readTVar (_toggled mle)
  when isOpen $
    unlessM (hasStartedInitialFetch fixedEl) $
      refresh (s ^. appBaseContext) item parents
  where
    hasStartedInitialFetch :: (MonadIO m) => SomeNode Fixed -> m Bool
    hasStartedInitialFetch (SomeNode (RepoNode (EntityData {_children}))) = do
      and <$> mapM hasStartedInitialFetch _children
    hasStartedInitialFetch (SomeNode (HeadingNode {})) = return True
    hasStartedInitialFetch (SomeNode (getEntityData -> (EntityData {..}))) = return (isFetchingOrFetched _state)

    isFetchingOrFetched :: Fetchable a -> Bool
    isFetchingOrFetched (Fetched {}) = True
    isFetchingOrFetched (Fetching {}) = True
    isFetchingOrFetched _ = False


getNodeUrl :: String -> Node Fixed a -> String
getNodeUrl repoBaseUrl (PaginatedIssuesNode _) = repoBaseUrl <> "/issues"
getNodeUrl repoBaseUrl (PaginatedPullsNode _) = repoBaseUrl <> "/pulls"
getNodeUrl repoBaseUrl (PaginatedWorkflowsNode _) = repoBaseUrl <> "/actions"
getNodeUrl _repoBaseUrl (PaginatedReposNode _) = ""
getNodeUrl repoBaseUrl (PaginatedBranchesNode _) = repoBaseUrl <> "/branches"
getNodeUrl repoBaseUrl (SingleIssueNode (EntityData {_static=issue})) = case issueHtmlUrl issue of
  Just url -> toString $ getUrl url
  Nothing -> repoBaseUrl <> [i|/issues/#{issueNumber issue}|]
getNodeUrl repoBaseUrl (SinglePullNode (EntityData {_static=pull})) = case issueHtmlUrl pull of
  Just url -> toString $ getUrl url
  Nothing -> repoBaseUrl <> [i|/pulls/#{issueNumber pull}|]
getNodeUrl _repoBaseUrl (SingleWorkflowNode (EntityData {_static=workflowRun})) = toString $ getUrl $ workflowRunHtmlUrl workflowRun
getNodeUrl _repoBaseUrl (SingleJobNode (EntityData {_static=job})) = toString $ getUrl $ jobHtmlUrl job
getNodeUrl repoBaseUrl (SingleBranchNode (EntityData {_static=branch})) = repoBaseUrl <> "/tree/" <> toString (branchName branch)
getNodeUrl repoBaseUrl (SingleCommitNode (EntityData {_static=commit})) = repoBaseUrl <> "/commit/" <> toString (untagName (commitSha commit))
getNodeUrl _repoBaseUrl (JobLogGroupNode _) = "" -- Job log groups don't have URLs
getNodeUrl _repoBaseUrl (HeadingNode _) = "" -- Heading nodes don't have URLs
getNodeUrl repoBaseUrl (RepoNode _) = repoBaseUrl
