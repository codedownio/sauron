{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

module Sauron.Event (appEvent) where

import Brick as B
import Brick.Forms
import Brick.Widgets.List
import Control.Monad
import Control.Monad.IO.Unlift
import Data.Function
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

appEvent s@(_appForm -> Just (form, _formIdentifier)) e = case e of
  VtyEvent (V.EvKey V.KEsc []) -> modify (appForm .~ Nothing)
  VtyEvent (V.EvKey V.KEnter []) -> do
    withFixedElemAndParents s $ \_fixedEl parents -> case (viaNonEmpty head [x | x@(MainListElemPaginated {}) <- toList parents], viaNonEmpty head [x | x@(MainListElemRepo {}) <- toList parents]) of
      (Just el@(MainListElemPaginated {..}), Just repoEl) -> do
        atomically $ do
          writeTVar _search $ SearchText (formState form)
          writeTVar _pageInfo $ PageInfo 1 Nothing Nothing Nothing Nothing
        refresh (s ^. appBaseContext) el repoEl
      _ -> modify (appForm .~ Nothing)
    modify (appForm .~ Nothing)
  _ -> zoom (appForm . _Just . _1) $ handleFormEvent e

appEvent s (VtyEvent e) = case e of
  -- Column 1
  V.EvKey c [] | c == nextKey -> modify (appMainList %~ (listMoveBy 1))
  V.EvKey c [] | c == previousKey -> modify (appMainList %~ (listMoveBy (-1)))

  V.EvKey c [] | c `elem` toggleKeys -> modifyToggled s not
  V.EvKey V.KLeft [] -> modifyToggled s (const False)
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

  -- Column 2
  V.EvKey c [V.MMeta] | c == browserToHomeKey ->
    withRepoParent s $ \(Repo {repoHtmlUrl=(URL url)}) -> openBrowserToUrl (toString url)
  V.EvKey c [V.MMeta] | c == browserToIssuesKey ->
    withRepoParent s $ \(Repo {repoHtmlUrl=(URL url)}) -> openBrowserToUrl (toString url </> "issues")
  V.EvKey c [V.MMeta] | c == browserToPullsKey ->
    withRepoParent s $ \(Repo {repoHtmlUrl=(URL url)}) -> openBrowserToUrl (toString url </> "pulls")
  V.EvKey c [V.MMeta] | c == browserToActionsKey ->
    withRepoParent s $ \(Repo {repoHtmlUrl=(URL url)}) -> openBrowserToUrl (toString url </> "actions")

  V.EvKey c [] | c == refreshSelectedKey -> do
    withNthChildAndRepoParent s $ \_fixedEl el repoEl ->
      refresh (s ^. appBaseContext) el repoEl
  V.EvKey c [] | c == refreshAllKey -> do
    liftIO $ runReaderT (refreshAll (s ^. appMainListVariable)) (s ^. appBaseContext)

  V.EvKey c [] | c == openSelectedKey ->
    withNthChildAndRepoParent s $ \fixedEl _el repoEl -> case fixedEl of
      MainListElemRepo {_repo=(Fetched (Repo {repoHtmlUrl=(URL url)}))} -> openBrowserToUrl (toString url)
      MainListElemItem {_item=(Fetched x)} -> openBrowserToItem x
      MainListElemPaginated {_urlSuffix} -> case repoEl of
        MainListElemRepo {_repo} -> readTVarIO _repo >>= \case
          Fetched (Repo {repoHtmlUrl=(URL url)}) -> openBrowserToUrl (toString url </> toString _urlSuffix)
          _ -> return ()
        _ -> return ()
      _ -> return ()

  -- Column 3
  V.EvKey c [] | c == nextPageKey -> tryNavigatePage s goNextPage
  V.EvKey c [] | c == prevPageKey -> tryNavigatePage s goPrevPage
  V.EvKey c [] | c == firstPageKey -> tryNavigatePage s goFirstPage
  V.EvKey c [] | c == lastPageKey -> tryNavigatePage s goLastPage

  V.EvKey c [] | c == editSearchKey -> do
    withNthChildAndMaybePaginationParent s $ \_fixedEl _el paginationElem -> case paginationElem of
      Just (MainListElemPaginated {_ident, _search}) -> do
        search' <- readTVarIO _search
        modify (appForm .~ (Just (newForm [ editTextField id TextForm (Just 1) ] (case search' of SearchText t -> t; SearchNone -> ""), _ident)))
      _ -> return ()

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



modifyToggled :: MonadIO m => AppState -> (Bool -> Bool) -> m ()
modifyToggled s cb = withNthChildAndRepoParent s $ \fixedEl mle repoElem -> do
  isOpen <- liftIO $ atomically $ do
    modifyTVar' (_toggled mle) cb
    readTVar (_toggled mle)
  when isOpen $
    unlessM (hasStartedInitialFetch fixedEl) $
      refresh (s ^. appBaseContext) mle repoElem
  where
    hasStartedInitialFetch :: (MonadIO m) => MainListElem -> m Bool
    hasStartedInitialFetch (MainListElemHeading {}) = return True
    hasStartedInitialFetch (MainListElemRepo {..}) = and <$> (mapM hasStartedInitialFetch [_issuesChild, _workflowsChild])
    hasStartedInitialFetch (MainListElemPaginated {..}) = return $ isFetchingOrFetched _items
    hasStartedInitialFetch (MainListElemItem {..}) = return (isFetchingOrFetched _item && isFetchingOrFetched _itemInner)

    isFetchingOrFetched :: Fetchable a -> Bool
    isFetchingOrFetched (Fetched {}) = True
    isFetchingOrFetched (Fetching {}) = True
    isFetchingOrFetched _ = False
