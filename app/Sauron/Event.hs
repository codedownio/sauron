{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

module Sauron.Event (appEvent) where

import Brick as B
import Brick.Forms
import Brick.Widgets.Edit (handleEditorEvent)
import Brick.Widgets.List
import Control.Monad
import Control.Monad.IO.Unlift
import Data.Function
import qualified Data.Sequence as Seq
import qualified Data.Vector as Vec
import GitHub
import qualified Graphics.Vty as V
import Lens.Micro
import Relude hiding (Down, pi)
import Sauron.Actions
import Sauron.Actions.Util (findRepoParent)
import Sauron.Event.CommentModal
import Sauron.Event.Helpers
import Sauron.Event.Open (openNode)
import Sauron.Event.Paging
import Sauron.Types
import Sauron.UI.Keys
import Sauron.UI.TopBox (isSearchable')


appEvent :: AppState -> BrickEvent ClickableName AppEvent -> EventM ClickableName AppState ()
appEvent s (AppEvent (ListUpdate l')) = modify (appMainList %~ listReplace l' (listSelected $ s ^. appMainList))

appEvent _ (AppEvent (ModalUpdate newModal)) = modify (appModal .~ newModal)

appEvent _ (AppEvent AnimationTick) = modify (appAnimationCounter %~ (+1))

appEvent s (AppEvent (CommentModalEvent commentModalEvent)) = handleCommentModalEvent s commentModalEvent

appEvent _s (AppEvent (TimeUpdated newTime)) = do
  -- Update the current time for accurate timestamps
  modify (appNow .~ newTime)

appEvent _s (AppEvent (LogEntryAdded logEntry)) = do
  -- Add log entry to the logs sequence
  modify (appLogs %~ (Seq.|> logEntry))



-- Handle modal events
appEvent s@(_appModal -> Just modalState) e = case e of
  VtyEvent ev -> case modalState of
    CommentModalState {} -> case ev of
      (V.EvKey V.KEsc []) -> do
        modify (appModal .~ Nothing)
        liftIO $ atomically $ writeTVar (_appModalVariable s) Nothing
      (V.EvKey (V.KChar 'q') [V.MCtrl]) -> do
        modify (appModal .~ Nothing)
        liftIO $ atomically $ writeTVar (_appModalVariable s) Nothing
      (V.EvKey V.KEnter [V.MMeta]) -> do
        modify (appModal . _Just . submissionState .~ SubmittingComment)
        liftIO $ submitComment s modalState
      (V.EvKey V.KEnter [V.MMeta, V.MShift]) -> do
        modify (appModal . _Just . submissionState .~ SubmittingCloseWithComment)
        liftIO $ closeWithComment s modalState
      (V.EvKey (V.KChar 'v') [V.MCtrl]) -> vScrollPage (viewportScroll CommentModalContent) Down
      (V.EvKey (V.KChar 'v') [V.MMeta]) -> vScrollPage (viewportScroll CommentModalContent) Up
      (V.EvKey (V.KChar 'n') [V.MCtrl]) -> vScrollBy (viewportScroll CommentModalContent) 1
      (V.EvKey (V.KChar 'p') [V.MCtrl]) -> vScrollBy (viewportScroll CommentModalContent) (-1)
      _ -> zoom (appModal . _Just . commentEditor) $ handleEditorEvent (VtyEvent ev)
    ZoomModalState {} -> case ev of
      (V.EvKey V.KEsc []) -> do
        modify (appModal .~ Nothing)
        liftIO $ atomically $ writeTVar (_appModalVariable s) Nothing
      (V.EvKey (V.KChar 'q') [V.MCtrl]) -> do
        modify (appModal .~ Nothing)
        liftIO $ atomically $ writeTVar (_appModalVariable s) Nothing
      (V.EvKey (V.KChar 'v') [V.MCtrl]) -> vScrollPage (viewportScroll ZoomModalContent) Down
      (V.EvKey (V.KChar 'v') [V.MMeta]) -> vScrollPage (viewportScroll ZoomModalContent) Up
      (V.EvKey (V.KChar 'n') [V.MCtrl]) -> vScrollBy (viewportScroll ZoomModalContent) 1
      (V.EvKey (V.KChar 'p') [V.MCtrl]) -> vScrollBy (viewportScroll ZoomModalContent) (-1)
      _ -> return () -- No other interactions for ZoomModal
    LogModalState -> case ev of
      (V.EvKey V.KEsc []) -> do
        modify (appModal .~ Nothing)
        liftIO $ atomically $ writeTVar (_appModalVariable s) Nothing
      (V.EvKey (V.KChar 'q') [V.MCtrl]) -> do
        modify (appModal .~ Nothing)
        liftIO $ atomically $ writeTVar (_appModalVariable s) Nothing
      (V.EvKey (V.KChar 'v') [V.MCtrl]) -> vScrollPage (viewportScroll LogModalContent) Down
      (V.EvKey (V.KChar 'v') [V.MMeta]) -> vScrollPage (viewportScroll LogModalContent) Up
      (V.EvKey (V.KChar 'n') [V.MCtrl]) -> vScrollBy (viewportScroll LogModalContent) 1
      (V.EvKey (V.KChar 'p') [V.MCtrl]) -> vScrollBy (viewportScroll LogModalContent) (-1)
      _ -> return () -- No other interactions for LogModal
  _ -> return ()

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
    withRepoParent s $ \(Repo {repoHtmlUrl=(URL url)}) -> openBrowserToUrl (toString url <> "/issues")
  V.EvKey c [] | c == browserToPullsKey ->
    withRepoParent s $ \(Repo {repoHtmlUrl=(URL url)}) -> openBrowserToUrl (toString url <> "/pulls")
  V.EvKey c [] | c == browserToActionsKey ->
    withRepoParent s $ \(Repo {repoHtmlUrl=(URL url)}) -> openBrowserToUrl (toString url <> "/actions")

  V.EvKey c [] | c == refreshSelectedKey -> do
    withFixedElemAndParents s $ \_fixedEl (SomeNode el) parents ->
      refresh (s ^. appBaseContext) el parents
  V.EvKey c [] | c == refreshAllKey -> do
    liftIO $ runReaderT (refreshAll (s ^. appMainListVariable)) (s ^. appBaseContext)

  V.EvKey c [] | c == openSelectedKey -> do
    withFixedElemAndParents s $ \(SomeNode el) _variableEl elems -> do
      openNode (s ^. appBaseContext) elems el

  -- Column 3
  V.EvKey c [] | c == nextPageKey -> tryNavigatePage s goNextPage
  V.EvKey c [] | c == prevPageKey -> tryNavigatePage s goPrevPage
  V.EvKey c [] | c == firstPageKey -> tryNavigatePage s goFirstPage
  V.EvKey c [] | c == lastPageKey -> tryNavigatePage s goLastPage

  V.EvKey c [] | c == editSearchKey -> do
    withNthChildAndPaginationParent s $ \_fixedEl _el (sn@(SomeNode (getEntityData -> (EntityData {_ident, _search}))), _) _parents -> do
      when (isSearchable' sn) $ do
        search' <- readTVarIO _search
        modify (appForm .~ (Just (newForm [ editTextField id TextForm (Just 1) ] (case search' of SearchText t -> t; SearchNone -> ""), _ident)))

  V.EvKey c [] | c == commentKey -> do
    withFixedElemAndParents s $ \(SomeNode el) _variableEl parents -> do
      case (el, findRepoParent parents) of
        (SingleIssueNode (EntityData {_static=issue, _state}), Just (RepoNode (EntityData {_static=(owner, name)}))) -> do
          fetchCommentsAndOpenModal (s ^. appBaseContext) issue False owner name
        (SinglePullNode (EntityData {_static=issue, _state}), Just (RepoNode (EntityData {_static=(owner, name)}))) -> do
          fetchCommentsAndOpenModal (s ^. appBaseContext) issue True owner name
        _ -> return ()

  V.EvKey c [] | c == zoomModalKey -> do
    withFixedElemAndParents s $ \(SomeNode el) (SomeNode variableEl) parents -> do
      when (_state (getEntityData el) == NotFetched) $
        refresh (s ^. appBaseContext) variableEl parents
      liftIO $ atomically $ writeTVar (_appModalVariable s) (Just (ZoomModalState (SomeNode variableEl)))

  V.EvKey (V.KChar 'l') [V.MCtrl] -> do
    modify (appModal .~ Just LogModalState)
    liftIO $ atomically $ writeTVar (_appModalVariable s) (Just LogModalState)

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
  liftIO (readTVarIO (_toggled (getEntityData mle))) >>= \case
    True -> liftIO $ atomically $ writeTVar (_toggled (getEntityData mle)) False
    False -> case toList parents of
      _:(SomeNode parent):_ -> do
        expandedList <- gets (^. appMainList)
        forM_ (Vec.findIndex (\(SomeNode el) -> (_ident (getEntityData parent) == _ident (getEntityData el))) (listElements expandedList)) $ \index ->
          modify (appMainList %~ listMoveTo index)
      _ -> return ()

modifyToggled :: MonadIO m => AppState -> (Bool -> Bool) -> m ()
modifyToggled s cb = withFixedElemAndParents s $ \_fixedEl someNode@(SomeNode item@(getEntityData -> mle)) parents -> do
  isOpen <- liftIO $ atomically $ do
    modifyTVar' (_toggled mle) cb
    readTVar (_toggled mle)
  when isOpen $ do
    atomically (getExistentialChildrenWrapped item)
      >>= refreshVisibleNodes (_appBaseContext s) (someNode : (toList parents))
