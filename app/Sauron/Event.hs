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
import Sauron.Mutations.Notification (markNotificationAsDone)
import Sauron.Event.CommentModal
import Sauron.Event.Helpers
import Sauron.Event.Open (openNode)
import Sauron.Event.Paging
import Sauron.Event.Search
import Sauron.Event.Util
import Sauron.HealthCheck.Stop (stopHealthCheckThreadsForChildren)
import Sauron.Logging
import Sauron.Types
import Sauron.UI.Keys
import Sauron.UI.Modals.LogModal (autoScrollLogsToBottom)
import Sauron.UI.TopBox (isSearchable')
import UnliftIO.Async


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
  autoScrollLogsToBottom

-- Modal events
appEvent s@(_appModal -> Just modalState) e = case e of
  VtyEvent ev -> case modalState of
    CommentModalState {} -> case ev of
      (V.EvKey V.KEsc []) -> closeModal s
      (V.EvKey (V.KChar 'q') [V.MCtrl]) -> closeModal s
      (V.EvKey V.KEnter [V.MMeta]) -> do
        modify (appModal . _Just . submissionState .~ SubmittingComment)
        liftIO $ submitComment s modalState
      (V.EvKey V.KEnter [V.MMeta, V.MShift]) -> do
        modify (appModal . _Just . submissionState .~ SubmittingCloseWithComment)
        liftIO $ closeWithComment s modalState
      _ -> do
        handleModalScrolling CommentModalContent ev
        case ev of
          (V.EvKey (V.KChar 'v') _) -> return () -- Already handled by scrolling
          (V.EvKey (V.KChar 'n') _) -> return () -- Already handled by scrolling
          (V.EvKey (V.KChar 'p') _) -> return () -- Already handled by scrolling
          _ -> zoom (appModal . _Just . commentEditor) $ handleEditorEvent (VtyEvent ev)
    ZoomModalState {} -> case ev of
      (V.EvKey V.KEsc []) -> closeModal s
      (V.EvKey (V.KChar 'q') [V.MCtrl]) -> closeModal s
      _ -> handleModalScrolling ZoomModalContent ev
    (LogModalState _) -> case ev of
      (V.EvKey V.KEsc []) -> closeModal s
      (V.EvKey (V.KChar 'q') [V.MCtrl]) -> closeModal s
      (V.EvKey (V.KChar 'c') []) -> modify (appLogs .~ Seq.empty)
      (V.EvKey (V.KChar 's') []) -> modify (appShowStackTraces %~ not)
      _ -> do
        handleModalScrolling LogModalContent ev
        handleViewportScrolling LogModalContent ev
        handleLogLevelFiltering ev
  _ -> return ()

-- Form events
appEvent s@(_appForm -> Just (form, _formIdentifier)) e = case e of
  VtyEvent (V.EvKey V.KEsc []) -> modify (appForm .~ Nothing)
  VtyEvent (V.EvKey V.KEnter []) -> do
    withFixedElemAndParents s $ \_fixedEl (SomeNode el) parents -> do
      atomically $ updateSearchForNode el $ case formState form of
        "" -> SearchNone
        t -> SearchText t
      void $ refreshLine (s ^. appBaseContext) el parents
    modify (appForm .~ Nothing)
  _ -> zoom (appForm . _Just . _1) $ handleFormEvent e

appEvent s (VtyEvent e) = case e of
  -- Focus switching hotkeys (work regardless of current focus)
  V.EvKey V.KLeft [V.MCtrl] -> switchToMainPane s
  V.EvKey V.KRight [V.MCtrl] -> switchToLogPane s

  -- Handle events based on focused pane when split logs is enabled
  _ | _appSplitLogs s && _appFocusedPane s == LogPaneFocus -> handleLogPaneEvents s e
  _ -> handleMainPaneEvents s e

-- Mouse events for main pane
appEvent s (MouseDown (ListRow _i) V.BScrollUp _ _) = do
  switchToMainPane s
  vScrollBy (viewportScroll MainList) (-1)
appEvent s (MouseDown (ListRow _i) V.BScrollDown _ _) = do
  switchToMainPane s
  vScrollBy (viewportScroll MainList) 1
appEvent s (MouseDown (ListRow n) V.BLeft _ _) = do
  switchToMainPane s
  modify (appMainList %~ listMoveTo n)

-- Log pane viewport events
appEvent s (MouseDown LogSplitContent V.BScrollUp _ _) = do
  switchToLogPane s
  vScrollBy (viewportScroll LogSplitContent) (-1)
appEvent s (MouseDown LogSplitContent V.BScrollDown _ _) = do
  switchToLogPane s
  vScrollBy (viewportScroll LogSplitContent) 1
appEvent s (MouseDown LogSplitContent V.BLeft _ _) = switchToLogPane s

-- Pane focus switching
appEvent s (MouseDown MainPane V.BLeft _ _) = switchToMainPane s
appEvent s (MouseDown LogPane V.BLeft _ _) = switchToLogPane s

-- Catch-all
appEvent _ _ = return ()


handleMainPaneEvents :: AppState -> V.Event -> EventM ClickableName AppState ()
handleMainPaneEvents s e = case e of
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
      refreshSelected (s ^. appBaseContext) el parents
  V.EvKey c [] | c == refreshAllKey -> do
    liftIO $ runReaderT (refreshVisibleLines (s ^. appMainListVariable)) (s ^. appBaseContext)

  V.EvKey c [] | c == openSelectedKey -> do
    withFixedElemAndParents s $ \(SomeNode el) _variableEl elems -> do
      openNode (s ^. appBaseContext) elems el

  -- Column 3
  V.EvKey c [] | c == nextPageKey -> tryNavigatePage s goNextPage
  V.EvKey c [] | c == prevPageKey -> tryNavigatePage s goPrevPage
  V.EvKey c [] | c == firstPageKey -> tryNavigatePage s goFirstPage
  V.EvKey c [] | c == lastPageKey -> tryNavigatePage s goLastPage

  V.EvKey c [] | c == editSearchKey -> do
    withNthChildAndPaginationParent s $ \_fixedEl _el (sn@(SomeNode node@(getEntityData -> (EntityData {_ident}))), _, _) _parents -> do
      when (isSearchable' sn) $ do
        searchText <- liftIO $ atomically $ ensureNonEmptySearch node
        modify (appForm ?~ (newForm [editTextField id TextForm (Just 1)] searchText, _ident))

  V.EvKey c [] | c == commentKey -> do
    withFixedElemAndParents s $ \(SomeNode el) _variableEl parents -> do
      case (el, findRepoParent parents) of
        (SingleIssueNode (EntityData {_static=issue, _state}), Just (RepoNode (EntityData {_static=(owner, name)}))) -> do
          fetchCommentsAndOpenModal (s ^. appBaseContext) issue False owner name
        (SinglePullNode (EntityData {_static=issue, _state}), Just (RepoNode (EntityData {_static=(owner, name)}))) -> do
          fetchCommentsAndOpenModal (s ^. appBaseContext) issue True owner name
        _ -> return ()

  V.EvKey c [] | c == markNotificationDoneKey -> do
    withFixedElemAndParents s $ \(SomeNode el) _variableEl _parents -> do
      case el of
        SingleNotificationNode (EntityData {_static=notification}) -> do
          liftIO $ void $ async $ runReaderT (markNotificationAsDone notification) (s ^. appBaseContext)
          -- Refresh the notification to update its read status
          withFixedElemAndParents s $ \_fixedEl (SomeNode variableEl') parents' ->
            refreshSelected (s ^. appBaseContext) variableEl' parents'
        _ -> return ()

  V.EvKey c [] | c == zoomModalKey -> do
    withFixedElemAndParents s $ \(SomeNode _) (SomeNode variableEl) parents -> do
      -- TODO: we used to check if the state is NotFetched before doing this refresh
      refreshOnZoom (s ^. appBaseContext) variableEl parents
      liftIO $ atomically $ writeTVar (_appModalVariable s) (Just (ZoomModalState (SomeNode variableEl)))

  V.EvKey (V.KChar 'l') [V.MCtrl] -> do
    let modalState = LogModalState (list LogModalContent (Vec.fromList []) 1)
    modify (appModal ?~ modalState)
    -- Scroll to the bottom to show latest logs
    vScrollToEnd (viewportScroll LogModalContent)

  V.EvKey c [] | c `elem` [V.KEsc, exitKey] -> do
    -- Cancel everything and wait for cleanups
    -- liftIO $ mapM_ cancelNode (s ^. appRunTreeBase)
    -- forM_ (s ^. appRunTreeBase) (liftIO . waitForTree)
    halt

  ev -> zoom appMainList $ handleListEvent ev

handleLogPaneEvents :: AppState -> V.Event -> EventM ClickableName AppState ()
handleLogPaneEvents _s e = case e of
  V.EvKey (V.KChar 'c') [] -> modify (appLogs .~ Seq.empty)
  V.EvKey (V.KChar 's') [] -> modify (appShowStackTraces %~ not)
  V.EvKey c [] | c `elem` [V.KEsc, exitKey] -> halt
  _ -> do
    handleModalScrolling LogSplitContent e
    handleViewportScrolling LogSplitContent e
    handleLogLevelFilteringForSplit e LogSplitContent

handleLeftArrow :: AppState -> EventM ClickableName AppState ()
handleLeftArrow s = withFixedElemAndParents s $ \_ (SomeNode mle) parents -> do
  liftIO (readTVarIO (_toggled (getEntityData mle))) >>= \case
    True -> modifyToggled s (const False)
    False -> case toList parents of
      _:(SomeNode parent):_ -> do
        expandedList <- gets (^. appMainList)
        forM_ (Vec.findIndex (\(SomeNode el) -> _ident (getEntityData parent) == _ident (getEntityData el)) (listElements expandedList)) $ \index ->
          modify (appMainList %~ listMoveTo index)
      _ -> return ()

modifyToggled :: MonadIO m => AppState -> (Bool -> Bool) -> m ()
modifyToggled s cb = withFixedElemAndParents s $ \_fixedEl someNode@(SomeNode item@(getEntityData -> mle)) parents -> do
  wasOpen <- liftIO $ readTVarIO (_toggled mle)
  isOpen <- liftIO $ atomically $ do
    modifyTVar' (_toggled mle) cb
    readTVar (_toggled mle)

  -- Node opened
  when (not wasOpen && isOpen) $
    fetchOnOpenIfNecessary (_appBaseContext s) item (someNode :| toList parents)

  -- Node closed: stop healthcheck threads for children only
  when (wasOpen && not isOpen) $
    liftIO $ stopHealthCheckThreadsForChildren (_appBaseContext s) someNode

closeModal :: AppState -> EventM ClickableName AppState ()
closeModal s = do
  modify (appModal .~ Nothing)
  liftIO $ atomically $ writeTVar (_appModalVariable s) Nothing

handleModalScrolling :: ClickableName -> V.Event -> EventM ClickableName AppState ()
handleModalScrolling viewportName ev = case ev of
  (V.EvKey (V.KChar 'v') [V.MCtrl]) -> vScrollPage (viewportScroll viewportName) Down
  (V.EvKey (V.KChar 'v') [V.MMeta]) -> vScrollPage (viewportScroll viewportName) Up
  (V.EvKey (V.KChar 'n') [V.MCtrl]) -> vScrollBy (viewportScroll viewportName) 1
  (V.EvKey (V.KChar 'p') [V.MCtrl]) -> vScrollBy (viewportScroll viewportName) (-1)
  _ -> return ()

handleLogLevelFiltering :: V.Event -> EventM ClickableName AppState ()
handleLogLevelFiltering = flip handleLogLevelFilteringForSplit LogModalContent

handleLogLevelFilteringForSplit :: V.Event -> ClickableName -> EventM ClickableName AppState ()
handleLogLevelFilteringForSplit ev viewportName = case ev of
  (V.EvKey (V.KChar 'd') []) -> do
    modify (appLogLevelFilter .~ LevelDebug)
    vScrollToEnd (viewportScroll viewportName)
  (V.EvKey (V.KChar 'i') []) -> do
    modify (appLogLevelFilter .~ LevelInfo)
    vScrollToEnd (viewportScroll viewportName)
  (V.EvKey (V.KChar 'w') []) -> do
    modify (appLogLevelFilter .~ LevelWarn)
    vScrollToEnd (viewportScroll viewportName)
  (V.EvKey (V.KChar 'e') []) -> do
    modify (appLogLevelFilter .~ LevelError)
    vScrollToEnd (viewportScroll viewportName)
  _ -> return ()

handleViewportScrolling :: ClickableName -> V.Event -> EventM ClickableName AppState ()
handleViewportScrolling viewportName ev = case ev of
  (V.EvKey V.KUp []) -> vScrollBy (viewportScroll viewportName) (-1)
  (V.EvKey V.KDown []) -> vScrollBy (viewportScroll viewportName) 1
  (V.EvKey V.KPageUp []) -> vScrollPage (viewportScroll viewportName) Up
  (V.EvKey V.KPageDown []) -> vScrollPage (viewportScroll viewportName) Down
  (V.EvKey V.KHome []) -> vScrollToBeginning (viewportScroll viewportName)
  (V.EvKey V.KEnd []) -> vScrollToEnd (viewportScroll viewportName)
  _ -> return ()

switchToMainPane :: AppState -> EventM ClickableName AppState ()
switchToMainPane s = when (_appSplitLogs s) $ modify (appFocusedPane .~ MainPaneFocus)

switchToLogPane :: AppState -> EventM ClickableName AppState ()
switchToLogPane s = when (_appSplitLogs s) $ modify (appFocusedPane .~ LogPaneFocus)
