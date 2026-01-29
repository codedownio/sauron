{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Brick as B
import Brick.BChan
import Brick.Widgets.Border (vBorder, border)
import Brick.Widgets.Center
import Brick.Widgets.List
import qualified Brick.Widgets.List as L
import Control.Concurrent.QSem
import Control.Concurrent.STM (retry)
import Control.Monad
import Control.Monad.Logger (LogLevel(..))
import Data.Function
import Data.String.Interpolate
import qualified Data.Text.IO as T
import Data.Time
import qualified Data.Vector as V
import GitHub
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V
import Lens.Micro ((^.))
import Network.HTTP.Client (newManager)
import Relude hiding (Down)
import Sauron.Actions
import Sauron.Actions.Util (githubWithLogging', withGithubApiSemaphore')
import Sauron.Auth
import Sauron.Event
import Sauron.Expanding
import Sauron.Fetch.Core (makeEmptyElemWithState)
import Sauron.Fix
import Sauron.HealthCheck.Notification (newNotificationHealthCheckThread)
import Sauron.OAuth (authenticateWithGitHub, loadSavedToken)
import Sauron.Options
import Sauron.Setup.AllReposForUser
import Sauron.Setup.ReposFromConfigFile
import Sauron.Setup.ReposFromCurrentDirectory
import Sauron.Types
import Sauron.UI
import Sauron.UI.AttrMap (buildAdaptiveAttrMap)
import Sauron.UI.Border (borderWithCounts)
import Sauron.UI.BottomBar
import Sauron.UI.Modals.CommentModal (renderModal)
import Sauron.UI.Modals.LogModal (renderLogModal, renderLogPanel)
import Sauron.UI.Modals.NewIssueModal (renderNewIssueModal)
import Sauron.UI.Modals.ZoomModal (renderZoomModal)
import Sauron.UI.TimelineBorder (borderWithAttr)
import Sauron.UI.TopBox (topBox)
import System.IO.Error (userError)
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.Exception
import UnliftIO.IO hiding (hFlush)


refreshPeriod :: Int
refreshPeriod = 100000

defaultHealthCheckPeriodUs :: PeriodSpec
defaultHealthCheckPeriodUs = PeriodSpec (1_000_000 * 60 * 10)

mkApp :: V.ColorMode -> App AppState AppEvent ClickableName
mkApp colorMode = App {
  appDraw = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent = \event -> get >>= \s -> appEvent s event
  , appStartEvent = return ()
  , appAttrMap = const (buildAdaptiveAttrMap colorMode)
  }

drawUI :: AppState -> [Widget ClickableName]
drawUI app = case _appModal app of
  Nothing -> [ui]
  Just modalState -> case modalState of
    CommentModalState {} -> [renderModal app modalState, ui]
    NewIssueModalState {} -> [renderNewIssueModal modalState, ui]
    ZoomModalState {} -> [renderZoomModal app modalState, ui]
    LogModalState {} -> [renderLogModal app modalState, ui]
  where
    ui = if _appSplitLogs app then splitUI else mainUI

    mainUI = vBox [
      topBox app
      , borderWithCounts app
      -- , fixedHeightOrViewportPercent (InnerViewport [i|viewport_debugging|]) 50 $
      --     vBox [border $ strWrap (show item <> "\n") | item <- toList (app ^. appMainList)]
      , hCenter $ padAll 1 $ L.renderListWithIndex (listDrawElement app) True (app ^. appMainList)
      , clickable InfoBar $ bottomBar app
      ]

    splitUI = hBox [
      clickable MainPane $
        (if _appFocusedPane app == MainPaneFocus
         then borderWithAttr (attrName "focusedPaneBorder") Nothing
         else border) $
        hLimitPercent 50 mainUI,
      vBorder,
      clickable LogPane $
        (if _appFocusedPane app == LogPaneFocus
         then borderWithAttr (attrName "focusedPaneBorder") Nothing
         else border) $
        padRight Max $ renderLogPanel app LogSplitContent
      ]

main :: IO ()
main = do
  CliArgs {cliConfigFile, cliShowAllRepos, cliColorMode, cliSplitLogs} <- parseCliArgs

  eventChan <- newBChan 10
  baseContext'@(BaseContext {requestSemaphore}) <- buildBaseContext eventChan

  currentUser@(User {userLogin}) <- withGithubApiSemaphore' requestSemaphore (githubWithLogging' baseContext' userInfoCurrentR) >>= \case
    Left err -> throwIO $ userError [i|Failed to fetch currently authenticated user: #{err}|]
    Right x -> pure x

  let baseContext = baseContext' { currentUser = Just currentUser }

  listElems' :: V.Vector (SomeNode Variable) <- case cliShowAllRepos of
    True -> V.singleton . SomeNode <$> allReposForUser baseContext defaultHealthCheckPeriodUs userLogin
    False -> case cliConfigFile of
      Just configFile -> reposFromConfigFile baseContext defaultHealthCheckPeriodUs configFile
      Nothing -> isContainedInGitRepo >>= \case
        Just (namespace, name) -> fmap SomeNode <$> reposFromCurrentDirectory baseContext defaultHealthCheckPeriodUs (namespace, name)
        Nothing -> V.singleton . SomeNode <$> allReposForUser baseContext defaultHealthCheckPeriodUs userLogin

  -- Prepend a PaginatedNotificationsNode with health check
  notificationNode <- atomically (PaginatedNotificationsNode <$> makeEmptyElemWithState baseContext () (SearchNone, emptyPageInfo, NotFetched) "" 0)
  -- Start health check thread for notifications
  let notificationEntityData = getEntityData notificationNode
  let PeriodSpec period = defaultHealthCheckPeriodUs
  notificationsHealthCheckThread <- newNotificationHealthCheckThread baseContext (_state notificationEntityData)
  atomically $ writeTVar (_healthCheckThread notificationEntityData) (Just (notificationsHealthCheckThread, period))
  let listElems = V.cons (SomeNode notificationNode) listElems'

  -- Kick off initial fetches
  runReaderT (refreshVisibleLines listElems) baseContext

  listElemsFixed :: V.Vector (SomeNode Fixed) <- atomically $ mapM fixSomeNode listElems

  now <- getCurrentTime

  modalVariableTVar <- newTVarIO (Nothing :: Maybe (ModalState Variable))
  let modalFixed :: Maybe (ModalState Fixed) = Nothing

  let initialState =
        AppState {
          _appUser = currentUser
          , _appBaseContext = baseContext

          , _appModalVariable = modalVariableTVar
          , _appModal = modalFixed

          , _appForm = Nothing

          , _appMainListVariable = listElems
          , _appMainList = list MainList (getExpandedList listElemsFixed) 1

          , _appSortBy = SortByStars
          , _appNow = now

          , _appAnimationCounter = 0

          , _appCliColorMode = Nothing
          , _appActualColorMode = V.FullColor
          , _appSplitLogs = cliSplitLogs
          , _appFocusedPane = MainPaneFocus

          , _appLogs = mempty

          , _appLogLevelFilter = LevelInfo
          , _appShowStackTraces = False
        }


  -- Animation timer thread
  _ <- async $
    forever $ do
      threadDelay 200000  -- 200ms delay for animation
      writeBChan eventChan AnimationTick

  -- Time update thread - update every 30 seconds for accurate timestamps
  _ <- async $
    forever $ do
      threadDelay 30000000  -- 30 seconds
      getCurrentTime >>= writeBChan eventChan . TimeUpdated

  listElemsVar <- newTVarIO listElemsFixed
  listElemsFixerAsync <- async $
    forever $ do
      handleAny (\e -> putStrLn [i|Got exception in list elems fixer async: #{e}|] >> threadDelay refreshPeriod) $ do
        newFixed <- atomically $ do
          currentFixed <- readTVar listElemsVar
          newFixed <- mapM fixSomeNode listElems
          when (newFixed == currentFixed) retry

          writeTVar listElemsVar newFixed
          return newFixed
        writeBChan eventChan (ListUpdate (getExpandedList newFixed))
        threadDelay refreshPeriod

  modalFixedVar <- newTVarIO modalFixed
  modalFixerAsync <- async $
    forever $ do
      handleAny (\e -> putStrLn [i|Got exception in modal fixer async: #{e}|] >> threadDelay refreshPeriod) $ do
        newFixed <- atomically $ do
          currentFixed <- readTVar modalFixedVar
          currentVariable <- readTVar modalVariableTVar
          newFixed <- case currentVariable of
            Nothing -> return Nothing
            Just variableModal -> Just <$> fixModal variableModal
          when (newFixed == currentFixed) retry

          writeTVar modalFixedVar newFixed
          return newFixed
        writeBChan eventChan (ModalUpdate newFixed)
        threadDelay refreshPeriod

  let buildVty = do
        v <- V.userConfig >>= V.mkVty
        when (V.supportsMode (V.outputIface v) V.Mouse) $
          V.setMode (V.outputIface v) V.Mouse True
        return v
  initialVty <- buildVty
  flip onException (cancel listElemsFixerAsync >> cancel modalFixerAsync) $ do
    let st = initialState {
          _appCliColorMode = cliColorMode
          , _appActualColorMode = V.outputColorMode (V.outputIface initialVty)
          }
    void $ customMain initialVty buildVty (Just eventChan) (mkApp (fromMaybe (V.outputColorMode (V.outputIface initialVty)) cliColorMode)) st


buildBaseContext :: BChan AppEvent -> IO BaseContext
buildBaseContext eventChan = do
  CliArgs {..} <- parseCliArgs

  githubApiSemaphore <- newQSem cliConcurrentGithubApiLimit

  auth <- case cliForceAuth of
    True -> authenticateWithGitHub
    False -> do
      maybeAuth <- case cliOAuthToken of
        Just t -> pure $ Just $ OAuth (encodeUtf8 t)
        Nothing ->
          loadSavedToken >>= \case
            Just auth -> pure $ Just auth
            Nothing -> tryDiscoverAuth

      maybe authenticateWithGitHub pure maybeAuth

  putStrLn [i|Got auth: #{auth}|]

  debugFn <- case cliDebugFile of
    Nothing -> return $ const $ return ()
    Just fp -> do
      h <- openFile fp AppendMode
      return $ \t -> do
        T.hPutStrLn h t
        hFlush h

  manager <- newManager tlsManagerSettings

  idRef <- newTVarIO 0

  let getIdentifierSTM = do
        ret <- readTVar idRef
        writeTVar idRef (ret + 1)
        return ret

  let getIdentifier = liftIO $ atomically getIdentifierSTM

  return $ BaseContext {
    requestSemaphore = githubApiSemaphore
    , auth = auth
    , debugFn = debugFn
    , manager = manager
    , getIdentifier = getIdentifier
    , getIdentifierSTM = getIdentifierSTM
    , eventChan = eventChan
    , currentUser = Nothing
    }
