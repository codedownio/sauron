{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Brick as B
import Brick.BChan
import Brick.Widgets.Center
import Brick.Widgets.List
import qualified Brick.Widgets.List as L
import Control.Concurrent.QSem
import Control.Concurrent.STM (retry)
import Control.Monad
import Data.Function
import Data.String.Interpolate
import qualified Data.Text.IO as T
import Data.Time
import qualified Data.Vector as V
import GitHub
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V
import Lens.Micro
import Network.HTTP.Client (newManager)
import Relude hiding (Down)
import Sauron.Actions
import Sauron.Actions.Util
import Sauron.Auth
import Sauron.Event
import Sauron.Expanding
import Sauron.Fetch (makeEmptyElem)
import Sauron.Fix
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
import Sauron.UI.CommentModal (renderModal)
import Sauron.UI.NodeModal (renderNodeModal)
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
  Nothing -> [mainUI]
  Just modalState -> case modalState of
    CommentModalState {} -> [renderModal app modalState, mainUI]
    NodeModalState {} -> [renderNodeModal app modalState, mainUI]
  where
    mainUI = vBox [
      topBox app
      , borderWithCounts app
      -- , fixedHeightOrViewportPercent (InnerViewport [i|viewport_debugging|]) 50 $
      --     vBox [border $ strWrap (show item <> "\n") | item <- toList (app ^. appMainList)]
      , hCenter $ padAll 1 $ L.renderListWithIndex (listDrawElement app) True (app ^. appMainList)
      , clickable InfoBar $ bottomBar app
      ]

main :: IO ()
main = do
  CliArgs {cliConfigFile, cliShowAllRepos, cliColorMode} <- parseCliArgs

  eventChan <- newBChan 10
  baseContext@(BaseContext {auth, requestSemaphore}) <- buildBaseContext eventChan

  currentUser@(User {userLogin}) <- withGithubApiSemaphore' requestSemaphore (github auth userInfoCurrentR) >>= \case
    Left err -> throwIO $ userError [i|Failed to fetch currently authenticated user: #{err}|]
    Right x -> pure x

  listElems' :: V.Vector (SomeNode Variable) <- case cliShowAllRepos of
    True -> V.singleton . SomeNode <$> allReposForUser baseContext defaultHealthCheckPeriodUs userLogin
    False -> case cliConfigFile of
      Just configFile -> reposFromConfigFile baseContext defaultHealthCheckPeriodUs configFile
      Nothing -> isContainedInGitRepo >>= \case
        Just (namespace, name) -> (fmap SomeNode) <$> reposFromCurrentDirectory baseContext defaultHealthCheckPeriodUs (namespace, name)
        Nothing -> V.singleton . SomeNode <$> allReposForUser baseContext defaultHealthCheckPeriodUs userLogin

  -- Prepend a PaginatedNotificationsNode
  listElems <- flip V.cons listElems' <$> atomically (SomeNode . PaginatedNotificationsNode <$> makeEmptyElem baseContext () "" 0)

  -- Kick off initial fetches
  runReaderT (refreshAll listElems) baseContext

  listElemsFixed :: V.Vector (SomeNode Fixed) <- atomically $ mapM fixSomeNode listElems

  now <- getCurrentTime

  let initialState =
        AppState {
          _appUser = currentUser
          , _appBaseContext = baseContext
          , _appMainListVariable = listElems
          , _appMainList = list MainList (getExpandedList listElemsFixed) 1

          , _appSortBy = SortByStars
          , _appNow = now

          , _appForm = Nothing
          , _appModal = Nothing
          , _appAnimationCounter = 0

          , _appColorMode = V.FullColor
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
  eventAsync <- async $
    forever $ do
      handleAny (\e -> putStrLn [i|Got exception in event async: #{e}|] >> threadDelay refreshPeriod) $ do
        newFixed <- atomically $ do
          currentFixed <- readTVar listElemsVar
          newFixed <- mapM fixSomeNode listElems
          when (newFixed == currentFixed) retry

          writeTVar listElemsVar newFixed
          return newFixed
        writeBChan eventChan (ListUpdate (getExpandedList newFixed))
        threadDelay refreshPeriod

  let buildVty = do
        v <- V.userConfig >>= V.mkVty
        when (V.supportsMode (V.outputIface v) V.Mouse) $
          V.setMode (V.outputIface v) V.Mouse True
        return v
  initialVty <- buildVty
  let colorMode = fromMaybe (V.outputColorMode (V.outputIface initialVty)) cliColorMode
  flip onException (cancel eventAsync) $
    void $ customMain initialVty buildVty (Just eventChan) (mkApp colorMode) (initialState { _appColorMode = colorMode })


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
    }
