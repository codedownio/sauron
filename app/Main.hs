{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Brick as B
import Brick.BChan
import Brick.Widgets.List
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
import Network.HTTP.Client (newManager)
import Relude hiding (Down)
import Sauron.Actions
import Sauron.Actions.Util
import Sauron.Auth
import Sauron.Event
import Sauron.Expanding
import Sauron.Fetch (makeEmptyElem)
import Sauron.Fix
import Sauron.OAuth
import Sauron.Options
import Sauron.Setup.AllReposForUser
import Sauron.Setup.ReposFromConfigFile
import Sauron.Setup.ReposFromCurrentDirectory
import Sauron.Types
import Sauron.UI
import Sauron.UI.AttrMap
import System.IO.Error (userError)
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.Exception
import UnliftIO.IO hiding (hFlush)


refreshPeriod :: Int
refreshPeriod = 100000

defaultHealthCheckPeriodUs :: PeriodSpec
defaultHealthCheckPeriodUs = PeriodSpec (1_000_000 * 60 * 10)

app :: App AppState AppEvent ClickableName
app = App {
  appDraw = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent = \event -> get >>= \s -> appEvent s event
  , appStartEvent = return ()
  , appAttrMap = const mainAttrMap
  }


main :: IO ()
main = do
  CliArgs {cliConfigFile} <- parseCliArgs

  baseContext@(BaseContext {..}) <- buildBaseContext

  currentUser@(User {userLogin}) <- withGithubApiSemaphore' requestSemaphore (github auth userInfoCurrentR) >>= \case
    Left err -> throwIO $ userError [i|Failed to fetch currently authenticated user: #{err}|]
    Right x -> pure x

  listElems' :: V.Vector (SomeNode Variable) <- case cliConfigFile of
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
          , _appAnimationCounter = 0
        }


  eventChan <- newBChan 10

  -- Animation timer thread
  _ <- async $
    forever $ do
      threadDelay 200000  -- 200ms delay for animation
      writeBChan eventChan AnimationTick

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
        v <- V.mkVty V.defaultConfig
        let output = V.outputIface v
        when (V.supportsMode output V.Mouse) $
          V.setMode output V.Mouse True
        return v
  initialVty <- buildVty
  flip onException (cancel eventAsync) $
    void $ customMain initialVty buildVty (Just eventChan) app initialState


buildBaseContext :: IO BaseContext
buildBaseContext = do
  args@(CliArgs {..}) <- parseCliArgs

  putStrLn [i|Got args: #{args}|]

  githubApiSemaphore <- newQSem cliConcurrentGithubApiLimit

  auth <- case cliForceAuth of
    True -> authenticateWithGitHub
    False -> do
      maybeAuth <- case cliOAuthToken of
        Just t -> pure $ Just $ OAuth (encodeUtf8 t)
        Nothing -> tryDiscoverAuth

      case maybeAuth of
        Nothing -> authenticateWithGitHub
        Just x -> pure x

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
    }
