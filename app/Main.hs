{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Brick as B
import Brick.BChan
import Brick.Widgets.List
import Control.Concurrent.QSem
import Control.Concurrent.STM (retry)
import Control.Monad
import Control.Monad.Writer
import Data.Function
import Data.String.Interpolate
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import GitHub
import GitHub.Data.Name
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V
import Relude hiding (Down)
import Sauron.Actions
import Sauron.Auth
import Sauron.Event
import Sauron.Fix
import Sauron.Options
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Draw
import System.IO.Error (userError)
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.Exception


refreshPeriod :: Int
refreshPeriod = 100000


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
  args@(CliArgs {..}) <- parseCliArgs

  putStrLn [i|Got args: #{args}|]

  githubApiSemaphore <- newQSem cliConcurrentGithubApiLimit

  -------------------------------------------------------------

  maybeAuth <- case cliOAuthToken of
    Just t -> pure $ Just $ OAuth (encodeUtf8 t)
    Nothing -> tryDiscoverAuth

  auth <- case maybeAuth of
    Nothing -> throwIO $ userError [i|Couldn't figure out authentication.|]
    Just x -> pure x

  putStrLn [i|Got auth: #{auth}|]

  let baseContext = BaseContext {
        requestSemaphore = githubApiSemaphore
        , auth = auth
        }

  currentUser@(User {userLogin=(N userLoginUnwrapped)}) <- withGithubApiSemaphore' githubApiSemaphore (github auth userInfoCurrentR) >>= \case
    Left err -> throwIO $ userError [i|Failed to fetch currently authenticated user: #{err}|]
    Right x -> pure x

  putStrLn [i|currentUser: #{currentUser}|]

  -------------------------------------------------------------

  listElems :: V.Vector MainListElemVariable <- case cliConfigFile of
    Nothing -> do
      -- Autodetect repos for user

      -- repos <- github' $ organizationReposR "codedownio" RepoPublicityAll FetchAll
      -- putStrLn [i|repos: #{second (fmap repoName) repos}|]

      repos <- withGithubApiSemaphore' githubApiSemaphore (github auth (userReposR (N userLoginUnwrapped) RepoPublicityAll FetchAll)) >>= \case
        Left err -> throwIO $ userError [i|Failed to fetch repos for '#{userLoginUnwrapped}': #{err}|]
        Right x -> return x

      (V.fromList <$>) $ forM (V.toList repos) $ \r -> do
        repoVar <- newTVarIO (Fetched r)
        workflowsVar <- newTVarIO NotFetched
        toggledVar <- newTVarIO False
        return $ MainListElemRepo {
          _namespaceName = (simpleOwnerLogin $ repoOwner r, repoName r)
          , _repo = repoVar
          , _workflows = workflowsVar
          , _depth = 0
          , _toggled = toggledVar
          , _ident = 0
          }

    Just configFile -> do
      Yaml.decodeFileEither configFile >>= \case
        Left err -> throwIO $ userError [i|Failed to decode config file '#{configFile}': #{err}|]
        Right (config@(Config {..}) :: Config) -> do
          putStrLn [i|Got config: #{config}|]

          (V.fromList <$>) $ execWriterT $ forM (fromMaybe [] configSections) $ \(ConfigSection {..}) -> do
            repoDepth <- case sectionDisplayName of
              Nothing -> pure 0
              Just l -> do
                toggledVar <- newTVarIO True
                statusVar <- newTVarIO NotFetched
                tell [MainListElemHeading {
                  _label = l
                  , _depth = 0
                  , _toggled = toggledVar
                  , _status = statusVar
                  , _ident = 0
                  }]
                pure 1

            forM sectionRepos $ \r -> do
              repoVar <- newTVarIO NotFetched
              workflowsVar <- newTVarIO NotFetched
              toggledVar <- newTVarIO False
              tell [MainListElemRepo {
                _namespaceName = case r of
                    ConfigRepoSingle owner name -> (mkName (Proxy @Owner) owner, mkName (Proxy @Repo) name)
                    ConfigRepoWildcard {} -> error "No"

                , _repo = repoVar
                , _workflows = workflowsVar
                , _depth = repoDepth
                , _toggled = toggledVar
                , _ident = 0
                }]

  -------------------------------------------------------------

  -- Kick off fetches for repos, workflows
  runReaderT (refreshAll listElems) baseContext

  -------------------------------------------------------------

  listElemsFixed :: V.Vector MainListElem <- atomically $ mapM fixMainListElem listElems

  let initialState =
        AppState {
          _appUser = currentUser
          , _appBaseContext = baseContext
          , _appMainListVariable = listElems
          , _appMainList = list MainList listElemsFixed 1

          , _appSortBy = SortByStars
        }


  eventChan <- newBChan 10

  listElemsVar <- newTVarIO listElemsFixed

  eventAsync <- async $
    forever $ do
      handleAny (\e -> putStrLn [i|Got exception in event async: #{e}|] >> threadDelay refreshPeriod) $ do
        newFixed <- atomically $ do
          currentFixed <- readTVar listElemsVar
          newFixed <- mapM fixMainListElem listElems
          when (newFixed == currentFixed) retry

          writeTVar listElemsVar newFixed
          return newFixed
        writeBChan eventChan (ListUpdate newFixed)
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
