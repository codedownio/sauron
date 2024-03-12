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
import qualified Data.Text.IO as T
import Data.Time
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import GitHub
import GitHub.Data.Name
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V
import Network.HTTP.Client (newManager)
import Relude hiding (Down)
import Sauron.Actions
import Sauron.Auth
import Sauron.Event
import Sauron.Expanding
import Sauron.Fix
import Sauron.HealthCheck
import Sauron.Options
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

  debugFn <- case cliDebugFile of
    Nothing -> return $ const $ return ()
    Just fp -> do
      h <- openFile fp AppendMode
      return $ \t -> do
        T.hPutStrLn h t
        hFlush h

  manager <- newManager tlsManagerSettings

  let baseContext = BaseContext {
        requestSemaphore = githubApiSemaphore
        , auth = auth
        , debugFn = debugFn
        , manager = manager
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
        let nsName = (simpleOwnerLogin $ repoOwner r, repoName r)
        repoVar <- newTVarIO (Fetched r)
        healthCheckVar <- newTVarIO NotFetched
        hcThread <- newHealthCheckThread baseContext nsName repoVar healthCheckVar defaultHealthCheckPeriodUs
        newRepoNode nsName repoVar healthCheckVar (Just hcThread) 0

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
              let nsName = case r of
                    ConfigRepoSingle owner name _repoSettings -> (mkName (Proxy @Owner) owner, mkName (Proxy @Repo) name)
                    ConfigRepoWildcard {} -> error "No"
              repoVar <- newTVarIO NotFetched
              healthCheckVar <- newTVarIO NotFetched
              hcThread <- case r of
                ConfigRepoSingle _ _ (HasSettings (RepoSettings {repoSettingsCheckPeriod=localPeriod})) -> do
                  let period = fromMaybe defaultHealthCheckPeriodUs (localPeriod <|> join (repoSettingsCheckPeriod <$> configSettings))
                  Just <$> lift (newHealthCheckThread baseContext nsName repoVar healthCheckVar period)
                ConfigRepoSingle _ _ _ -> do
                  let period = fromMaybe defaultHealthCheckPeriodUs (join (repoSettingsCheckPeriod <$> configSettings))
                  Just <$> lift (newHealthCheckThread baseContext nsName repoVar healthCheckVar period)
                ConfigRepoWildcard {} -> pure Nothing
              node <- newRepoNode nsName repoVar healthCheckVar hcThread repoDepth
              tell [node]

  -------------------------------------------------------------

  -- Kick off fetches for repos, workflows
  runReaderT (refreshAll listElems) baseContext

  -------------------------------------------------------------

  listElemsFixed :: V.Vector MainListElem <- atomically $ mapM fixMainListElem listElems

  now <- getCurrentTime

  let initialState =
        AppState {
          _appUser = currentUser
          , _appBaseContext = baseContext
          , _appMainListVariable = listElems
          , _appMainList = list MainList listElemsFixed 1

          , _appSortBy = SortByStars
          , _appNow = now
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


newRepoNode ::
  (MonadIO m)
  => (Name Owner, Name Repo)
  -> TVar (Fetchable Repo)
  -> TVar (Fetchable HealthCheckResult)
  -> Maybe (Async ())
  -> Int
  -> m MainListElemVariable
newRepoNode nsName repoVar healthCheckVar hcThread repoDepth = do
  toggledVar <- newTVarIO False

  issuesVar <- newTVarIO NotFetched
  issuesToggledVar <- newTVarIO False
  issuesChildrenVar <- newTVarIO []
  issuesPageInfoVar <- newTVarIO $ PageInfo 1 Nothing Nothing Nothing Nothing
  issuesChildVar <- newTVarIO $ MainListElemPaginated {
    _typ = PaginatedIssues
    , _items = issuesVar
    , _label = "Issues"
    , _urlSuffix = "issues"
    , _toggled = issuesToggledVar
    , _children = issuesChildrenVar
    , _pageInfo = issuesPageInfoVar
    , _depth = 2
    , _ident = 0
    }

  pullsVar <- newTVarIO NotFetched
  pullsToggledVar <- newTVarIO False
  pullsChildrenVar <- newTVarIO []
  pullsPageInfoVar <- newTVarIO $ PageInfo 1 Nothing Nothing Nothing Nothing
  pullsChildVar <- newTVarIO $ MainListElemPaginated {
    _typ = PaginatedPulls
    , _items = pullsVar
    , _label = "Pulls"
    , _urlSuffix = "pulls"
    , _toggled = pullsToggledVar
    , _children = pullsChildrenVar
    , _pageInfo = pullsPageInfoVar
    , _depth = 2
    , _ident = 0
    }

  workflowsVar <- newTVarIO NotFetched
  workflowsToggledVar <- newTVarIO False
  workflowsChildrenVar <- newTVarIO []
  workflowsPageInfoVar <- newTVarIO $ PageInfo 1 Nothing Nothing Nothing Nothing
  workflowsChildVar <- newTVarIO $ MainListElemPaginated {
    _typ = PaginatedWorkflows
    , _items = workflowsVar
    , _label = "Workflows"
    , _urlSuffix = "actions"
    , _toggled = workflowsToggledVar
    , _children = workflowsChildrenVar
    , _pageInfo = workflowsPageInfoVar
    , _depth = 2
    , _ident = 0
    }

  return $ MainListElemRepo {
    _namespaceName = nsName
    , _repo = repoVar

    , _healthCheck = healthCheckVar
    , _healthCheckThread = hcThread

    , _toggled = toggledVar
    , _issuesChild = issuesChildVar
    , _pullsChild = pullsChildVar
    , _workflowsChild = workflowsChildVar

    , _depth = repoDepth
    , _ident = 0
    }
