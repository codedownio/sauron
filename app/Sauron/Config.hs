{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Sauron.Config (
  handleInitConfig
  , loadConfig
  , defaultConfigNodes
  , singleRepoConfigNodes
  , buildConfigNodes
  , defaultHealthCheckPeriodUs
  ) where

import Data.String.Interpolate
import Data.Text (splitOn)
import qualified Data.Yaml as Yaml
import GitHub
import Relude hiding (Down)
import Sauron.Fetch.Core (makeEmptyElemWithState)
import Sauron.HealthCheck.Notification (newNotificationHealthCheckThread)
import Sauron.HealthCheck.Repo (newRepoHealthCheckThread)
import Sauron.OAuth (getConfigDir)
import Sauron.Options
import Sauron.SampleConfig (sampleConfig)
import Sauron.Setup.Common (newRepoNode)
import Sauron.Types
import System.FilePath ((</>))
import System.IO (hPutStrLn)
import System.IO.Error (userError)
import UnliftIO.Directory (createDirectoryIfMissing, doesFileExist)
import UnliftIO.Exception


defaultHealthCheckPeriodUs :: PeriodSpec
defaultHealthCheckPeriodUs = PeriodSpec (1_000_000 * 60 * 10)

-- | Handle the init-config command
handleInitConfig :: Bool -> IO ()
handleInitConfig doWrite = do
  configDir <- getConfigDir
  let configPath = configDir </> "config.yaml"
  case doWrite of
    False -> do
      putStrLn sampleConfig
      hPutStrLn stderr [i|Save this to: #{configPath}|]
      hPutStrLn stderr "Or run: sauron init-config --write"
    True -> do
      exists <- doesFileExist configPath
      when exists $ do
        hPutStrLn stderr [i|Config file already exists at #{configPath}. Overwrite? [y/N] |]
        hFlush stderr
        answer <- getLine
        when (answer `notElem` ["y", "Y", "yes", "Yes"]) $ do
          hPutStrLn stderr "Aborted."
          exitFailure
      createDirectoryIfMissing True configDir
      writeFile configPath sampleConfig
      hPutStrLn stderr [i|Wrote config to #{configPath}|]

-- | Load and parse a config file
loadConfig :: FilePath -> IO Config
loadConfig configFile =
  Yaml.decodeFileEither configFile >>= \case
    Left err -> throwIO $ userError [i|Failed to decode config file '#{configFile}': #{err}|]
    Right config -> pure config

-- | Default top-level nodes when no config nodes are specified
defaultConfigNodes :: [ConfigNode]
defaultConfigNodes =
  [ ConfigNodeNotifications Nothing
  , ConfigNodeIssues "My Issues" "is:issue state:open archived:false assignee:@me sort:updated-desc" Nothing
  , ConfigNodeHeading "My Pulls"
      [ ConfigNodePulls "Needs your review" "is:pr is:open review-requested:@me sort:updated-desc" Nothing
      , ConfigNodePulls "Needs action" "is:pr is:open author:@me review:changes_requested sort:updated-desc" Nothing
      , ConfigNodePulls "Ready to merge" "is:pr is:open author:@me review:approved sort:updated-desc" Nothing
      ]
      (Just True)
  ]

-- | Top-level nodes for single-repo mode (just notifications)
singleRepoConfigNodes :: [ConfigNode]
singleRepoConfigNodes = [
  ConfigNodeNotifications Nothing
  ]

-- | Build Variable nodes from config node definitions
buildConfigNodes :: BaseContext -> PeriodSpec -> [ConfigNode] -> Int -> IO [SomeNode Variable]
buildConfigNodes baseContext defaultPeriod nodes depth' = do
  let setOpen node mOpen = do
        whenJust mOpen $ atomically . writeTVar (_toggled (getEntityData node))
        pure (SomeNode node)

  forM nodes $ \case
    ConfigNodeNotifications mOpen -> do
      node <- atomically (PaginatedNotificationsNode <$> makeEmptyElemWithState baseContext () (SearchNone, emptyPageInfo, NotFetched) "" depth')
      let ed = getEntityData node
      let PeriodSpec period = defaultPeriod
      thread <- newNotificationHealthCheckThread baseContext (_state ed)
      atomically $ writeTVar (_healthCheckThread ed) (Just (thread, period))
      setOpen node mOpen

    ConfigNodeIssues name searchQuery mOpen -> do
      node <- atomically (PaginatedIssuesNode <$> makeEmptyElemWithState baseContext name
        (SearchText searchQuery, emptyPageInfo, NotFetched) "" depth')
      setOpen node mOpen

    ConfigNodePulls name searchQuery mOpen -> do
      node <- atomically (PaginatedPullsNode <$> makeEmptyElemWithState baseContext name
        (SearchText searchQuery, emptyPageInfo, NotFetched) "" depth')
      setOpen node mOpen

    ConfigNodeHeading name childConfigs mOpen -> do
      childNodes <- buildConfigNodes baseContext defaultPeriod childConfigs (depth' + 1)
      heading <- atomically (HeadingNode <$> makeEmptyElemWithState baseContext name () "" depth')
      atomically $ writeTVar (_children (getEntityData heading)) childNodes
      setOpen heading mOpen

    ConfigNodeRepo repoFullName mSettings mOpen -> do
      case splitOn "/" repoFullName of
        [owner, repoName] -> do
          let nsName = (mkName (Proxy @Owner) owner, mkName (Proxy @Repo) repoName)
          repoVar <- newTVarIO NotFetched
          healthCheckVar <- newTVarIO NotFetched
          let ps@(PeriodSpec period) = fromMaybe defaultPeriod (mSettings >>= repoSettingsCheckPeriod)
          hcThread <- newRepoHealthCheckThread baseContext nsName repoVar healthCheckVar ps
          node <- newRepoNode nsName repoVar healthCheckVar (Just (hcThread, period)) depth' (getIdentifier baseContext)
          setOpen node mOpen
        _ -> error [i|Invalid repo name format (expected "owner/name"): #{repoFullName}|]
