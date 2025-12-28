{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Sauron.Setup.ReposFromConfigFile (
  reposFromConfigFile
  ) where

import Control.Monad
import Control.Monad.Writer
import Data.Function
import Data.String.Interpolate
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import GitHub
import Relude hiding (Down)
import Sauron.HealthCheck
import Sauron.Options
import Sauron.Setup.Common (newRepoNode)
import Sauron.Types
import System.IO.Error (userError)
import UnliftIO.Exception


-- | Autodetect repos for user
reposFromConfigFile :: BaseContext -> PeriodSpec -> FilePath -> IO (V.Vector (SomeNode Variable))
reposFromConfigFile baseContext defaultHealthCheckPeriodUs configFile = do
  Yaml.decodeFileEither configFile >>= \case
    Left err -> throwIO $ userError [i|Failed to decode config file '#{configFile}': #{err}|]
    Right (config@(Config {..}) :: Config) -> do
      putStrLn [i|Got config: #{config}|]

      (V.fromList <$>) $ execWriterT $ forM (fromMaybe [] configSections) $ \(ConfigSection {..}) -> do
        (repoDepth, maybeHeadingNode) <- case sectionDisplayName of
          Nothing -> pure (0, Nothing)
          Just l -> do
            toggledVar <- newTVarIO True
            statusVar <- newTVarIO NotFetched
            searchVar <- newTVarIO SearchNone
            pageInfoVar <- newTVarIO emptyPageInfo
            identifier <- liftIO $ getIdentifier baseContext
            pure (1, Just (toggledVar, statusVar, searchVar, pageInfoVar, identifier, l))

        repoNodes <- forM sectionRepos $ \r -> do
          let nsName = case r of
                ConfigRepoSingle owner name _repoSettings -> (mkName (Proxy @Owner) owner, mkName (Proxy @Repo) name)
                ConfigRepoWildcard {} -> error "No"
          repoVar <- newTVarIO NotFetched
          healthCheckVar <- newTVarIO NotFetched
          hcThread <- case r of
            ConfigRepoSingle _ _ (HasSettings (RepoSettings {repoSettingsCheckPeriod=localPeriod})) -> do
              let ps@(PeriodSpec period) = fromMaybe defaultHealthCheckPeriodUs (localPeriod <|> join (repoSettingsCheckPeriod <$> configSettings))
              thread <- lift (newHealthCheckThread baseContext nsName repoVar healthCheckVar ps)
              pure (Just (thread, period))
            ConfigRepoSingle _ _ _ -> do
              let ps@(PeriodSpec period) = fromMaybe defaultHealthCheckPeriodUs (join (repoSettingsCheckPeriod <$> configSettings))
              thread <- lift (newHealthCheckThread baseContext nsName repoVar healthCheckVar ps)
              pure (Just (thread, period))
            ConfigRepoWildcard {} -> pure Nothing
          newRepoNode nsName repoVar healthCheckVar hcThread repoDepth (getIdentifier baseContext)

        case maybeHeadingNode of
          Nothing -> tell (fmap SomeNode repoNodes)
          Just (toggledVar, statusVar, searchVar, pageInfoVar, identifier, l) -> do
            childrenVar <- newTVarIO (fmap SomeNode repoNodes)
            headingHealthCheckVar <- newTVarIO NotFetched
            headingHealthCheckThreadVar <- newTVarIO Nothing
            let headingNode = HeadingNode $ EntityData {
                  _static = l
                  , _state = statusVar
                  , _urlSuffix = ""
                  , _toggled = toggledVar
                  , _children = childrenVar
                  , _search = searchVar
                  , _pageInfo = pageInfoVar
                  , _healthCheck = headingHealthCheckVar
                  , _healthCheckThread = headingHealthCheckThreadVar
                  , _depth = 0
                  , _ident = identifier
                  }
            tell [SomeNode headingNode]
