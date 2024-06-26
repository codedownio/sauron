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
reposFromConfigFile :: BaseContext -> PeriodSpec -> FilePath -> IO (V.Vector MainListElemVariable)
reposFromConfigFile baseContext defaultHealthCheckPeriodUs configFile = do
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
            identifier <- liftIO $ getIdentifier baseContext
            tell [MainListElemHeading {
              _label = l
              , _depth = 0
              , _toggled = toggledVar
              , _status = statusVar
              , _ident = identifier
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
          node <- newRepoNode nsName repoVar healthCheckVar hcThread repoDepth (getIdentifier baseContext)
          tell [node]
