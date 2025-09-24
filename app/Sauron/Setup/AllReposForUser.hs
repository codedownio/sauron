{-# LANGUAGE DataKinds #-}

module Sauron.Setup.AllReposForUser (
  allReposForUser
  ) where

import Control.Monad
import Data.Function
import Data.String.Interpolate
import qualified Data.Vector as V
import GitHub
import GitHub.Data.Name
import Relude hiding (Down)
import Sauron.Actions.Util (withGithubApiSemaphore')
import Sauron.HealthCheck
import Sauron.Options
import Sauron.Setup.Common (newRepoNode)
import Sauron.Types
import System.IO.Error (userError)
import UnliftIO.Exception


-- | Autodetect repos for user
allReposForUser :: BaseContext -> PeriodSpec -> Name User -> IO (V.Vector (Node Variable RepoT))
allReposForUser baseContext defaultHealthCheckPeriodUs (N userLoginUnwrapped) = do
  let BaseContext {..} = baseContext
  -- repos <- github' $ organizationReposR "codedownio" RepoPublicityAll FetchAll
  -- putStrLn [i|repos: #{second (fmap repoName) repos}|]

  repos <- withGithubApiSemaphore' requestSemaphore (github auth (userReposR (N userLoginUnwrapped) RepoPublicityAll FetchAll)) >>= \case
    Left err -> throwIO $ userError [i|Failed to fetch repos for '#{userLoginUnwrapped}': #{err}|]
    Right x -> return x

  (V.fromList <$>) $ forM (V.toList repos) $ \r -> do
    let nsName = (simpleOwnerLogin $ repoOwner r, repoName r)
    repoStateVar <- newTVarIO (Fetched r)
    healthCheckVar <- newTVarIO NotFetched
    hcThread <- newHealthCheckThread baseContext nsName repoStateVar healthCheckVar defaultHealthCheckPeriodUs
    newRepoNode nsName repoStateVar healthCheckVar (Just hcThread) 0 getIdentifier
