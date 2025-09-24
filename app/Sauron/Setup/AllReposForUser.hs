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
allReposForUser :: BaseContext -> PeriodSpec -> Name User -> IO (Node Variable PaginatedReposT)
allReposForUser baseContext defaultHealthCheckPeriodUs (N userLoginUnwrapped) = do
  let BaseContext {..} = baseContext
  -- repos <- github' $ organizationReposR "codedownio" RepoPublicityAll FetchAll
  -- putStrLn [i|repos: #{second (fmap repoName) repos}|]

  repos <- withGithubApiSemaphore' requestSemaphore (github auth (userReposR (N userLoginUnwrapped) RepoPublicityAll FetchAll)) >>= \case
    Left err -> throwIO $ userError [i|Failed to fetch repos for '#{userLoginUnwrapped}': #{err}|]
    Right x -> return x

  repoNodes <- (V.fromList <$>) $ forM (V.toList repos) $ \r -> do
    let nsName = (simpleOwnerLogin $ repoOwner r, repoName r)
    repoStateVar <- newTVarIO (Fetched r)
    healthCheckVar <- newTVarIO NotFetched
    hcThread <- newHealthCheckThread baseContext nsName repoStateVar healthCheckVar defaultHealthCheckPeriodUs
    newRepoNode nsName repoStateVar healthCheckVar (Just hcThread) 1 getIdentifier

  -- Create PaginatedReposNode
  reposStateVar <- newTVarIO (Fetched repos)
  toggledVar <- newTVarIO False
  childrenVar <- newTVarIO (V.toList repoNodes)
  searchVar <- newTVarIO SearchNone
  pageInfoVar <- newTVarIO emptyPageInfo
  healthCheckVar <- newTVarIO NotFetched
  identifier <- getIdentifier

  return $ PaginatedReposNode $ EntityData {
    _static = ()
    , _state = reposStateVar
    , _urlSuffix = ""
    , _toggled = toggledVar
    , _children = childrenVar
    , _search = searchVar
    , _pageInfo = pageInfoVar
    , _healthCheck = healthCheckVar
    , _healthCheckThread = Nothing
    , _depth = 0
    , _ident = identifier
    }
