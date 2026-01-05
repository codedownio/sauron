{-# LANGUAGE DataKinds #-}

module Sauron.Setup.AllReposForUser (
  allReposForUser
  ) where

import Control.Monad
import Data.Function
import GitHub
import Relude hiding (Down)
import Sauron.Options
import Sauron.Types


allReposForUser :: BaseContext -> PeriodSpec -> Name User -> IO (Node Variable PaginatedReposT)
allReposForUser baseContext _defaultHealthCheckPeriodUs userLogin = do
  let BaseContext {..} = baseContext
  reposStateVar <- newTVarIO (SearchText ("user:" <> untagName userLogin), emptyPageInfo, NotFetched)
  toggledVar <- newTVarIO True
  childrenVar <- newTVarIO mempty
  healthCheckVar <- newTVarIO NotFetched
  healthCheckThreadVar <- newTVarIO Nothing
  identifier <- getIdentifier

  return $ PaginatedReposNode $ EntityData {
    _static = userLogin
    , _state = reposStateVar
    , _urlSuffix = ""
    , _toggled = toggledVar
    , _children = childrenVar
    , _healthCheck = healthCheckVar
    , _healthCheckThread = healthCheckThreadVar
    , _depth = 0
    , _ident = identifier
    }
