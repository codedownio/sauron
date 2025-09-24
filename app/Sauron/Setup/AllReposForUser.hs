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
  reposStateVar <- newTVarIO NotFetched
  toggledVar <- newTVarIO False
  childrenVar <- newTVarIO mempty
  searchVar <- newTVarIO SearchNone
  pageInfoVar <- newTVarIO emptyPageInfo
  healthCheckVar <- newTVarIO NotFetched
  identifier <- getIdentifier

  return $ PaginatedReposNode $ EntityData {
    _static = userLogin
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
