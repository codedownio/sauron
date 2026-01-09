{-# LANGUAGE TypeFamilies #-}

module Sauron.Fetch.Core (
  fetchPaginatedWithState
  , pageSize

  , makeEmptyElemWithState
) where

import Control.Exception.Safe (bracketOnError_)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import qualified Data.List as L
import GitHub
import Network.HTTP.Client (responseBody)
import Network.HTTP.Types.URI (QueryItem, parseQuery)
import qualified Network.URI as NURI
import Relude
import Sauron.Actions.Util
import Sauron.Types


-- | Default page size for GitHub API requests
pageSize :: Int
pageSize = 10


-- | Create an empty element with a specific initial state
makeEmptyElemWithState :: BaseContext -> NodeStatic a -> NodeState a -> Text -> Int -> STM (EntityData Variable a)
makeEmptyElemWithState (BaseContext {getIdentifierSTM}) typ' initialState urlSuffix' depth' = do
  stateVar <- newTVar initialState
  ident' <- getIdentifierSTM
  toggledVar <- newTVar False
  childrenVar <- newTVar []
  healthCheckVar <- newTVar NotFetched
  healthCheckThreadVar <- newTVar Nothing
  return $ EntityData {
    _static = typ'
    , _state = stateVar

    , _urlSuffix = urlSuffix'

    , _toggled = toggledVar
    , _children = childrenVar

    , _healthCheck = healthCheckVar
    , _healthCheckThread = healthCheckThreadVar

    , _depth = depth'
    , _ident = ident'
}

-- | Updated fetchPaginated function that works with the new state structure (Search, PageInfo, Fetchable TotalCount)
fetchPaginatedWithState :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m, FromJSON res
  )
  => (FetchCount -> Request k res)
  -> TVar (Search, PageInfo, Fetchable a)
  -> (Either Text (res, PageInfo) -> STM ())
  -> m ()
fetchPaginatedWithState mkReq stateVar cb = do
  (_, PageInfo {pageInfoCurrentPage}, _) <- readTVarIO stateVar

  bracketOnError_ (atomically $ do
                    (s, p, f) <- readTVar stateVar
                    writeTVar stateVar (s, p, Fetching (fetchableCurrent f)))
                  (atomically $ do
                    (s, p, _) <- readTVar stateVar
                    writeTVar stateVar (s, p, Errored "Fetch failed with exception.")) $
    withGithubApiSemaphore (githubWithLoggingResponse (mkReq (FetchPage (PageParams (Just pageSize) (Just pageInfoCurrentPage))))) >>= \case
      Left err -> atomically $ do
        (s, p, _) <- readTVar stateVar
        writeTVar stateVar (s, p, Errored (show err))
        cb $ Left (show err)

      Right x -> do
        atomically $ do
          let PageLinks {..} = parsePageLinks x
          let parsePageFromUri :: NURI.URI -> Maybe Int
              parsePageFromUri uri = do
                let q = NURI.uriQuery uri
                let parsed :: [QueryItem] = parseQuery (encodeUtf8 q)
                result :: ByteString <- join $ L.lookup "page" parsed
                readMaybe (decodeUtf8 result)
          let pgInfo = PageInfo {
                pageInfoCurrentPage = pageInfoCurrentPage
                , pageInfoFirstPage = pageLinksFirst >>= parsePageFromUri
                , pageInfoPrevPage = pageLinksPrev >>= parsePageFromUri
                , pageInfoNextPage = pageLinksNext >>= parsePageFromUri
                , pageInfoLastPage = pageLinksLast >>= parsePageFromUri
                }

          (s, _, _) <- readTVar stateVar
          writeTVar stateVar (s, pgInfo, NotFetched) -- We'll update the fetchable in the callback
          cb $ Right (responseBody x, pgInfo)
