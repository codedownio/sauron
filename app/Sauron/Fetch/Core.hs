{-# LANGUAGE TypeFamilies #-}

module Sauron.Fetch.Core (
  fetchPaginated''
  , pageSize

  , makeEmptyElem
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

fetchPaginated'' :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m, FromJSON res
  )
  => (FetchCount -> Request k res)
  -> TVar PageInfo
  -> TVar (Fetchable a)
  -> (Either Text (res, PageInfo) -> STM ())
  -> m ()
fetchPaginated'' mkReq pageInfoVar stateVar cb = do
  PageInfo {pageInfoCurrentPage} <- readTVarIO pageInfoVar

  bracketOnError_ (atomically $ markFetching stateVar)
                  (atomically $ writeTVar stateVar (Errored "Fetch failed with exception.")) $
    withGithubApiSemaphore (githubWithLoggingResponse (mkReq (FetchPage (PageParams (Just pageSize) (Just pageInfoCurrentPage))))) >>= \case
      Left err -> atomically $ do
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

          cb $ Right (responseBody x, pgInfo)

makeEmptyElem :: BaseContext -> NodeStatic a -> Text -> Int -> STM (EntityData Variable a)
makeEmptyElem (BaseContext {getIdentifierSTM}) typ' urlSuffix' depth' = do
  stateVar <- newTVar NotFetched
  ident' <- getIdentifierSTM
  toggledVar <- newTVar False
  childrenVar <- newTVar []
  searchVar <- newTVar $ SearchNone
  pageInfoVar <- newTVar emptyPageInfo
  healthCheckVar <- newTVar NotFetched
  healthCheckThreadVar <- newTVar Nothing
  return $ EntityData {
    _static = typ'
    , _state = stateVar

    , _urlSuffix = urlSuffix'

    , _toggled = toggledVar
    , _children = childrenVar

    , _search = searchVar
    , _pageInfo = pageInfoVar

    , _healthCheck = healthCheckVar
    , _healthCheckThread = healthCheckThreadVar

    , _depth = depth'
    , _ident = ident'
}
