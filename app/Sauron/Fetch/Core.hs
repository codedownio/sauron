{-# LANGUAGE TypeFamilies #-}

module Sauron.Fetch.Core (
  fetchPaginated'
  , fetchPaginated''
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

fetchPaginated' :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m, MonadFail m, FromJSON res
  )
  => (FetchCount -> Request k res)
  -> (res -> NodeState)
  -> (Int -> [NodeState] -> [Int] -> STM [MainListElemVariable])
  -> TVar MainListElemVariable
  -> m ()
fetchPaginated' mkReq wrapResponse makeChildren parentVar = do
  BaseContext {auth, getIdentifier, manager} <- ask

  MainListElemItem {..} <- readTVarIO parentVar

  PageInfo {pageInfoCurrentPage} <- readTVarIO _pageInfo

  bracketOnError_ (atomically $ writeTVar _state Fetching)
                  (atomically $ writeTVar _state (Errored "Fetch failed with exception.")) $
    withGithubApiSemaphore (liftIO $ executeRequestWithMgrAndRes manager auth (mkReq (FetchPage (PageParams (Just 10) (Just pageInfoCurrentPage))))) >>= \case
      Left err -> atomically $ do
        writeTVar _state (Errored (show err))
        writeTVar _children []

      Right x -> do
        let paginatedItems = paginatedItemsToList $ wrapResponse (responseBody x)
        identifiers <- replicateM (L.length paginatedItems) (liftIO getIdentifier)

        atomically $ do
          writeTVar _state (Fetched (wrapResponse (responseBody x)))

          let PageLinks {..} = parsePageLinks x
          let parsePageFromUri :: NURI.URI -> Maybe Int
              parsePageFromUri uri = do
                let q = NURI.uriQuery uri
                let parsed :: [QueryItem] = parseQuery (encodeUtf8 q)
                result :: ByteString <- join $ L.lookup "page" parsed
                readMaybe (decodeUtf8 result)
          writeTVar _pageInfo $ PageInfo {
            pageInfoCurrentPage = pageInfoCurrentPage
            , pageInfoFirstPage = pageLinksFirst >>= parsePageFromUri
            , pageInfoPrevPage = pageLinksPrev >>= parsePageFromUri
            , pageInfoNextPage = pageLinksNext >>= parsePageFromUri
            , pageInfoLastPage = pageLinksLast >>= parsePageFromUri
            }

          itemChildren <- makeChildren _depth paginatedItems identifiers
          writeTVar _children itemChildren

fetchPaginated'' :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m, FromJSON res
  )
  => (FetchCount -> Request k res)
  -> TVar PageInfo
  -> (Fetchable a -> STM ())
  -> (Either Text (res, PageInfo) -> STM ())
  -> m ()
fetchPaginated'' mkReq pageInfoVar writeFetchable cb = do
  BaseContext {auth, manager} <- ask

  PageInfo {pageInfoCurrentPage} <- readTVarIO pageInfoVar

  bracketOnError_ (atomically $ writeFetchable Fetching)
                  (atomically $ writeFetchable (Errored "Fetch failed with exception.")) $
    withGithubApiSemaphore (liftIO $ executeRequestWithMgrAndRes manager auth (mkReq (FetchPage (PageParams (Just 10) (Just pageInfoCurrentPage))))) >>= \case
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
