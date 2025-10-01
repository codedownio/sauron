{-# LANGUAGE TypeFamilies #-}

module Sauron.Fetch.Core (
  fetchPaginated''
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

fetchPaginated'' :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m, FromJSON res
  )
  => (FetchCount -> Request k res)
  -> TVar PageInfo
  -> TVar (Fetchable a)
  -> (Either Text (res, PageInfo) -> STM ())
  -> m ()
fetchPaginated'' mkReq pageInfoVar stateVar cb = do
  BaseContext {auth, manager} <- ask

  PageInfo {pageInfoCurrentPage} <- readTVarIO pageInfoVar

  bracketOnError_ (atomically $ do
                      previous <- fetchableCurrent <$> readTVar stateVar
                      writeTVar stateVar (Fetching previous)
                  )
                  (atomically $ writeTVar stateVar (Errored "Fetch failed with exception.")) $
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
