{-# LANGUAGE TypeFamilies #-}

module Sauron.Actions.Fetch (
  fetchPaginated'
  , fetchPaginated
  , fetchPaginatedPaginated
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
  -> (res -> PaginatedItems)
  -> (Int -> [PaginatedItem] -> [Int] -> STM [MainListElemVariable])
  -> TVar MainListElemVariable
  -> m ()
fetchPaginated' mkReq wrapResponse makeChildren parentVar = do
  BaseContext {auth, getIdentifier, manager} <- ask

  MainListElemPaginated {..} <- readTVarIO parentVar

  PageInfo {pageInfoCurrentPage} <- readTVarIO _pageInfo

  bracketOnError_ (atomically $ writeTVar _items Fetching)
                  (atomically $ writeTVar _items (Errored "Workflows fetch failed with exception.")) $
    withGithubApiSemaphore (liftIO $ executeRequestWithMgrAndRes manager auth (mkReq (FetchPage (PageParams (Just 10) (Just pageInfoCurrentPage))))) >>= \case
      Left err -> atomically $ do
        writeTVar _items (Errored (show err))
        writeTVar _children []

      Right x -> do
        let paginatedItems = paginatedItemsToList $ wrapResponse (responseBody x)
        identifiers <- replicateM (L.length paginatedItems) (liftIO getIdentifier)

        atomically $ do
          writeTVar _items (Fetched (wrapResponse (responseBody x)))

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

fetchPaginated :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m, MonadFail m, FromJSON res
  ) => (FetchCount -> Request k res)
    -> (res -> PaginatedItems)
    -> TVar MainListElemVariable
    -> m ()
fetchPaginated mkReq wrapResponse = fetchPaginated' mkReq wrapResponse makeItemChildren
  where
    makeItemChildren parentDepth paginatedItems identifiers = do
      forM (zip paginatedItems identifiers) $ \(iss, identifier) -> do
        itemVar <- newTVar (Fetched iss)
        itemInnerVar <- newTVar NotFetched
        toggledVar' <- newTVar False
        pure $ MainListElemItem {
          _item = itemVar
          , _itemInner = itemInnerVar
          , _toggled = toggledVar'
          , _depth = parentDepth + 1
          , _ident = identifier
          }

fetchPaginatedPaginated :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m, MonadFail m, FromJSON res
  )
  => PaginatedType
  -> Text
  -> Text
  -> (FetchCount -> Request k res)
  -> (res -> PaginatedItems)
  -> TVar MainListElemVariable
  -> m ()
fetchPaginatedPaginated childType childLabel childUrlSuffix mkReq wrapResponse =
  fetchPaginated' mkReq wrapResponse (makePaginatedChildren childType childLabel childUrlSuffix)
  where
    makePaginatedChildren childType' childLabel' childUrlSuffix' parentDepth paginatedItems identifiers = do
      forM (zip paginatedItems identifiers) $ \(paginatedItem, identifier) -> do
        itemVar <- newTVar (Fetched paginatedItem)
        toggledVar <- newTVar False

        -- For workflows, create a child paginated section that will hold jobs
        case paginatedItem of
          PaginatedItemWorkflow _ -> do
            let childIdentifier = identifier * 1000 + 1 -- Simple child ID generation
            childItemsVar <- newTVar NotFetched
            childToggledVar <- newTVar False
            childChildrenVar <- newTVar []
            childSearchVar <- newTVar SearchNone
            childPageInfoVar <- newTVar $ PageInfo 1 Nothing Nothing Nothing Nothing

            let childPaginated = MainListElemPaginated {
                  _typ = childType'
                  , _items = childItemsVar
                  , _label = childLabel'
                  , _urlSuffix = childUrlSuffix'
                  , _toggled = childToggledVar
                  , _children = childChildrenVar
                  , _search = childSearchVar
                  , _pageInfo = childPageInfoVar
                  , _depth = parentDepth + 2
                  , _ident = childIdentifier
                  }

            itemInnerVar <- newTVar (Fetched (PaginatedItemInnerPaginated childPaginated))

            pure $ MainListElemItem {
              _item = itemVar
              , _itemInner = itemInnerVar
              , _toggled = toggledVar
              , _depth = parentDepth + 1
              , _ident = identifier
              }
          _ -> do
            itemInnerVar <- newTVar NotFetched
            pure $ MainListElemItem {
              _item = itemVar
              , _itemInner = itemInnerVar
              , _toggled = toggledVar
              , _depth = parentDepth + 1
              , _ident = identifier
              }