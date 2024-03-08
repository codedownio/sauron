{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Sauron.Actions (
  openBrowserToUrl

  , withScroll

  , fetchRepo

  , fetchWorkflows

  , fetchIssues
  , fetchIssue

  , hasStartedInitialFetch
  , refresh
  , refreshAll

  , withGithubApiSemaphore
  , withGithubApiSemaphore'
  ) where

import Brick as B
import Brick.Widgets.List
import Control.Concurrent.QSem
import Control.Exception.Safe (bracket_, bracketOnError_)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.List as L
import Data.String.Interpolate
import qualified Data.Vector as V
import GitHub
import Lens.Micro
import Network.HTTP.Client (responseBody)
import Network.HTTP.Types.URI (QueryItem, parseQuery)
import qualified Network.URI as NURI
import Relude
import Sauron.Types
import UnliftIO.Async
import UnliftIO.Process


openBrowserToUrl :: MonadIO m => String -> m ()
openBrowserToUrl url =
  void $ readCreateProcessWithExitCode (proc "xdg-open" [url]) ""

withScroll :: AppState -> (forall s. ViewportScroll ClickableName -> EventM n s ()) -> EventM n AppState ()
withScroll s action = do
  case listSelectedElement (s ^. appMainList) of
    Just (_, MainListElemRepo {..}) -> do
      let scroll = viewportScroll (InnerViewport [i|viewport_#{_ident}|])
      action scroll
    _ -> return ()

fetchRepo :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> TVar (Fetchable Repo) -> m ()
fetchRepo owner name repoVar = do
  BaseContext {auth} <- ask
  bracketOnError_ (atomically $ writeTVar repoVar Fetching)
                  (atomically $ writeTVar repoVar (Errored "Repo fetch failed with exception.")) $
    withGithubApiSemaphore (liftIO $ github auth (repositoryR owner name)) >>= \case
      Left err -> atomically $ writeTVar repoVar (Errored (show err))
      Right x -> atomically $ writeTVar repoVar (Fetched x)

fetchIssues :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m, MonadFail m
  ) => Name Owner -> Name Repo -> TVar MainListElemVariable -> m ()
fetchIssues owner name childrenVar = do
  let search = mempty
  fetchPaginated (issuesForRepoR owner name search) PaginatedItemsIssues childrenVar

fetchWorkflows :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m, MonadFail m
  ) => Name Owner -> Name Repo -> TVar MainListElemVariable -> m ()
fetchWorkflows owner name childrenVar = do
  let search = mempty
  fetchPaginated (workflowRunsR owner name search) PaginatedItemsWorkflows childrenVar

fetchComments :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> IssueNumber -> TVar (Fetchable PaginatedItemInner) -> m ()
fetchComments owner name issueNumber inner = do
  BaseContext {auth, manager} <- ask
  bracketOnError_ (atomically $ writeTVar inner Fetching)
                  (atomically $ writeTVar inner (Errored "Comments fetch failed with exception.")) $
    withGithubApiSemaphore (liftIO $ executeRequestWithMgrAndRes manager auth (commentsR owner name issueNumber FetchAll)) >>= \case
      Left err -> atomically $ writeTVar inner (Errored (show err))
      Right v -> atomically $ writeTVar inner (Fetched (PaginatedItemInnerIssue (responseBody v)))

fetchPaginated :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m, MonadFail m, FromJSON res
  ) => (FetchCount -> Request k res)
    -> (res -> PaginatedItems)
    -> TVar MainListElemVariable
    -> m ()
fetchPaginated mkReq wrapResponse childrenVar = do
  BaseContext {auth, manager} <- ask

  MainListElemPaginated {..} <- readTVarIO childrenVar

  PageInfo {pageInfoCurrentPage} <- readTVarIO _pageInfo

  bracketOnError_ (atomically $ writeTVar _items Fetching)
                  (atomically $ writeTVar _items (Errored "Workflows fetch failed with exception.")) $
    withGithubApiSemaphore (liftIO $ executeRequestWithMgrAndRes manager auth (mkReq (FetchPage (PageParams (Just 10) (Just pageInfoCurrentPage))))) >>= \case
      Left err -> atomically $ do
        writeTVar _items (Errored (show err))
        writeTVar _children []

      Right x -> atomically $ do
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

        itemChildren <- forM (paginatedItemsToList $ wrapResponse (responseBody x)) $ \iss -> do
          itemVar <- newTVar (Fetched iss)
          itemInnerVar <- newTVar NotFetched
          toggledVar' <- newTVar False
          pure $ MainListElemItem {
            _item = itemVar
            , _itemInner = itemInnerVar
            , _toggled = toggledVar'
            , _depth = 3
            , _ident = 0
            }
        writeTVar _children itemChildren

fetchIssue :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> IssueNumber -> TVar (Fetchable Issue) -> m ()
fetchIssue owner name issueNumber issueVar = do
  BaseContext {auth, manager} <- ask
  bracketOnError_ (atomically $ writeTVar issueVar Fetching)
                  (atomically $ writeTVar issueVar (Errored "Workflows fetch failed with exception.")) $
    withGithubApiSemaphore (liftIO $ executeRequestWithMgrAndRes manager auth (issueR owner name issueNumber)) >>= \case
      Left err -> atomically $ do
        writeTVar issueVar (Errored (show err))
        undefined
      Right x -> atomically $ do
        writeTVar issueVar (Fetched (responseBody x))
        undefined

refresh :: (MonadIO m) => BaseContext -> MainListElemVariable -> MainListElemVariable -> m ()
refresh _ (MainListElemHeading {}) _ = return () -- TODO: refresh all repos
refresh bc (MainListElemRepo {_namespaceName=(owner, name), ..}) _ = liftIO $ do
  void $ async $ liftIO $ runReaderT (fetchWorkflows owner name _workflowsChild) bc
  void $ async $ liftIO $ runReaderT (fetchIssues owner name _issuesChild) bc
refresh bc (MainListElemPaginated {..}) (MainListElemRepo {_namespaceName=(owner, name), _issuesChild, _workflowsChild}) = liftIO $ case _typ of
  PaginatedIssues -> void $ async $ liftIO $ runReaderT (fetchIssues owner name _issuesChild) bc
  PaginatedWorkflows -> void $ async $ liftIO $ runReaderT (fetchWorkflows owner name _workflowsChild) bc
refresh bc (MainListElemItem {_item, ..}) (MainListElemRepo {_namespaceName=(owner, name)}) = readTVarIO _item >>= \case
  Fetched (PaginatedItemIssue (Issue {..})) -> liftIO $ void $ async $ liftIO $ runReaderT (fetchComments owner name issueNumber _itemInner) bc
  _ -> return () -- TODO
refresh _ _ _ = return ()

hasStartedInitialFetch :: (MonadIO m) => MainListElem -> m Bool
hasStartedInitialFetch (MainListElemHeading {}) = return True
hasStartedInitialFetch (MainListElemRepo {..}) = and <$> (mapM hasStartedInitialFetch [_issuesChild, _workflowsChild])
hasStartedInitialFetch (MainListElemPaginated {..}) = return $ isFetchingOrFetched _items
hasStartedInitialFetch (MainListElemItem {..}) = return (isFetchingOrFetched _item && isFetchingOrFetched _itemInner)

isFetchingOrFetched :: Fetchable a -> Bool
isFetchingOrFetched (Fetched {}) = True
isFetchingOrFetched (Fetching {}) = True
isFetchingOrFetched _ = False

refreshAll :: (
  MonadReader BaseContext m, MonadIO m
  ) => V.Vector MainListElemVariable -> m ()
refreshAll elems = do
  baseContext <- ask

  liftIO $ flip runReaderT baseContext $
    void $ async $ forConcurrently (V.toList elems) $ \case
      MainListElemHeading {} -> return ()
      MainListElemRepo {_namespaceName=(owner, name), ..} -> do
        fetchRepo owner name _repo
        -- TODO: clear issues, workflows, etc. and re-fetch for open repos?
      MainListElemPaginated {} -> return ()
      MainListElemItem {} -> return ()

withGithubApiSemaphore :: (MonadReader BaseContext m, MonadIO m, MonadMask m) => m a -> m a
withGithubApiSemaphore action = do
  sem <- asks requestSemaphore
  withGithubApiSemaphore' sem action

-- TODO: add timeout here?
withGithubApiSemaphore' :: (MonadIO m, MonadMask m) => QSem -> m a -> m a
withGithubApiSemaphore' sem = bracket_ (liftIO $ waitQSem sem) (liftIO $ signalQSem sem)
