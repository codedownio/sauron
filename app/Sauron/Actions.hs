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
import qualified Data.Text as T
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
    Just (_, el) -> action $ viewportScroll (InnerViewport [i|viewport_#{_ident el}|])
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
  extraTerms <- readTVarIO childrenVar >>= (readTVarIO . _search) >>= \case
    SearchNone -> pure []
    SearchText t -> pure $ T.words t

  let fullQuery = T.intercalate "+" ([i|repo:#{untagName owner}/#{untagName name}|] : extraTerms)

  fetchPaginated (searchIssuesR fullQuery) PaginatedItemsIssues childrenVar

fetchPulls :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m, MonadFail m
  ) => Name Owner -> Name Repo -> TVar MainListElemVariable -> m ()
fetchPulls owner name childrenVar = do
  extraTerms <- readTVarIO childrenVar >>= (readTVarIO . _search) >>= \case
    SearchNone -> pure []
    SearchText t -> pure $ T.words t

  let fullQuery = T.intercalate "+" ([i|repo:#{untagName owner}/#{untagName name}|] : extraTerms)

  fetchPaginated (searchIssuesR fullQuery) PaginatedItemsPulls childrenVar

fetchWorkflows :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m, MonadFail m
  ) => Name Owner -> Name Repo -> TVar MainListElemVariable -> m ()
fetchWorkflows owner name childrenVar = do
  fetchPaginated (workflowRunsR owner name mempty) PaginatedItemsWorkflows childrenVar

fetchIssueComments :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> IssueNumber -> TVar (Fetchable PaginatedItemInner) -> m ()
fetchIssueComments owner name issueNumber inner = do
  BaseContext {auth, manager} <- ask
  bracketOnError_ (atomically $ writeTVar inner Fetching)
                  (atomically $ writeTVar inner (Errored "Issue comments fetch failed with exception.")) $
    withGithubApiSemaphore (liftIO $ executeRequestWithMgrAndRes manager auth (commentsR owner name issueNumber FetchAll)) >>= \case
      Left err -> atomically $ writeTVar inner (Errored (show err))
      Right v -> atomically $ writeTVar inner (Fetched (PaginatedItemInnerIssue (responseBody v)))

fetchPullComments :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> IssueNumber -> TVar (Fetchable PaginatedItemInner) -> m ()
fetchPullComments owner name issueNumber inner = do
  BaseContext {auth, manager} <- ask
  bracketOnError_ (atomically $ writeTVar inner Fetching)
                  (atomically $ writeTVar inner (Errored "Pull comments fetch failed with exception.")) $
    -- pullRequestCommentsR returns comments on the "unified diff"
    -- there are also "commit comments" and "issue comments".
    -- The last one are the most common on PRs, so we use commentsR
    withGithubApiSemaphore (liftIO $ executeRequestWithMgrAndRes manager auth (commentsR owner name issueNumber FetchAll)) >>= \case
      Left err -> atomically $ writeTVar inner (Errored (show err))
      Right v -> atomically $ writeTVar inner (Fetched (PaginatedItemInnerPull (responseBody v)))

fetchPaginated :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m, MonadFail m, FromJSON res
  ) => (FetchCount -> Request k res)
    -> (res -> PaginatedItems)
    -> TVar MainListElemVariable
    -> m ()
fetchPaginated mkReq wrapResponse childrenVar = do
  BaseContext {auth, getIdentifier, manager} <- ask

  MainListElemPaginated {..} <- readTVarIO childrenVar

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

          itemChildren <- forM (zip paginatedItems identifiers) $ \(iss, identifier) -> do
            itemVar <- newTVar (Fetched iss)
            itemInnerVar <- newTVar NotFetched
            toggledVar' <- newTVar False
            pure $ MainListElemItem {
              _item = itemVar
              , _itemInner = itemInnerVar
              , _toggled = toggledVar'
              , _depth = 3
              , _ident = identifier
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
      Right x -> atomically $ do
        writeTVar issueVar (Fetched (responseBody x))

refresh :: (MonadIO m) => BaseContext -> MainListElemVariable -> MainListElemVariable -> m ()
refresh _ (MainListElemHeading {}) _ = return () -- TODO: refresh all repos
refresh bc (MainListElemRepo {_namespaceName=(owner, name), ..}) _ = liftIO $ do
  void $ async $ liftIO $ runReaderT (fetchIssues owner name _issuesChild) bc
  void $ async $ liftIO $ runReaderT (fetchPulls owner name _pullsChild) bc
  void $ async $ liftIO $ runReaderT (fetchWorkflows owner name _workflowsChild) bc
refresh bc (MainListElemPaginated {..}) (MainListElemRepo {_namespaceName=(owner, name), _issuesChild, _pullsChild, _workflowsChild}) = liftIO $ case _typ of
  PaginatedIssues -> void $ async $ liftIO $ runReaderT (fetchIssues owner name _issuesChild) bc
  PaginatedPulls -> void $ async $ liftIO $ runReaderT (fetchPulls owner name _pullsChild) bc
  PaginatedWorkflows -> void $ async $ liftIO $ runReaderT (fetchWorkflows owner name _workflowsChild) bc
refresh bc (MainListElemItem {_item, ..}) (MainListElemRepo {_namespaceName=(owner, name)}) = readTVarIO _item >>= \case
  Fetched (PaginatedItemIssue (Issue {..})) -> liftIO $ void $ async $ liftIO $ runReaderT (fetchIssueComments owner name issueNumber _itemInner) bc
  Fetched (PaginatedItemPull (Issue {..})) -> liftIO $ void $ async $ liftIO $ runReaderT (fetchPullComments owner name issueNumber _itemInner) bc
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
