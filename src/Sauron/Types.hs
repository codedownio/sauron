{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Sauron.Types where

import Brick.Forms
import qualified Brick.Widgets.List as L
import Control.Concurrent.QSem
import Data.String.Interpolate
import Data.Text ()
import Data.Time
import Data.Typeable
import qualified Data.Vector as V
import GitHub hiding (Status)
import Lens.Micro
import Lens.Micro.TH
import Network.HTTP.Client (Manager)
import Relude
import qualified Text.Show
import UnliftIO.Async


-- * Main list elem

data Node f (a :: NodeTyp) where
  HeadingNode :: EntityData f 'HeadingT -> Node f 'HeadingT
  RepoNode :: EntityData f 'RepoT -> Node f 'RepoT

  PaginatedIssuesNode :: EntityData f 'PaginatedIssuesT -> Node f 'PaginatedIssuesT
  PaginatedPullsNode :: EntityData f 'PaginatedPullsT -> Node f 'PaginatedPullsT
  PaginatedWorkflowsNode :: EntityData f 'PaginatedWorkflowsT -> Node f 'PaginatedWorkflowsT
  PaginatedReposNode :: EntityData f 'PaginatedReposT -> Node f 'PaginatedReposT
  PaginatedBranchesNode :: EntityData f 'PaginatedBranchesT -> Node f 'PaginatedBranchesT
  PaginatedNotificationsNode :: EntityData f 'PaginatedNotificationsT -> Node f 'PaginatedNotificationsT

  SingleIssueNode :: EntityData f 'SingleIssueT -> Node f 'SingleIssueT
  SinglePullNode :: EntityData f 'SinglePullT -> Node f 'SinglePullT
  SingleWorkflowNode :: EntityData f 'SingleWorkflowT -> Node f 'SingleWorkflowT
  SingleJobNode :: EntityData f 'SingleJobT -> Node f 'SingleJobT
  SingleBranchNode :: EntityData f 'SingleBranchT -> Node f 'SingleBranchT
  SingleCommitNode :: EntityData f 'SingleCommitT -> Node f 'SingleCommitT
  SingleNotificationNode :: EntityData f 'SingleNotificationT -> Node f 'SingleNotificationT
  JobLogGroupNode :: EntityData f 'JobLogGroupT -> Node f 'JobLogGroupT

data NodeTyp =
  HeadingT
  | RepoT

  | PaginatedIssuesT
  | PaginatedPullsT
  | PaginatedWorkflowsT
  | PaginatedReposT
  | PaginatedBranchesT
  | PaginatedNotificationsT

  | SingleIssueT
  | SinglePullT
  | SingleWorkflowT
  | SingleJobT
  | SingleBranchT
  | SingleCommitT
  | SingleNotificationT
  | JobLogGroupT

deriving instance Eq (EntityData Fixed a) => Eq (Node Fixed a)

instance Show (Node f a) where
  show (RepoNode (EntityData {..})) = [i|RepoNode<#{_ident}>|]
  show (HeadingNode (EntityData {..})) = [i|HeadingNode<#{_ident}>|]

  show (PaginatedIssuesNode (EntityData {..})) = [i|PaginatedIssuesNode<#{_ident}>|]
  show (PaginatedPullsNode (EntityData {..})) = [i|PaginatedPullsNode<#{_ident}>|]
  show (PaginatedWorkflowsNode (EntityData {..})) = [i|PaginatedWorkflowsNode<#{_ident}>|]
  show (PaginatedReposNode (EntityData {..})) = [i|PaginatedReposNode<#{_ident}>|]
  show (PaginatedBranchesNode (EntityData {..})) = [i|PaginatedBranchesNode<#{_ident}>|]
  show (PaginatedNotificationsNode (EntityData {..})) = [i|PaginatedNotificationsNode<#{_ident}>|]

  show (SingleIssueNode (EntityData {..})) = [i|SingleIssueNode<#{_ident}>|]
  show (SinglePullNode (EntityData {..})) = [i|SinglePullNode<#{_ident}>|]
  show (SingleWorkflowNode (EntityData {..})) = [i|SingleWorkflowNode<#{_ident}>|]
  show (SingleJobNode (EntityData {..})) = [i|SingleJobNode<#{_ident}>|]
  show (SingleBranchNode (EntityData {..})) = [i|SingleBranchNode<#{_ident}>|]
  show (SingleCommitNode (EntityData {..})) = [i|SingleCommitNode<#{_ident}>|]
  show (SingleNotificationNode (EntityData {..})) = [i|SingleNotificationNode<#{_ident}>|]
  show (JobLogGroupNode (EntityData {..})) = [i|JobLogGroupNode<#{_ident}>|]

-- * Entity data

data EntityData f a = EntityData {
  _static :: NodeStatic a
  , _state :: Switchable f (Fetchable (NodeState a))

  , _urlSuffix :: Text

  , _toggled :: Switchable f Bool
  , _children :: Switchable f [NodeChildType f a]

  , _search :: Switchable f Search
  , _pageInfo :: Switchable f PageInfo

  -- Health check fields (currently used only for repos)
  , _healthCheck :: Switchable f (Fetchable HealthCheckResult)
  , _healthCheckThread :: TVar (Maybe (Async ()))

  , _depth :: Int
  , _ident :: Int
  }

deriving instance (Eq (NodeStatic a), Eq (NodeChildType Fixed a), Eq (NodeState a)) => Eq (EntityData Fixed a)

-- * Static state, fetched state, and child state for nodes

type family NodeStatic a where
  NodeStatic PaginatedIssuesT = ()
  NodeStatic PaginatedPullsT = ()
  NodeStatic PaginatedWorkflowsT = ()
  NodeStatic PaginatedReposT = Name User
  NodeStatic PaginatedBranchesT = ()
  NodeStatic PaginatedNotificationsT = ()
  NodeStatic SingleIssueT = Issue
  NodeStatic SinglePullT = Issue
  NodeStatic SingleWorkflowT = WorkflowRun
  NodeStatic SingleJobT = Job
  NodeStatic SingleBranchT = Branch
  NodeStatic SingleCommitT = Commit
  NodeStatic SingleNotificationT = Notification
  NodeStatic JobLogGroupT = JobLogGroup
  NodeStatic HeadingT = Text
  NodeStatic RepoT = (Name Owner, Name Repo)

type family NodeState a where
  NodeState PaginatedIssuesT = SearchResult Issue
  NodeState PaginatedPullsT = SearchResult Issue
  NodeState PaginatedWorkflowsT = WithTotalCount WorkflowRun
  NodeState PaginatedReposT = SearchResult Repo
  NodeState PaginatedBranchesT = V.Vector Branch
  NodeState PaginatedNotificationsT = V.Vector Notification
  NodeState SingleIssueT = V.Vector IssueComment
  NodeState SinglePullT = V.Vector IssueComment
  NodeState SingleWorkflowT = WithTotalCount Job
  NodeState SingleJobT = [JobLogGroup]
  NodeState SingleBranchT = V.Vector Commit
  NodeState SingleCommitT = ()
  NodeState SingleNotificationT = ()
  NodeState JobLogGroupT = ()
  NodeState HeadingT = ()
  NodeState RepoT = Repo

type family NodeChildType f a where
  NodeChildType f PaginatedIssuesT = Node f SingleIssueT
  NodeChildType f PaginatedPullsT = Node f SinglePullT
  NodeChildType f PaginatedWorkflowsT = Node f SingleWorkflowT
  NodeChildType f PaginatedReposT = Node f RepoT
  NodeChildType f PaginatedBranchesT = Node f SingleBranchT
  NodeChildType f PaginatedNotificationsT = Node f SingleNotificationT
  NodeChildType f SingleIssueT = ()
  NodeChildType f SinglePullT = ()
  NodeChildType f SingleWorkflowT = Node f SingleJobT
  NodeChildType f SingleJobT = Node f JobLogGroupT
  NodeChildType f SingleBranchT = Node f SingleCommitT
  NodeChildType f SingleCommitT = ()
  NodeChildType f SingleNotificationT = ()
  NodeChildType f JobLogGroupT = Node f JobLogGroupT
  NodeChildType f HeadingT = SomeNode f
  NodeChildType f RepoT = SomeNode f

-- * Existential wrapper

data SomeNode f where
  SomeNode :: (
    Show (Node f a)
    , Eq (Node Fixed a)
    , Typeable a
    ) => { unSomeNode :: Node f a } -> SomeNode f

instance Eq (SomeNode Fixed) where
  (SomeNode (x :: a)) == (SomeNode y) = case cast y of
    Just (y' :: a) -> x == y'
    _ -> False

deriving instance Show (SomeNode Fixed)
deriving instance Show (SomeNode Variable)

-- * Packing and unpacking

getExistentialChildren :: Node Variable a -> IO [NodeChildType Variable a]
getExistentialChildren node = readTVarIO (_children (getEntityData node))

getExistentialChildrenWrapped :: Node Variable a -> STM [SomeNode Variable]
getExistentialChildrenWrapped node = case node of
  -- These types have SomeNode children
  HeadingNode ed -> readTVar (_children ed)
  RepoNode ed -> readTVar (_children ed)

  -- These types have specific GADT constructor children, so wrap them
  PaginatedIssuesNode ed -> fmap (fmap SomeNode) (readTVar (_children ed))
  PaginatedPullsNode ed -> fmap (fmap SomeNode) (readTVar (_children ed))
  PaginatedWorkflowsNode ed -> fmap (fmap SomeNode) (readTVar (_children ed))
  PaginatedReposNode ed -> fmap (fmap SomeNode) (readTVar (_children ed))
  PaginatedBranchesNode ed -> fmap (fmap SomeNode) (readTVar (_children ed))
  PaginatedNotificationsNode ed -> fmap (fmap SomeNode) (readTVar (_children ed))
  SingleWorkflowNode ed -> fmap (fmap SomeNode) (readTVar (_children ed))
  SingleJobNode ed -> fmap (fmap SomeNode) (readTVar (_children ed))
  SingleBranchNode ed -> fmap (fmap SomeNode) (readTVar (_children ed))
  JobLogGroupNode ed -> fmap (fmap SomeNode) (readTVar (_children ed))

  -- These are leaf nodes with no meaningful children
  SingleIssueNode _ -> return []
  SinglePullNode _ -> return []
  SingleCommitNode _ -> return []
  SingleNotificationNode _ -> return []

getEntityData :: Node f a -> EntityData f a
getEntityData node = node ^. entityDataL

setEntityData :: EntityData f' a -> Node f a -> Node f' a
setEntityData ed node = node & entityDataL .~ ed

entityDataL :: Lens (Node f a) (Node f' a) (EntityData f a) (EntityData f' a)
entityDataL f (PaginatedIssuesNode ed) = PaginatedIssuesNode <$> f ed
entityDataL f (PaginatedPullsNode ed) = PaginatedPullsNode <$> f ed
entityDataL f (PaginatedWorkflowsNode ed) = PaginatedWorkflowsNode <$> f ed
entityDataL f (PaginatedReposNode ed) = PaginatedReposNode <$> f ed
entityDataL f (PaginatedBranchesNode ed) = PaginatedBranchesNode <$> f ed
entityDataL f (PaginatedNotificationsNode ed) = PaginatedNotificationsNode <$> f ed
entityDataL f (SingleIssueNode ed) = SingleIssueNode <$> f ed
entityDataL f (SinglePullNode ed) = SinglePullNode <$> f ed
entityDataL f (SingleWorkflowNode ed) = SingleWorkflowNode <$> f ed
entityDataL f (SingleJobNode ed) = SingleJobNode <$> f ed
entityDataL f (SingleBranchNode ed) = SingleBranchNode <$> f ed
entityDataL f (SingleCommitNode ed) = SingleCommitNode <$> f ed
entityDataL f (SingleNotificationNode ed) = SingleNotificationNode <$> f ed
entityDataL f (JobLogGroupNode ed) = JobLogGroupNode <$> f ed
entityDataL f (HeadingNode ed) = HeadingNode <$> f ed
entityDataL f (RepoNode ed) = RepoNode <$> f ed

-- * Misc

data SortBy =
  SortByStars
  | SortByPushed
  | SortByUpdated
  deriving (Eq)

data JobLogGroup =
  JobLogLines UTCTime [Text]
  | JobLogGroup UTCTime Text [JobLogGroup]
  deriving (Show, Eq)

type Var = TVar

data BaseContext = BaseContext {
  requestSemaphore :: QSem
  , auth :: Auth
  , debugFn :: Text -> IO ()
  , manager :: Manager
  , getIdentifier :: IO Int
  , getIdentifierSTM :: STM Int
  }

data ClickableName =
  ListRow Int
  | MainList
  | InnerViewport Text
  | InfoBar
  | TextForm
  deriving (Show, Ord, Eq)

data Variable (x :: Type)
data Fixed (x :: Type)

type family Switchable (f :: Type -> Type) x where
  Switchable Variable x = TVar x
  Switchable Fixed x = x

data Fetchable a =
  NotFetched
  | Fetching (Maybe a)
  | Errored Text
  | Fetched a
  deriving (Show, Eq)

fetchableCurrent :: Fetchable a -> Maybe a
fetchableCurrent (Fetched x) = Just x
fetchableCurrent (Fetching x) = x
fetchableCurrent _ = Nothing

readFetchableCurrentSTM :: MonadIO m => TVar (Fetchable a) -> m (Maybe a)
readFetchableCurrentSTM var = fetchableCurrent <$> readTVarIO var

markFetching :: TVar (Fetchable a) -> STM ()
markFetching var = do
  previous <- fetchableCurrent <$> readTVar var
  writeTVar var (Fetching previous)

instance Functor Fetchable where
  fmap _ NotFetched = NotFetched
  fmap f (Fetching ma) = Fetching (f <$> ma)
  fmap _ (Errored e) = Errored e
  fmap f (Fetched a) = Fetched (f a)

data WorkflowStatus =
  WorkflowSuccess
  | WorkflowPending
  | WorkflowRunning
  | WorkflowFailed
  | WorkflowCancelled
  | WorkflowNeutral
  | WorkflowUnknown
  deriving (Show, Eq)

data HealthCheckResult =
  HealthCheckWorkflowResult WorkflowStatus
  | HealthCheckNoData
  | HealthCheckUnhealthy Text
  deriving (Show, Eq)

data Search = SearchText Text
            | SearchNone
  deriving (Show, Eq)

data PageInfo = PageInfo {
  pageInfoCurrentPage :: Int
  , pageInfoFirstPage :: Maybe Int
  , pageInfoPrevPage :: Maybe Int
  , pageInfoNextPage :: Maybe Int
  , pageInfoLastPage :: Maybe Int
  } deriving (Show, Eq)

emptyPageInfo :: PageInfo
emptyPageInfo = PageInfo 1 Nothing Nothing Nothing Nothing

-- * Overall app state

data AppEvent =
  ListUpdate (V.Vector (SomeNode Fixed))
  | AnimationTick

data AppState = AppState {
  _appUser :: User
  , _appBaseContext :: BaseContext
  , _appMainListVariable :: V.Vector (SomeNode Variable)
  , _appMainList :: L.List ClickableName (SomeNode Fixed)
  , _appSortBy :: SortBy
  , _appNow :: UTCTime

  , _appForm :: Maybe (Form Text AppEvent ClickableName, Int)
  , _appAnimationCounter :: Int
  }

makeLenses ''AppState
