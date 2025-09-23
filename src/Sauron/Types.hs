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
import Lens.Micro.TH
import Network.HTTP.Client (Manager)
import Relude
import qualified Text.Show
import UnliftIO.Async


-- * Main list elem

data MainListElem' f (a :: NodeTyp) where
  PaginatedIssuesNode :: EntityData f 'PaginatedIssuesT -> MainListElem' f 'PaginatedIssuesT
  PaginatedPullsNode :: EntityData f 'PaginatedPullsT -> MainListElem' f 'PaginatedPullsT
  PaginatedWorkflowsNode :: EntityData f 'PaginatedWorkflowsT -> MainListElem' f 'PaginatedWorkflowsT

  SingleIssueNode :: EntityData f 'SingleIssueT -> MainListElem' f 'SingleIssueT
  SinglePullNode :: EntityData f 'SinglePullT -> MainListElem' f 'SinglePullT
  SingleWorkflowNode :: EntityData f 'SingleWorkflowT -> MainListElem' f 'SingleWorkflowT
  SingleJobNode :: EntityData f 'SingleJobT -> MainListElem' f 'SingleJobT
  JobLogGroupNode :: EntityData f 'JobLogGroupT -> MainListElem' f 'JobLogGroupT
  HeadingNode :: EntityData f 'HeadingT -> MainListElem' f 'HeadingT
  RepoNode :: EntityData f 'RepoT -> MainListElem' f 'RepoT

data NodeTyp =
  PaginatedIssuesT
  | PaginatedPullsT
  | PaginatedWorkflowsT
  | SingleIssueT
  | SinglePullT
  | SingleWorkflowT
  | SingleJobT
  | JobLogGroupT
  | HeadingT
  | RepoT

deriving instance Eq (EntityData Fixed a) => Eq (MainListElem' Fixed a)

instance Show (MainListElem' f a) where
  show (PaginatedIssuesNode (EntityData {..})) = [i|PaginatedIssuesNode<#{_ident}>|]
  show (PaginatedPullsNode (EntityData {..})) = [i|PaginatedPullsNode<#{_ident}>|]
  show (PaginatedWorkflowsNode (EntityData {..})) = [i|PaginatedWorkflowsNode<#{_ident}>|]
  show (SingleIssueNode (EntityData {..})) = [i|SingleIssueNode<#{_ident}>|]
  show (SinglePullNode (EntityData {..})) = [i|SinglePullNode<#{_ident}>|]
  show (SingleWorkflowNode (EntityData {..})) = [i|SingleWorkflowNode<#{_ident}>|]
  show (SingleJobNode (EntityData {..})) = [i|SingleJobNode<#{_ident}>|]
  show (JobLogGroupNode (EntityData {..})) = [i|JobLogGroupNode<#{_ident}>|]
  show (HeadingNode (EntityData {..})) = [i|HeadingNode<#{_ident}>|]
  show (RepoNode (EntityData {..})) = [i|RepoNode<#{_ident}>|]

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
  , _healthCheckThread :: Maybe (Async ())

  , _depth :: Int
  , _ident :: Int
  }

deriving instance (Eq (NodeStatic a), Eq (NodeChildType Fixed a), Eq (NodeState a)) => Eq (EntityData Fixed a)

-- * Static state, fetched state, and child state for nodes

type family NodeStatic a where
  NodeStatic PaginatedIssuesT = ()
  NodeStatic PaginatedPullsT = ()
  NodeStatic PaginatedWorkflowsT = ()
  NodeStatic SingleIssueT = Issue
  NodeStatic SinglePullT = Issue
  NodeStatic SingleWorkflowT = WorkflowRun
  NodeStatic SingleJobT = Job
  NodeStatic JobLogGroupT = JobLogGroup
  NodeStatic HeadingT = Text
  NodeStatic RepoT = (Name Owner, Name Repo)

type family NodeState a where
  NodeState PaginatedIssuesT = SearchResult Issue
  NodeState PaginatedPullsT = SearchResult Issue
  NodeState PaginatedWorkflowsT = WithTotalCount WorkflowRun
  NodeState SingleIssueT = V.Vector IssueComment
  NodeState SinglePullT = V.Vector IssueComment
  NodeState SingleWorkflowT = WithTotalCount Job
  NodeState SingleJobT = [JobLogGroup]
  NodeState JobLogGroupT = ()
  NodeState HeadingT = ()
  NodeState RepoT = Repo

type family NodeChildType f a where
  NodeChildType f PaginatedIssuesT = MainListElem' f SingleIssueT
  NodeChildType f PaginatedPullsT = MainListElem' f SinglePullT
  NodeChildType f PaginatedWorkflowsT = MainListElem' f SingleWorkflowT
  NodeChildType f SingleIssueT = ()
  NodeChildType f SinglePullT = ()
  NodeChildType f SingleWorkflowT = MainListElem' f SingleJobT
  NodeChildType f SingleJobT = MainListElem' f JobLogGroupT
  NodeChildType f JobLogGroupT = MainListElem' f JobLogGroupT
  NodeChildType f HeadingT = SomeMainListElem f
  NodeChildType f RepoT = SomeMainListElem f

-- * Existential wrapper

data SomeMainListElem f where
  SomeMainListElem :: (
    Show (MainListElem' f a)
    , Eq (MainListElem' Fixed a)
    , Typeable a
    ) => { unSomeMainListElem :: MainListElem' f a } -> SomeMainListElem f

instance Eq (SomeMainListElem Fixed) where
  (SomeMainListElem (x :: a)) == (SomeMainListElem y) = case cast y of
    Just (y' :: a) -> x == y'
    _ -> False

deriving instance Show (SomeMainListElem Fixed)
deriving instance Show (SomeMainListElem Variable)

-- * Packing and unpacking

getExistentialChildren :: MainListElem' Variable a -> IO [NodeChildType Variable a]
getExistentialChildren node = readTVarIO (_children (getEntityData node))

getExistentialChildrenWrapped :: MainListElem' Variable a -> STM [SomeMainListElem Variable]
getExistentialChildrenWrapped node = case node of
  -- These types have SomeMainListElem children
  HeadingNode ed -> readTVar (_children ed)
  RepoNode ed -> readTVar (_children ed)

  -- These types have specific GADT constructor children, so wrap them
  PaginatedIssuesNode ed -> fmap (fmap SomeMainListElem) (readTVar (_children ed))
  PaginatedPullsNode ed -> fmap (fmap SomeMainListElem) (readTVar (_children ed))
  PaginatedWorkflowsNode ed -> fmap (fmap SomeMainListElem) (readTVar (_children ed))
  SingleWorkflowNode ed -> fmap (fmap SomeMainListElem) (readTVar (_children ed))
  SingleJobNode ed -> fmap (fmap SomeMainListElem) (readTVar (_children ed))
  JobLogGroupNode ed -> fmap (fmap SomeMainListElem) (readTVar (_children ed))

  -- These are leaf nodes with no meaningful children
  SingleIssueNode _ -> return []
  SinglePullNode _ -> return []

getEntityData :: MainListElem' f a -> EntityData f a
getEntityData (PaginatedIssuesNode ed) = ed
getEntityData (PaginatedPullsNode ed) = ed
getEntityData (PaginatedWorkflowsNode ed) = ed
getEntityData (SingleIssueNode ed) = ed
getEntityData (SinglePullNode ed) = ed
getEntityData (SingleWorkflowNode ed) = ed
getEntityData (SingleJobNode ed) = ed
getEntityData (JobLogGroupNode ed) = ed
getEntityData (HeadingNode ed) = ed
getEntityData (RepoNode ed) = ed

setEntityData :: EntityData f' a -> MainListElem' f a -> MainListElem' f' a
setEntityData ed (PaginatedIssuesNode _) = PaginatedIssuesNode ed
setEntityData ed (PaginatedPullsNode _) = PaginatedPullsNode ed
setEntityData ed (PaginatedWorkflowsNode _) = PaginatedWorkflowsNode ed
setEntityData ed (SingleIssueNode _) = SingleIssueNode ed
setEntityData ed (SinglePullNode _) = SinglePullNode ed
setEntityData ed (SingleWorkflowNode _) = SingleWorkflowNode ed
setEntityData ed (SingleJobNode _) = SingleJobNode ed
setEntityData ed (JobLogGroupNode _) = JobLogGroupNode ed
setEntityData ed (HeadingNode _) = HeadingNode ed
setEntityData ed (RepoNode _) = RepoNode ed

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
  | Fetching
  | Errored Text
  | Fetched a
  deriving (Show, Eq)

instance Functor Fetchable where
  fmap _ NotFetched = NotFetched
  fmap _ Fetching = Fetching
  fmap _ (Errored e) = Errored e
  fmap f (Fetched a) = Fetched (f a)

data WorkflowStatus =
  WorkflowSuccess
  | WorkflowPending
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
  ListUpdate (V.Vector (SomeMainListElem Fixed))
  | AnimationTick

data AppState = AppState {
  _appUser :: User
  , _appBaseContext :: BaseContext
  , _appMainListVariable :: V.Vector (SomeMainListElem Variable)
  , _appMainList :: L.List ClickableName (SomeMainListElem Fixed)
  , _appSortBy :: SortBy
  , _appNow :: UTCTime

  , _appForm :: Maybe (Form Text AppEvent ClickableName, Int)
  , _appAnimationCounter :: Int
  }

makeLenses ''AppState
