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
import Data.Text
import Data.Time
import Data.Typeable
import qualified Data.Vector as V
import GitHub hiding (Status)
import Lens.Micro.TH
import Network.HTTP.Client (Manager)
import Relude
import qualified Text.Show
import UnliftIO.Async


data SortBy = SortByStars | SortByPushed | SortByUpdated
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

data SomeMainListElem f where
  SomeMainListElem :: (Typeable a) => { unSomeMainListElem :: MainListElem' f a } -> SomeMainListElem f

getExistentialChildren :: MainListElem' f a -> [MainListElem' f a]
getExistentialChildren _ = undefined

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

getEntityData :: MainListElem' f a -> EntityData f a
getEntityData = undefined

type MainListElem = SomeMainListElem Fixed
type MainListElemVariable = SomeMainListElem Variable

data AppEvent =
  ListUpdate (V.Vector MainListElem)
  | AnimationTick

data AppState = AppState {
  _appUser :: User
  , _appBaseContext :: BaseContext
  , _appMainListVariable :: V.Vector MainListElemVariable
  , _appMainList :: L.List ClickableName MainListElem
  , _appSortBy :: SortBy
  , _appNow :: UTCTime

  , _appForm :: Maybe (Form Text AppEvent ClickableName, Int)
  , _appAnimationCounter :: Int
  }

makeLenses ''AppState
makeLenses ''MainListElem'
