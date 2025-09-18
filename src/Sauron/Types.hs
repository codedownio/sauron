{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Sauron.Types where

import Brick.Forms
import qualified Brick.Widgets.List as L
import Control.Concurrent.QSem
import Data.String.Interpolate
import Data.Text
import Data.Time
import qualified Data.Vector as V
import GitHub hiding (Status)
import Lens.Micro.TH
import Network.HTTP.Client (Manager)
import Relude
import qualified Text.Show
import UnliftIO.Async


data SortBy = SortByStars | SortByPushed | SortByUpdated
  deriving (Eq)

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

data WorkflowStatus =
  WorkflowSuccess
  | WorkflowPending
  | WorkflowFailed
  | WorkflowCancelled
  | WorkflowNeutral
  | WorkflowUnknown
  deriving (Show, Eq)

chooseWorkflowStatus :: Text -> WorkflowStatus
chooseWorkflowStatus "completed" = WorkflowSuccess
chooseWorkflowStatus "action_required" = WorkflowPending
chooseWorkflowStatus "cancelled" = WorkflowCancelled
chooseWorkflowStatus "failure" = WorkflowFailed
chooseWorkflowStatus "neutral" = WorkflowNeutral
chooseWorkflowStatus "skipped" = WorkflowCancelled
chooseWorkflowStatus "stale" = WorkflowNeutral
chooseWorkflowStatus "success" = WorkflowSuccess
chooseWorkflowStatus "timed_out" = WorkflowFailed
chooseWorkflowStatus "in_progress" = WorkflowPending
chooseWorkflowStatus "queued" = WorkflowPending
chooseWorkflowStatus "requested" = WorkflowPending
chooseWorkflowStatus "waiting" = WorkflowPending
chooseWorkflowStatus "pending" = WorkflowPending
chooseWorkflowStatus _ = WorkflowUnknown

data HealthCheckResult =
  HealthCheckWorkflowResult WorkflowStatus
  | HealthCheckNoData
  | HealthCheckUnhealthy Text
  deriving (Show, Eq)

data NodeType =
  PaginatedIssues
  | PaginatedPulls
  | PaginatedWorkflows

  | SingleIssue Issue
  | SinglePull Issue
  | SingleWorkflow WorkflowRun
  | SingleJob Job
  deriving (Show, Eq)

data NodeState =
  PaginatedItemsIssues (SearchResult Issue)
  | PaginatedItemsPulls (SearchResult Issue)
  | PaginatedItemsWorkflows (WithTotalCount WorkflowRun)
  | PaginatedItemsJobs (WithTotalCount Job)

  | PaginatedItemIssue (V.Vector IssueComment)
  | PaginatedItemPull (V.Vector IssueComment)
  | PaginatedItemWorkflow (WithTotalCount Job)
  | PaginatedItemJob Job
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

data MainListElem' f =
  MainListElemHeading {
      _label :: Text
      , _depth :: Int
      , _toggled :: Switchable f Bool
      , _status :: Switchable f (Fetchable ())
      , _ident :: Int
      }
  | MainListElemRepo {
      _namespaceName :: (Name Owner, Name Repo)
      , _repo :: Switchable f (Fetchable Repo)

      -- TODO: move the healthCheck thread inside the _repo?
      -- kill it and restart when the repo is refreshed?
      , _healthCheck :: Switchable f (Fetchable HealthCheckResult)
      , _healthCheckThread :: Maybe (Async ())

      , _toggled :: Switchable f Bool
      , _issuesChild :: Switchable f (MainListElem' f)
      , _pullsChild :: Switchable f (MainListElem' f)
      , _workflowsChild :: Switchable f (MainListElem' f)

      , _depth :: Int
      , _ident :: Int
      }
  | MainListElemItem {
      _typ :: NodeType
      , _state :: Switchable f (Fetchable NodeState)

      , _urlSuffix :: Text

      , _toggled :: Switchable f Bool
      , _children :: Switchable f [MainListElem' f]

      , _search :: Switchable f Search
      , _pageInfo :: Switchable f PageInfo

      , _depth :: Int
      , _ident :: Int
      }

type MainListElem = MainListElem' Fixed
type MainListElemVariable = MainListElem' Variable

deriving instance Eq MainListElem

data AppEvent =
  ListUpdate (V.Vector MainListElem)

instance Show (MainListElem' Variable) where
  show (MainListElemHeading {..}) = [i|Heading<#{_label}>|]
  show (MainListElemRepo {_namespaceName=(owner, name)}) = [i|Repo<#{owner}, #{name}>|]
  show (MainListElemItem {..}) = [i|Item<#{_typ}>|]

instance Show (MainListElem' Fixed) where
  show (MainListElemHeading {..}) = [i|Heading<#{_label}>|]
  show (MainListElemRepo {_namespaceName=(owner, name)}) = [i|Repo<#{owner}, #{name}>|]
  show (MainListElemItem {..}) = [i|Item<#{_typ}, #{_state}>|]

data AppState = AppState {
  _appUser :: User
  , _appBaseContext :: BaseContext
  , _appMainListVariable :: V.Vector MainListElemVariable
  , _appMainList :: L.List ClickableName MainListElem
  , _appSortBy :: SortBy
  , _appNow :: UTCTime

  , _appForm :: Maybe (Form Text AppEvent ClickableName, Int)
  }



makeLenses ''AppState
makeLenses ''MainListElem'
