{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Sauron.Types where

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
  }

data ClickableName = ListRow Int | MainList | InnerViewport Text | InfoBar
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

data PaginatedType =
  PaginatedIssues
  | PaginatedWorkflows
  deriving (Show, Eq)

data PaginatedItems =
  PaginatedItemsIssues (V.Vector Issue)
  | PaginatedItemsWorkflows (WithTotalCount WorkflowRun)
  deriving (Show, Eq)

data PaginatedItem =
  PaginatedItemIssue Issue
  | PaginatedItemWorkflow WorkflowRun
  deriving (Show, Eq)

data PaginatedItemInner =
  PaginatedItemInnerIssue (V.Vector IssueComment)
  | PaginatedItemInnerWorkflow (WithTotalCount Job)
  deriving (Show, Eq)

paginatedItemsToList :: PaginatedItems -> [PaginatedItem]
paginatedItemsToList (PaginatedItemsIssues xs) = fmap PaginatedItemIssue $ V.toList xs
paginatedItemsToList (PaginatedItemsWorkflows (WithTotalCount xs _)) = fmap PaginatedItemWorkflow $ V.toList xs

data PageInfo = PageInfo {
  pageInfoCurrentPage :: Int
  , pageInfoFirstPage :: Maybe Int
  , pageInfoPrevPage :: Maybe Int
  , pageInfoNextPage :: Maybe Int
  , pageInfoLastPage :: Maybe Int
  } deriving (Show, Eq)

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
      , _workflowsChild :: Switchable f (MainListElem' f)

      , _depth :: Int
      , _ident :: Int
      }
  | MainListElemPaginated {
      _typ :: PaginatedType
      , _items :: Switchable f (Fetchable PaginatedItems)

      -- This is a reference to the same fetchable as the parent repo. It's only
      -- provided here to support openingi etc.
      , _repo :: Switchable f (Fetchable Repo)

      , _label :: Text
      , _urlSuffix :: Text

      , _toggled :: Switchable f Bool
      , _children :: Switchable f [MainListElem' f]

      , _pageInfo :: Switchable f PageInfo

      , _depth :: Int
      , _ident :: Int
      }
  | MainListElemItem {
      _item :: Switchable f (Fetchable PaginatedItem)
      , _itemInner :: Switchable f (Fetchable PaginatedItemInner)

      , _toggled :: Switchable f Bool

      , _depth :: Int
      , _ident :: Int
      }

makeLenses ''MainListElem'
type MainListElem = MainListElem' Fixed
deriving instance Eq MainListElem
type MainListElemVariable = MainListElem' Variable

instance Show (MainListElem' f) where
  show (MainListElemHeading {..}) = [i|Heading<#{_label}>|]
  show (MainListElemRepo {_namespaceName=(owner, name)}) = [i|Repo<#{owner}, #{name}>|]
  show (MainListElemPaginated {..}) = [i|Paginated<#{_label}>|]
  show (MainListElemItem {}) = [i|Item|]

data AppState = AppState {
  _appUser :: User
  , _appBaseContext :: BaseContext
  , _appMainListVariable :: V.Vector MainListElemVariable
  , _appMainList :: L.List ClickableName MainListElem
  , _appSortBy :: SortBy
  , _appNow :: UTCTime
  }
makeLenses ''AppState

data AppEvent =
  ListUpdate (V.Vector MainListElem)
