{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}
-- For JobLogGroup, whose two constructors carry different fields. The modal states
-- used to need this too, before each got its own type.
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Sauron.Types where

import Brick
import Brick.BChan
import Brick.Forms
import Brick.Widgets.Edit (Editor)
import qualified Brick.Widgets.List as L
import Control.Concurrent.QSem
import Control.Monad.Logger (LogLevel(..))
import Data.Aeson
import Data.String.Interpolate
import Data.Text ()
import Data.Time
import Data.Typeable
import qualified Data.Vector as V
import GitHub hiding (Status)
import qualified Graphics.Vty as V
import Lens.Micro
import Lens.Micro.TH
import Network.HTTP.Client (Manager)
import Relude
import qualified Text.Show
import UnliftIO.Async
import WEditorBrick.WrappingEditor (WrappingEditor)


-- * ListDrawable typeclass

-- | Typeclass for rendering nodes in the list
class ListDrawable f a where
  -- | Draw the main line for a node (the summary/header line)
  drawLine :: AppState -> EntityData f a -> Widget ClickableName

  -- | Draw the inner content when the node is toggled open
  -- This should return Nothing if the node doesn't support inner content
  drawInner :: AppState -> EntityData f a -> Maybe (Widget ClickableName)
  drawInner _ _ = Nothing

  -- | Get extra widgets to display in the top box for this node type
  -- These widgets will be appended to column3 in TopBox.hs
  getExtraTopBoxWidgets :: AppState -> EntityData f a -> [Widget ClickableName]
  getExtraTopBoxWidgets _ _ = []

  -- | Handle a hotkey press for this node type
  -- Returns True if the hotkey was handled, False otherwise
  -- If True is returned, no further hotkey matching will occur
  handleHotkey :: AppState -> V.Key -> EntityData f a -> EventM ClickableName AppState Bool
  handleHotkey _ _ _ = return False

-- * Main list elem

data Node f (a :: NodeTyp) where
  HeadingNode :: EntityData f 'HeadingT -> Node f 'HeadingT
  RepoNode :: EntityData f 'RepoT -> Node f 'RepoT

  PaginatedIssuesNode :: EntityData f 'PaginatedIssuesT -> Node f 'PaginatedIssuesT
  PaginatedPullsNode :: EntityData f 'PaginatedPullsT -> Node f 'PaginatedPullsT
  PaginatedWorkflowsNode :: EntityData f 'PaginatedWorkflowsT -> Node f 'PaginatedWorkflowsT
  PaginatedReposNode :: EntityData f 'PaginatedReposT -> Node f 'PaginatedReposT
  PaginatedBranchesNode :: EntityData f 'PaginatedBranchesT -> Node f 'PaginatedBranchesT
  PaginatedYourBranchesNode :: EntityData f 'PaginatedYourBranchesT -> Node f 'PaginatedYourBranchesT
  PaginatedActiveBranchesNode :: EntityData f 'PaginatedActiveBranchesT -> Node f 'PaginatedActiveBranchesT
  PaginatedStaleBranchesNode :: EntityData f 'PaginatedStaleBranchesT -> Node f 'PaginatedStaleBranchesT
  PaginatedNotificationsNode :: EntityData f 'PaginatedNotificationsT -> Node f 'PaginatedNotificationsT

  SingleIssueNode :: EntityData f 'SingleIssueT -> Node f 'SingleIssueT
  SinglePullNode :: EntityData f 'SinglePullT -> Node f 'SinglePullT
  SingleWorkflowNode :: EntityData f 'SingleWorkflowT -> Node f 'SingleWorkflowT
  SingleJobNode :: EntityData f 'SingleJobT -> Node f 'SingleJobT
  SingleBranchNode :: EntityData f 'SingleBranchT -> Node f 'SingleBranchT
  SingleBranchWithInfoNode :: EntityData f 'SingleBranchWithInfoT -> Node f 'SingleBranchWithInfoT
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
  | PaginatedYourBranchesT
  | PaginatedActiveBranchesT
  | PaginatedStaleBranchesT
  | PaginatedNotificationsT

  | SingleIssueT
  | SinglePullT
  | SingleWorkflowT
  | SingleJobT
  | SingleBranchT
  | SingleBranchWithInfoT
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
  show (PaginatedYourBranchesNode (EntityData {..})) = [i|PaginatedYourBranchesNode<#{_ident}>|]
  show (PaginatedActiveBranchesNode (EntityData {..})) = [i|PaginatedActiveBranchesNode<#{_ident}>|]
  show (PaginatedStaleBranchesNode (EntityData {..})) = [i|PaginatedStaleBranchesNode<#{_ident}>|]
  show (PaginatedNotificationsNode (EntityData {..})) = [i|PaginatedNotificationsNode<#{_ident}>|]

  show (SingleIssueNode (EntityData {..})) = [i|SingleIssueNode<#{_ident}>|]
  show (SinglePullNode (EntityData {..})) = [i|SinglePullNode<#{_ident}>|]
  show (SingleWorkflowNode (EntityData {..})) = [i|SingleWorkflowNode<#{_ident}>|]
  show (SingleJobNode (EntityData {..})) = [i|SingleJobNode<#{_ident}>|]
  show (SingleBranchNode (EntityData {..})) = [i|SingleBranchNode<#{_ident}>|]
  show (SingleBranchWithInfoNode (EntityData {..})) = [i|SingleBranchWithInfoNode<#{_ident}>|]
  show (SingleCommitNode (EntityData {..})) = [i|SingleCommitNode<#{_ident}>|]
  show (SingleNotificationNode (EntityData {..})) = [i|SingleNotificationNode<#{_ident}>|]
  show (JobLogGroupNode (EntityData {..})) = [i|JobLogGroupNode<#{_ident}>|]

-- * Entity data

data EntityData f a = EntityData {
  _static :: NodeStatic a
  , _state :: Switchable f (NodeState a)

  , _urlSuffix :: Text

  , _toggled :: Switchable f Bool
  , _children :: Switchable f [NodeChildType f a]

  -- Health check fields (currently used only for repos)
  , _healthCheck :: Switchable f (Fetchable HealthCheckResult)
  , _healthCheckThread :: Switchable f (Maybe (Async (), Int))

  , _depth :: Int
  , _ident :: Int
  }

deriving instance (Eq (NodeStatic a), Eq (NodeChildType Fixed a), Eq (NodeState a)) => Eq (EntityData Fixed a)

-- * Static state, fetched state, and child state for nodes

type family NodeStatic a where
  NodeStatic PaginatedIssuesT = Text
  NodeStatic PaginatedPullsT = Text
  NodeStatic PaginatedWorkflowsT = ()
  NodeStatic PaginatedReposT = Name User
  NodeStatic PaginatedBranchesT = ()
  NodeStatic PaginatedYourBranchesT = ()
  NodeStatic PaginatedActiveBranchesT = ()
  NodeStatic PaginatedStaleBranchesT = ()
  NodeStatic PaginatedNotificationsT = ()
  NodeStatic SingleIssueT = Issue
  NodeStatic SinglePullT = Issue
  NodeStatic SingleWorkflowT = WorkflowRun
  NodeStatic SingleJobT = Job
  NodeStatic SingleBranchT = Branch
  NodeStatic SingleBranchWithInfoT = (BranchWithInfo, ColumnWidths)
  NodeStatic SingleCommitT = Commit
  NodeStatic SingleNotificationT = Notification
  NodeStatic JobLogGroupT = JobLogGroup
  NodeStatic HeadingT = Text
  NodeStatic RepoT = (Name Owner, Name Repo)

type TotalCount = Int

type family NodeState a where
  NodeState PaginatedIssuesT = (Search, PageInfo, Fetchable TotalCount)
  NodeState PaginatedPullsT = (Search, PageInfo, Fetchable TotalCount)
  NodeState PaginatedWorkflowsT = (Search, PageInfo, Fetchable TotalCount)
  NodeState PaginatedReposT = (Search, PageInfo, Fetchable TotalCount)
  NodeState PaginatedBranchesT = (Search, PageInfo, Fetchable TotalCount)
  NodeState PaginatedYourBranchesT = (Search, PageInfo, Fetchable TotalCount)
  NodeState PaginatedActiveBranchesT = (Search, PageInfo, Fetchable TotalCount)
  NodeState PaginatedStaleBranchesT = (Search, PageInfo, Fetchable TotalCount)
  NodeState PaginatedNotificationsT = (Search, PageInfo, Fetchable TotalCount)
  NodeState SingleIssueT = Fetchable (V.Vector TimelineEvent)
  NodeState SinglePullT = PullNodeState
  NodeState SingleWorkflowT = WorkflowNodeState
  NodeState SingleJobT = JobNodeState
  NodeState SingleBranchT = Fetchable (V.Vector Commit)
  NodeState SingleBranchWithInfoT = Fetchable (V.Vector Commit)
  NodeState SingleCommitT = Fetchable Commit
  NodeState SingleNotificationT = NotificationState
  NodeState JobLogGroupT = Maybe ScrollTarget
  NodeState HeadingT = ()
  NodeState RepoT = Fetchable Repo

type family NodeChildType f a where
  NodeChildType f PaginatedIssuesT = Node f SingleIssueT
  NodeChildType f PaginatedPullsT = Node f SinglePullT
  NodeChildType f PaginatedWorkflowsT = Node f SingleWorkflowT
  NodeChildType f PaginatedReposT = Node f RepoT
  NodeChildType f PaginatedBranchesT = Node f SingleBranchT
  NodeChildType f PaginatedYourBranchesT = Node f SingleBranchWithInfoT
  NodeChildType f PaginatedActiveBranchesT = Node f SingleBranchWithInfoT
  NodeChildType f PaginatedStaleBranchesT = Node f SingleBranchWithInfoT
  NodeChildType f PaginatedNotificationsT = Node f SingleNotificationT
  NodeChildType f SingleIssueT = ()
  NodeChildType f SinglePullT = ()
  NodeChildType f SingleWorkflowT = Node f SingleJobT
  NodeChildType f SingleJobT = Node f JobLogGroupT
  NodeChildType f SingleBranchT = Node f SingleCommitT
  NodeChildType f SingleBranchWithInfoT = Node f SingleCommitT
  NodeChildType f SingleCommitT = ()
  NodeChildType f SingleNotificationT = ()
  NodeChildType f JobLogGroupT = Node f JobLogGroupT
  NodeChildType f HeadingT = SomeNode f
  NodeChildType f RepoT = SomeNode f

-- * Existential wrapper

type SomeNodeConstraints f a = (
  Show (Node f a)
  , Eq (Node Fixed a)
  , Eq (NodeState a)
  , Typeable a
  )

data SomeNode f where
  SomeNode :: SomeNodeConstraints f a => { unSomeNode :: Node f a } -> SomeNode f

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
getExistentialChildrenWrapped = getExistentialChildrenWrapped' readTVar

getExistentialChildrenWrappedIdentity :: Node Fixed a -> Identity [SomeNode Fixed]
getExistentialChildrenWrappedIdentity = getExistentialChildrenWrapped' return

getExistentialChildrenWrappedPure :: Node Fixed a -> [SomeNode Fixed]
getExistentialChildrenWrappedPure = runIdentity . getExistentialChildrenWrapped' return

getExistentialChildrenWrapped' :: (Applicative m) => (Switchable f [NodeChildType f a] -> m [NodeChildType f a]) -> Node f a -> m [SomeNode f]
getExistentialChildrenWrapped' readChildren node = case node of
  -- These types have SomeNode children
  HeadingNode ed -> readChildren (_children ed)
  RepoNode ed -> readChildren (_children ed)

  -- These types have specific GADT constructor children, so wrap them
  PaginatedIssuesNode ed -> fmap (fmap SomeNode) (readChildren (_children ed))
  PaginatedPullsNode ed -> fmap (fmap SomeNode) (readChildren (_children ed))
  PaginatedWorkflowsNode ed -> fmap (fmap SomeNode) (readChildren (_children ed))
  PaginatedReposNode ed -> fmap (fmap SomeNode) (readChildren (_children ed))
  PaginatedBranchesNode ed -> fmap (fmap SomeNode) (readChildren (_children ed))
  PaginatedYourBranchesNode ed -> fmap (fmap SomeNode) (readChildren (_children ed))
  PaginatedActiveBranchesNode ed -> fmap (fmap SomeNode) (readChildren (_children ed))
  PaginatedStaleBranchesNode ed -> fmap (fmap SomeNode) (readChildren (_children ed))
  PaginatedNotificationsNode ed -> fmap (fmap SomeNode) (readChildren (_children ed))
  SingleWorkflowNode ed -> fmap (fmap SomeNode) (readChildren (_children ed))
  SingleJobNode ed -> fmap (fmap SomeNode) (readChildren (_children ed))
  SingleBranchNode ed -> fmap (fmap SomeNode) (readChildren (_children ed))
  SingleBranchWithInfoNode ed -> fmap (fmap SomeNode) (readChildren (_children ed))
  JobLogGroupNode ed -> fmap (fmap SomeNode) (readChildren (_children ed))

  -- These are leaf nodes with no meaningful children
  SingleIssueNode _ -> pure []
  SinglePullNode _ -> pure []
  SingleCommitNode _ -> pure []
  SingleNotificationNode _ -> pure []

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
entityDataL f (PaginatedYourBranchesNode ed) = PaginatedYourBranchesNode <$> f ed
entityDataL f (PaginatedActiveBranchesNode ed) = PaginatedActiveBranchesNode <$> f ed
entityDataL f (PaginatedStaleBranchesNode ed) = PaginatedStaleBranchesNode <$> f ed
entityDataL f (PaginatedNotificationsNode ed) = PaginatedNotificationsNode <$> f ed
entityDataL f (SingleIssueNode ed) = SingleIssueNode <$> f ed
entityDataL f (SinglePullNode ed) = SinglePullNode <$> f ed
entityDataL f (SingleWorkflowNode ed) = SingleWorkflowNode <$> f ed
entityDataL f (SingleJobNode ed) = SingleJobNode <$> f ed
entityDataL f (SingleBranchNode ed) = SingleBranchNode <$> f ed
entityDataL f (SingleBranchWithInfoNode ed) = SingleBranchWithInfoNode <$> f ed
entityDataL f (SingleCommitNode ed) = SingleCommitNode <$> f ed
entityDataL f (SingleNotificationNode ed) = SingleNotificationNode <$> f ed
entityDataL f (JobLogGroupNode ed) = JobLogGroupNode <$> f ed
entityDataL f (HeadingNode ed) = HeadingNode <$> f ed
entityDataL f (RepoNode ed) = RepoNode <$> f ed

-- * Notification content

data NotificationContent =
  NotificationIssue Issue (V.Vector TimelineEvent)
  | NotificationPull Issue (V.Vector TimelineEvent)
  | NotificationRelease Release
  | NotificationOther -- ^ For subject types we don't know how to render inline
  deriving (Show, Eq)

-- | The GitHub notification @subject.type@ value for a release.
subjectTypeRelease :: Text
subjectTypeRelease = "Release"

data SubjectState = IssueOpen | IssueClosed | PullOpen | PullClosed | PullMerged | PullDraft
  deriving (Show, Eq)

data NotificationState = NotificationState {
  notificationStateContent :: Fetchable NotificationContent
  , notificationStateSubjectState :: Maybe SubjectState
  , notificationStateAutoScroll :: Bool -- ^ Whether to auto-scroll to the latest comment
  } deriving (Show, Eq)

-- * Data types beyond "github" package

data GraphQLPullRequest = GraphQLPullRequest {
  prNumber :: Maybe Int
  , prTitle :: Maybe Text
  , prUrl :: Maybe Text
  , prState :: Maybe Text
  } deriving (Show, Eq, Generic)
instance FromJSON GraphQLPullRequest where
  parseJSON = withObject "GraphQLPullRequest" $ \o -> GraphQLPullRequest
    <$> o .:? "number"
    <*> o .:? "title"
    <*> o .:? "url"
    <*> o .:? "state"

data BranchWithInfo = BranchWithInfo {
  branchWithInfoBranchName :: Text
  , branchWithInfoCommitOid :: Maybe Text
  , branchWithInfoCommitAuthor :: Maybe Text
  , branchWithInfoAuthorEmail :: Maybe Text
  , branchWithInfoCommitDate :: Maybe Text
  , branchWithInfoCheckStatus :: Maybe Text
  , branchWithInfoAssociatedPR :: Maybe GraphQLPullRequest
  , branchWithInfoAheadBy :: Maybe Int
  , branchWithInfoBehindBy :: Maybe Int
  } deriving (Show, Eq)

data ColumnWidths = ColumnWidths {
  cwCommitTime :: Int
  , cwCheckStatus :: Int
  , cwAheadBehind :: Int
  , cwPRInfo :: Int
  } deriving (Show, Eq)

-- * Misc

data SortBy =
  SortByStars
  | SortByPushed
  | SortByUpdated
  deriving (Eq)

data LogSplitMethod
  = LogsNotSplit
  | PerStepLogs
  | FlatLogTimestampSplit
  deriving (Show, Eq)

data JobLogGroup = JobLogLines {
  jobLogLinesTimestamp :: UTCTime
  , jobLogLinesLines :: [Text]
  }
  | JobLogGroup {
      jobLogGroupTimestamp :: UTCTime
      , jobLogGroupTitle :: Text
      , jobLogGroupStatus :: Maybe Text
      , jobLogGroupDuration :: Maybe NominalDiffTime
      , jobLogGroupMaxSiblingDuration :: Maybe NominalDiffTime
      , jobLogGroupChildren :: [JobLogGroup]
      }
  deriving (Show, Eq)

data JobNodeState = JobNodeState {
  jnsJob :: Fetchable Job
  , jnsLogs :: Fetchable ([JobLogGroup], LogSplitMethod)
  , jnsMaxSiblingDuration :: Maybe NominalDiffTime
  } deriving (Show, Eq)

type Var = TVar

data BaseContext = BaseContext {
  requestSemaphore :: QSem
  , auth :: Auth
  , debugFn :: Text -> IO ()
  , manager :: Manager
  , getIdentifier :: IO Int
  , getIdentifierSTM :: STM Int
  , eventChan :: BChan AppEvent
  , currentUser :: Maybe User
  }

data ClickableName =
  MainUI
  | ListRow Int
  | MainList
  | InnerViewport Text
  | InfoBar
  | TextForm
  | CommentEditor
  | ZoomModalContent
  | LogSplitContent
  | NewIssueTitleEditor
  | NewIssueBodyEditor
  | MergeCommitTitleEditor
  | MergeCommitMessageEditor
  | ScrollbarClick ClickableScrollbarElement ClickableName
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

data WorkflowJobSortBy =
  SortJobsByFailures
  | SortJobsByName
  | SortJobsByRuntime
  deriving (Show, Eq)

-- | The state of an opened pull request node: everything the PR modal's tabs show.
-- The commits, files and viewed states are fetched lazily when their tab is opened.
data PullNodeState = PullNodeState {
  pullNodeStateTimeline :: Fetchable (V.Vector TimelineEvent)
  , pullNodeStateDetails :: Fetchable PullRequest
  , pullNodeStateChecks :: Fetchable (V.Vector CheckRun)
  , pullNodeStateCommits :: Fetchable (V.Vector Commit)
  -- | Full details (including patches) of commits expanded in the Commits tab, by sha
  , pullNodeStateCommitDetails :: Map Text (Fetchable Commit)
  , pullNodeStateFiles :: Fetchable (V.Vector File)
  , pullNodeStateViewedStates :: Map Text FileViewedState
  -- | The PR's GraphQL node id, needed by the mark/unmark viewed mutations
  , pullNodeStatePullRequestId :: Maybe Text
  } deriving (Show, Eq)

emptyPullNodeState :: PullNodeState
emptyPullNodeState = PullNodeState {
  pullNodeStateTimeline = NotFetched
  , pullNodeStateDetails = NotFetched
  , pullNodeStateChecks = NotFetched
  , pullNodeStateCommits = NotFetched
  , pullNodeStateCommitDetails = mempty
  , pullNodeStateFiles = NotFetched
  , pullNodeStateViewedStates = mempty
  , pullNodeStatePullRequestId = Nothing
  }

data WorkflowNodeState = WorkflowNodeState {
  workflowNodeStateFetchable :: Fetchable TotalCount
  , workflowNodeStateJobPage :: Int
  , workflowNodeStateJobSortBy :: WorkflowJobSortBy
  -- | Set while the repo's run poller is refreshing this workflow. Its status comes from a batch
  -- query rather than a fetch of its own, so this is what tells the UI it's being refreshed.
  , workflowNodeStatePolling :: Bool
  } deriving (Show, Eq)

workflowJobPageSize :: Int
workflowJobPageSize = 20

notificationPageSize :: Int
notificationPageSize = 10

-- | Take just the notifications shown on the given page. Kept here so the expanded-list
-- flattening and the nthChild traversal paginate notifications identically.
paginateNotifications :: Int -> [a] -> [a]
paginateNotifications page xs
  | length xs <= notificationPageSize = xs
  | otherwise = take notificationPageSize $ drop ((page - 1) * notificationPageSize) xs

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

-- * Logging

data LogEntry = LogEntry {
  _logEntryTimestamp :: UTCTime
  , _logEntryLevel :: LogLevel
  , _logEntryMessage :: Text
  , _logEntryDuration :: Maybe NominalDiffTime
  , _logEntryStackTrace :: Maybe CallStack
  } deriving (Show)

instance Eq LogEntry where
  (LogEntry t1 l1 m1 d1 _) == (LogEntry t2 l2 m2 d2 _) =
    t1 == t2 && l1 == l2 && m1 == m2 && d1 == d2

-- * Overall app state

data ToastLevel = ToastDefault | ToastSuccess | ToastWarn | ToastError
  deriving (Show, Eq, Enum, Bounded)

data AppEvent =
  ListUpdate UTCTime (V.Vector (SomeNode Fixed))
  | ModalUpdate (Maybe (ModalState Fixed))
  | AnimationTick
  | TimeUpdated UTCTime
  | CommentModalEvent CommentModalEvent
  | NewIssueModalEvent NewIssueModalEvent
  | MergeModalEvent MergeModalEvent
  | LogEntryAdded LogEntry
  | ToastFired ToastLevel Text
  | ToastWidgetFired ToastLevel (Widget ClickableName)

data ScrollTarget =
  ScrollToBeginning
  | ScrollToLine Int
  | ScrollToEnd
  deriving (Show, Eq)

data CommentModalEvent =
  CommentSubmitted (Either Error Comment)
  | IssueClosedWithComment (Either Error Issue)
  -- | Turn on the zoom modal's comment editor for this issue/PR (fired from a
  -- background thread once the issue behind a notification has been fetched)
  | EnterCommentMode Issue Bool (Name Owner) (Name Repo)

data NewIssueModalEvent =
  NewIssueCreated (Either Error Issue)

-- | The merge either went through (carrying GitHub's confirmation message) or
-- was rejected (carrying GitHub's explanation, e.g. "Pull Request is not mergeable").
data MergeModalEvent =
  MergeFinished (Either Text Text)

-- | The web UI's per-file "viewed" checkbox state. Dismissed means the file was
-- viewed but new changes were pushed since.
data FileViewedState = FileViewed | FileUnviewed | FileDismissed
  deriving (Show, Eq, Ord)

data SubmissionState =
  NotSubmitting
  | SubmittingComment
  | SubmittingCloseWithComment
  | SubmittingNewIssue
  | SubmittingMerge
  deriving (Show, Eq)

data MergeFocus = MergeFocusMethods | MergeFocusTitle | MergeFocusBody
  deriving (Show, Eq)

-- | The tabs of the pull request modal, mirroring the web UI
data PullModalTab = TabConversation | TabCommits | TabChecks | TabReview
  deriving (Show, Eq, Enum, Bounded)

tabTitle :: PullModalTab -> String
tabTitle TabConversation = "Conversation"
tabTitle TabCommits = "Commits"
tabTitle TabChecks = "Checks"
tabTitle TabReview = "Review"

-- | State of the comment editor shown at the bottom of the zoom modal
data CommentMode = CommentMode {
  _commentModeEditor :: WrappingEditor Char ClickableName
  , _commentModeIssue :: Issue
  , _commentModeIsPR :: Bool
  , _commentModeOwner :: Name Owner
  , _commentModeName :: Name Repo
  , _commentModeSubmission :: SubmissionState
  }

-- | Each modal's state is its own type rather than a clutch of fields on
-- 'ModalState', so that code which only makes sense for one modal -- its renderer,
-- its key handler -- can say which one in its type and read the fields totally.
-- 'ModalState' just says which of them is open.

data ZoomModal f = ZoomModal {
  _zoomModalSomeNode :: SomeNode f
  , _zoomModalParents :: [SomeNode f]
  -- | The inline comment editor, when comment mode is on
  , _zoomModalCommentMode :: Maybe CommentMode
  }

data PullModal f = PullModal {
  _pullModalNode :: Node f 'SinglePullT
  , _pullModalParents :: [SomeNode f]
  -- | The inline comment editor on the Conversation tab, when it's focused
  , _pullModalCommentMode :: Maybe CommentMode
  , _pullModalTab :: PullModalTab
  , _pullModalCurrentFile :: Int
  , _pullModalSelectedCommit :: Int
  , _pullModalExpandedCommits :: Set Text
  }

-- | Unparameterized: this modal holds no node-tree data, so the fixer passes it
-- through untouched.
data NewIssueModal = NewIssueModal {
  _newIssueTitleEditor :: Editor Text ClickableName
  , _newIssueBodyEditor :: WrappingEditor Char ClickableName
  , _newIssueRepoOwner :: Name Owner
  , _newIssueRepoName :: Name Repo
  , _newIssueSubmissionState :: SubmissionState
  , _newIssueFocusTitle :: Bool -- True = title focused, False = body focused
  }

-- | Unparameterized, for the same reason as 'NewIssueModal'.
data MergeModal = MergeModal {
  _mergeIssue :: Issue
  , _mergeRepoOwner :: Name Owner
  , _mergeRepoName :: Name Repo
  , _mergeMethod :: MergeMethod
  -- | Commit title/message for the squash commit; empty means GitHub's default.
  -- Only shown for squash merges.
  , _mergeCommitTitleEditor :: Editor Text ClickableName
  , _mergeCommitMessageEditor :: Editor Text ClickableName
  , _mergeFocus :: MergeFocus
  , _mergeSubmissionState :: SubmissionState
  }

-- | Which modal is open, if any.
data ModalState f =
  ZoomModalState (ZoomModal f)
  | PullRequestModalState (PullModal f)
  | NewIssueModalState NewIssueModal
  | MergeModalState MergeModal
  | HelpModalState

instance Eq (ZoomModal Fixed) where
  (ZoomModal node1 parents1 comment1) == (ZoomModal node2 parents2 comment2) =
    node1 == node2 && parents1 == parents2 && sameCommentMode comment1 comment2

instance Eq (PullModal Fixed) where
  (PullModal node1 parents1 comment1 tab1 file1 commit1 expanded1) ==
    (PullModal node2 parents2 comment2 tab2 file2 commit2 expanded2) =
    node1 == node2 && parents1 == parents2 && sameCommentMode comment1 comment2
    && tab1 == tab2 && file1 == file2 && commit1 == commit2 && expanded1 == expanded2

-- | Editors have no Eq, so they're compared by the issue they're about
instance Eq NewIssueModal where
  (NewIssueModal _t1 _b1 o1 n1 s1 _f1) == (NewIssueModal _t2 _b2 o2 n2 s2 _f2) =
    o1 == o2 && n1 == n2 && s1 == s2

instance Eq MergeModal where
  (MergeModal issue1 owner1 name1 method1 _t1 _m1 _f1 submission1) == (MergeModal issue2 owner2 name2 method2 _t2 _m2 _f2 submission2) =
    issue1 == issue2 && owner1 == owner2 && name1 == name2 && method1 == method2 && submission1 == submission2

instance Eq (ModalState Fixed) where
  (ZoomModalState zoom1) == (ZoomModalState zoom2) = zoom1 == zoom2
  (PullRequestModalState pull1) == (PullRequestModalState pull2) = pull1 == pull2
  (NewIssueModalState newIssue1) == (NewIssueModalState newIssue2) = newIssue1 == newIssue2
  (MergeModalState merge1) == (MergeModalState merge2) = merge1 == merge2
  HelpModalState == HelpModalState = True
  _ == _ = False

-- | Comment modes are compared by what they're commenting on rather than by editor
-- contents (editors have no Eq, and the modal fixer only needs to notice a change of
-- target, not every keystroke).
sameCommentMode :: Maybe CommentMode -> Maybe CommentMode -> Bool
sameCommentMode Nothing Nothing = True
sameCommentMode (Just a) (Just b) =
  issueId (_commentModeIssue a) == issueId (_commentModeIssue b)
  && _commentModeSubmission a == _commentModeSubmission b
sameCommentMode _ _ = False

-- | A zoom modal freshly opened on a node
newZoomModalState :: SomeNode f -> [SomeNode f] -> ModalState f
newZoomModalState node parents = ZoomModalState (ZoomModal node parents Nothing)

-- | A pull request modal freshly opened on the given tab
newPullRequestModalState :: PullModalTab -> Node f 'SinglePullT -> [SomeNode f] -> ModalState f
newPullRequestModalState tab node parents =
  PullRequestModalState (PullModal node parents Nothing tab 0 0 mempty)

-- * Focusing one modal's state
--
-- Each of these has at most one target: it fires when that modal is the one open,
-- and does nothing otherwise. Compose with the field lenses to reach into the modal
-- that's up without having to say what to do about the ones that aren't, e.g.
-- @appModal . _Just . mergeModal . mergeFocus .~ MergeFocusTitle@.

zoomModal :: Traversal' (ModalState f) (ZoomModal f)
zoomModal g (ZoomModalState st) = ZoomModalState <$> g st
zoomModal _ m = pure m

pullModal :: Traversal' (ModalState f) (PullModal f)
pullModal g (PullRequestModalState st) = PullRequestModalState <$> g st
pullModal _ m = pure m

newIssueModal :: Traversal' (ModalState f) NewIssueModal
newIssueModal g (NewIssueModalState st) = NewIssueModalState <$> g st
newIssueModal _ m = pure m

mergeModal :: Traversal' (ModalState f) MergeModal
mergeModal g (MergeModalState st) = MergeModalState <$> g st
mergeModal _ m = pure m

-- | The inline comment editor of whichever modal owns one
modalCommentMode :: ModalState f -> Maybe CommentMode
modalCommentMode (ZoomModalState (ZoomModal {_zoomModalCommentMode})) = _zoomModalCommentMode
modalCommentMode (PullRequestModalState (PullModal {_pullModalCommentMode})) = _pullModalCommentMode
modalCommentMode _ = Nothing

-- | Update the inline comment editor of whichever modal owns one. Each traversal
-- fires only for its own modal, so running both is the same as dispatching on which
-- one is open.
overModalCommentMode :: (Maybe CommentMode -> Maybe CommentMode) -> ModalState f -> ModalState f
overModalCommentMode f =
  (zoomModal %~ \z -> z { _zoomModalCommentMode = f (_zoomModalCommentMode z) })
  . (pullModal %~ \p -> p { _pullModalCommentMode = f (_pullModalCommentMode p) })

data AppState = AppState {
  _appUser :: User
  , _appBaseContext :: BaseContext

  , _appMainUiExtent :: Maybe (Extent ClickableName)

  , _appModalVariable :: TVar (Maybe (ModalState Variable))
  , _appModal :: Maybe (ModalState Fixed)

  , _appForm :: Maybe (Form Text AppEvent ClickableName, Int)

  , _appMainListVariable :: V.Vector (SomeNode Variable)
  , _appMainList :: L.List ClickableName (SomeNode Fixed)

  , _appSortBy :: SortBy
  , _appNow :: UTCTime
  -- | The time used to sort the currently-displayed job list. Navigation re-sorts the
  -- variable tree with this same value so its ordering matches what's on screen.
  , _appSortNow :: UTCTime

  , _appAnimationCounter :: Int

  , _appCliColorMode :: Maybe V.ColorMode
  , _appActualColorMode :: V.ColorMode
  , _appSplitLogs :: Bool

  , _appLogs :: Seq LogEntry

  , _appLogLevelFilter :: LogLevel
  , _appShowStackTraces :: Bool

  , _appDetailsExpanded :: DetailsExpanded

  , _appToasts :: [(ToastLevel, Widget ClickableName, Int)]
  }

data DetailsExpanded = DetailsCollapsed | DetailsExpanded
  deriving (Show, Eq)


makeLenses ''EntityData
makeLenses ''ZoomModal
makeLenses ''PullModal
makeLenses ''NewIssueModal
makeLenses ''MergeModal
makeLenses ''CommentMode
makeLenses ''LogEntry
makeLenses ''AppState
