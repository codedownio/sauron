{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Sauron.Types where

import qualified Brick.Widgets.List as L
import Control.Concurrent.QSem
import Data.Sequence
import Data.Text
import GitHub (Auth, Repo, User, WithTotalCount, WorkflowRun)
import Lens.Micro.TH
import Relude

data SortBy = SortByStars | SortByPushed | SortByUpdated

data NodeWithStatus context s t where
  NodeOwner :: {
    nodeInfo :: ()
    } -> NodeWithStatus context s t
  NodeRepo :: {
    nodeInfo :: ()
    } -> NodeWithStatus context s t

data NodeCommonWithStatus s t = NodeCommonWithStatus {
  commonLabel :: String
  , commonId :: Int
  , commonAncestors :: Seq Int
  , commonToggled :: t
  , commonOpen :: t
  , commonStatus :: s
  , commonVisible :: Bool
  } deriving (Show, Eq)

type NodeCommonFixed = NodeCommonWithStatus Status Bool
type NodeCommon = NodeCommonWithStatus (Var Status) (Var Bool)

type Var = TVar

data Status = Fetching | Ready
  deriving (Show, Eq)

type Node context = NodeWithStatus context (Var Status) (Var Bool)
type NodeFixed context = NodeWithStatus context Status Bool

data BaseContext = BaseContext {
  requestSemaphore :: QSem
  , auth :: Auth
  }

data ClickableName = ListRow Int | MainList | InnerViewport Text | InfoBar
  deriving (Show, Ord, Eq)

data MainListElem = MainListElemHeading {
  _label :: Text
  , _depth :: Int
  , _toggled :: Bool
  , _status :: Status
  , _ident :: Int
  } | MainListElemRepo {
  _repo :: Repo
  , _workflows :: Maybe (WithTotalCount WorkflowRun)
  , _depth :: Int
  , _toggled :: Bool
  , _status :: Status
  , _ident :: Int
  }

makeLenses ''MainListElem

data AppState = AppState {
  _appUser :: User
  , _appBaseContext :: BaseContext

  , _appMainList :: L.List ClickableName MainListElem

  , _appSortBy :: SortBy
  }
makeLenses ''AppState

newtype AppEvent = TreeUpdated [NodeFixed BaseContext]
