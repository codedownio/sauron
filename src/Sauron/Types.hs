{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Sauron.Types where

import qualified Brick.Widgets.List as L
import Control.Concurrent.QSem
import Data.Sequence
import Data.Text
import GitHub (Repo, User)
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

data Status = NotStarted | Running | Done () | Failure ()
  deriving (Show, Eq)

type Node context = NodeWithStatus context (Var Status) (Var Bool)
type NodeFixed context = NodeWithStatus context Status Bool

data BaseContext = BaseContext {
  requestSemaphore :: QSem
  }

data ClickableName = ListRow Int | MainList | InnerViewport Text | InfoBar
  deriving (Show, Ord, Eq)

data MainListElem = MainListElem {
  repo :: Repo
  , depth :: Int
  , toggled :: Bool
  , open :: Bool
  , status :: Status
  , ident :: Int
  }

data AppState = AppState {
  _appTreeBase :: [Node BaseContext]
  , _appTree :: [NodeFixed BaseContext]
  , _appUser :: User
  , _appMainList :: L.List ClickableName MainListElem

  , _appSortBy :: SortBy
  }
makeLenses ''AppState

newtype AppEvent = TreeUpdated [NodeFixed BaseContext]

getCommons :: NodeWithStatus context s t -> [NodeCommonWithStatus s t]
getCommons = undefined -- extractValues runNodeCommon
