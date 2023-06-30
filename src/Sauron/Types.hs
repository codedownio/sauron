{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Sauron.Types where

import qualified Brick.Widgets.List as L
import Control.Concurrent.QSem
import Control.Concurrent.STM (TVar)
import Data.Text
import GitHub ()
import Lens.Micro.TH

data SortBy = SortByStars | SortByPushed | SortByUpdated

data NodeWithStatus context s t where
  NodeOwner :: {
    nodeInfo :: ()
    } -> NodeWithStatus context s t
  NodeRepo :: {
    nodeInfo :: ()
    } -> NodeWithStatus context s t

type Var = TVar

data Status = NotStarted | Running | Done () | Failure ()

type Node context = NodeWithStatus context (Var Status) (Var Bool)
type NodeFixed context = NodeWithStatus context Status Bool

data BaseContext = BaseContext {
  requestSemaphore :: QSem
  }

data ClickableName = ListRow Int | MainList | InnerViewport Text | InfoBar
  deriving (Show, Ord, Eq)

data MainListElem = MainListElem {
  label :: String
  , depth :: Int
  , toggled :: Bool
  , open :: Bool
  , status :: Status
  , ident :: Int
  }

data AppState = AppState {
  _appTreeBase :: [Node BaseContext]
  , _appTree :: [NodeFixed BaseContext]
  , _appMainList :: L.List ClickableName MainListElem

  , _appSortBy :: SortBy
  }
makeLenses ''AppState
