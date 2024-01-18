{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Sauron.Types where

import qualified Brick.Widgets.List as L
import Control.Concurrent.QSem
import Data.Text
import qualified Data.Vector as V
import GitHub hiding (Status)
import Lens.Micro.TH
import Relude


data SortBy = SortByStars | SortByPushed | SortByUpdated
  deriving (Eq)

-- data NodeWithStatus context s t where
--   NodeOwner :: {
--     nodeInfo :: ()
--     } -> NodeWithStatus context s t
--   NodeRepo :: {
--     nodeInfo :: ()
--     } -> NodeWithStatus context s t

-- data NodeCommonWithStatus s t = NodeCommonWithStatus {
--   commonLabel :: String
--   , commonId :: Int
--   , commonAncestors :: Seq Int
--   , commonToggled :: t
--   , commonOpen :: t
--   , commonStatus :: s
--   , commonVisible :: Bool
--   } deriving (Show, Eq)

-- type NodeCommonFixed = NodeCommonWithStatus Status Bool
-- type NodeCommon = NodeCommonWithStatus (Var Status) (Var Bool)

type Var = TVar

-- type Node context = NodeWithStatus context (Var Status) (Var Bool)
-- type NodeFixed context = NodeWithStatus context Status Bool

data BaseContext = BaseContext {
  requestSemaphore :: QSem
  , auth :: Auth
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

data MainListElem' f = MainListElemHeading {
  _label :: Text
  , _depth :: Int
  , _toggled :: Switchable f Bool
  , _status :: Switchable f (Fetchable ())
  , _ident :: Int
  } | MainListElemRepo {
  _namespaceName :: (Name Owner, Name Repo)
  , _repo :: Switchable f (Fetchable Repo)
  , _workflows :: Switchable f (Fetchable (WithTotalCount WorkflowRun))
  , _depth :: Int
  , _toggled :: Switchable f Bool
  , _ident :: Int
  }
makeLenses ''MainListElem'
type MainListElem = MainListElem' Fixed
deriving instance Eq MainListElem
type MainListElemVariable = MainListElem' Variable

data AppState = AppState {
  _appUser :: User
  , _appBaseContext :: BaseContext
  , _appMainListVariable :: V.Vector MainListElemVariable
  , _appMainList :: L.List ClickableName MainListElem
  , _appSortBy :: SortBy
  }
makeLenses ''AppState
-- type AppState = AppState' Fixed
-- type AppStateVariable = AppState' Variable
-- instance Eq AppState where
--   x == y =
--     _appUser x == _appUser y
--     && L.listElements (_appMainList x) == L.listElements (_appMainList y)
--     && _appSortBy x == _appSortBy y

data AppEvent =
  ListUpdate (V.Vector MainListElem)
