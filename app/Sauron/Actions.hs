{-# LANGUAGE RankNTypes #-}

module Sauron.Actions (
  openBrowserToUrl

  , modifyOpen
  , modifyToggled

  , withScroll

  , refreshSelected
  , refreshAll

  , withGithubApiSemaphore
  , withGithubApiSemaphore'
  ) where

import Brick as B
import Brick.Widgets.List
import Control.Concurrent.QSem
import Control.Exception.Safe (bracket_)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Data.String.Interpolate
import GitHub
import Lens.Micro
import Relude
import Sauron.Types
import UnliftIO.Process


openBrowserToUrl :: MonadIO m => String -> m ()
openBrowserToUrl url =
  void $ readCreateProcessWithExitCode (proc "xdg-open" [url]) ""


modifyToggled :: Monad m => AppState -> (Bool -> Bool) -> m ()
modifyToggled s f = case listSelectedElement (s ^. appMainList) of
  Nothing -> return ()
  Just (_i, MainListElem {..}) -> do
    -- liftIO $ atomically $ modifyTVar' (runTreeToggled node) f
    return ()

modifyOpen :: Monad m => AppState -> (Bool -> Bool) -> m ()
modifyOpen s f = case listSelectedElement (s ^. appMainList) of
  Nothing -> return ()
  Just (_i, MainListElem {..}) -> do
    -- liftIO $ atomically $ modifyTVar' (runTreeOpen node) f
    return ()

withScroll :: AppState -> (forall s. ViewportScroll ClickableName -> EventM n s ()) -> EventM n AppState ()
withScroll s action = do
  case listSelectedElement (s ^. appMainList) of
    Nothing -> return ()
    Just (_, MainListElem {..}) -> do
      let scroll = viewportScroll (InnerViewport [i|viewport_#{ident}|])
      action scroll

refreshSelected :: (MonadReader BaseContext m, MonadIO m, MonadMask m) => Repo -> m ()
refreshSelected (Repo {..}) = do
  BaseContext {auth} <- ask
  withGithubApiSemaphore (liftIO $ github auth (workflowRunsR (simpleOwnerLogin repoOwner) repoName mempty (FetchAtLeast 10))) >>= \case
    Left err -> putStrLn [i|Got err: #{err}|]
    Right x -> putStrLn [i|Got ret: #{x}|]

refreshAll :: MonadIO m => m ()
refreshAll = undefined

withGithubApiSemaphore :: (MonadReader BaseContext m, MonadIO m, MonadMask m) => m a -> m a
withGithubApiSemaphore action = do
  sem <- asks requestSemaphore
  withGithubApiSemaphore' sem action

withGithubApiSemaphore' :: (MonadIO m, MonadMask m) => QSem -> m a -> m a
withGithubApiSemaphore' sem = bracket_ (liftIO $ waitQSem sem) (liftIO $ signalQSem sem)
