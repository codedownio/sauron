{-# LANGUAGE RankNTypes #-}

module Sauron.Actions (
  openBrowserToUrl

  , withScroll

  , refreshSelected
  , refreshAll

  , withGithubApiSemaphore
  , withGithubApiSemaphore'

  , whenRepoSelected
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

withScroll :: AppState -> (forall s. ViewportScroll ClickableName -> EventM n s ()) -> EventM n AppState ()
withScroll s action = do
  case listSelectedElement (s ^. appMainList) of
    Just (_, MainListElemRepo {..}) -> do
      let scroll = viewportScroll (InnerViewport [i|viewport_#{_ident}|])
      action scroll
    _ -> return ()

refreshSelected :: (MonadReader BaseContext m, MonadIO m, MonadMask m) => Repo -> m ()
refreshSelected (Repo {..}) = do
  BaseContext {auth} <- ask
  withGithubApiSemaphore (liftIO $ github auth (workflowRunsR (simpleOwnerLogin repoOwner) repoName mempty (FetchAtLeast 10))) >>= \case
    Left err -> putStrLn [i|Got err: #{err}|]
    Right x -> putStrLn [i|Got ret: #{x}|]

refreshAll :: (MonadIO m) => m ()
refreshAll = undefined

withGithubApiSemaphore :: (MonadReader BaseContext m, MonadIO m, MonadMask m) => m a -> m a
withGithubApiSemaphore action = do
  sem <- asks requestSemaphore
  withGithubApiSemaphore' sem action

withGithubApiSemaphore' :: (MonadIO m, MonadMask m) => QSem -> m a -> m a
withGithubApiSemaphore' sem = bracket_ (liftIO $ waitQSem sem) (liftIO $ signalQSem sem)

whenRepoSelected :: Monad f => AppState -> (Repo -> f ()) -> f ()
whenRepoSelected s cb = whenJust (listSelectedElement (s ^. appMainList)) $ \(_i, el) -> case el of
  MainListElemRepo {_repo} -> cb _repo
  MainListElemHeading {} -> return ()
