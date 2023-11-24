{-# LANGUAGE RankNTypes #-}

module Sauron.Actions (
  openBrowserToUrl

  , modifyOpen
  , modifyToggled

  , withScroll
  ) where

import Brick as B
import Brick.Widgets.List
import Control.Monad.IO.Class
import Data.String.Interpolate
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
