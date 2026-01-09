{-# LANGUAGE RankNTypes #-}

module Sauron.Event.Util (
  withScroll
  ) where

import Brick as B
import Brick.Widgets.List
import Data.String.Interpolate
import Lens.Micro
import Relude
import Sauron.Types


withScroll :: AppState -> (forall s. ViewportScroll ClickableName -> EventM n s ()) -> EventM n AppState ()
withScroll s action = do
  case listSelectedElement (s ^. appMainList) of
    Just (_, _el@(SomeNode (getEntityData -> EntityData {..}))) -> action $ viewportScroll (InnerViewport [i|viewport_#{_ident}|])
    _ -> return ()
