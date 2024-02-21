{-# LANGUAGE GADTs #-}

module Sauron.Event (appEvent) where

import Brick as B
import Brick.Widgets.List
import Control.Monad
import Control.Monad.IO.Unlift
import Data.Function
import GitHub
import qualified Graphics.Vty as V
import Lens.Micro
import Relude hiding (Down)
import Sauron.Actions
import Sauron.Expanding
import Sauron.Types
import Sauron.UI.Keys
import System.FilePath


appEvent :: AppState -> BrickEvent ClickableName AppEvent -> EventM ClickableName AppState ()
appEvent s (AppEvent (ListUpdate l')) = put $ s
  & appMainList %~ listReplace l' (listSelected $ s ^. appMainList)

appEvent s (VtyEvent e) = case e of
  -- Column 1
  V.EvKey c [] | c == nextKey -> put (s & appMainList %~ (listMoveBy 1))
  V.EvKey c [] | c == previousKey -> put (s & appMainList %~ (listMoveBy (-1)))
  -- V.EvKey c [] | c == nextFailureKey -> do
  --   let ls = Vec.toList $ listElements (s ^. appMainList)
  --   let listToSearch = case listSelectedElement (s ^. appMainList) of
  --         Just (i, MainListElem {}) -> let (front, back) = L.splitAt (i + 1) (zip [0..] ls) in back <> front
  --         Nothing -> zip [0..] ls
  --   case L.find (isFailureStatus . status . snd) listToSearch of
  --     Nothing -> put s
  --     Just (i', _) -> put (s & appMainList %~ (listMoveTo i'))
  -- V.EvKey c [] | c == previousFailureKey -> do
  --   let ls = Vec.toList $ listElements (s ^. appMainList)
  --   let listToSearch = case listSelectedElement (s ^. appMainList) of
  --         Just (i, MainListElem {}) -> let (front, back) = L.splitAt i (zip [0..] ls) in (L.reverse front) <> (L.reverse back)
  --         Nothing -> L.reverse (zip [0..] ls)
  --   case L.find (isFailureStatus . status . snd) listToSearch of
  --     Nothing -> put s
  --     Just (i', _) -> put (s & appMainList %~ (listMoveTo i'))
  V.EvKey c [] | c `elem` toggleKeys -> case listSelectedElement (s ^. appMainList) of
    Nothing -> return ()
    Just (n, _) -> do
      atomically (nthChildVector n (s ^. appMainListVariable)) >>= \case
        Just mle -> do
          isOpen <- liftIO $ atomically $ do
            modifyTVar' (_toggled mle) not
            readTVar (_toggled mle)
          when isOpen (refresh (s ^. appBaseContext) mle)
        _ -> return ()

  -- Scrolling in toggled items
  -- Wanted to make these uniformly Ctrl+whatever, but Ctrl+PageUp/PageDown was causing it to get KEsc and exit (?)
  V.EvKey V.KUp [V.MCtrl] -> withScroll s $ \vp -> vScrollBy vp (-1)
  V.EvKey (V.KChar 'p') [V.MCtrl] -> withScroll s $ \vp -> vScrollBy vp (-1)
  V.EvKey V.KDown [V.MCtrl] -> withScroll s $ \vp -> vScrollBy vp 1
  V.EvKey (V.KChar 'n') [V.MCtrl] -> withScroll s $ \vp -> vScrollBy vp 1
  V.EvKey (V.KChar 'v') [V.MMeta] -> withScroll s $ \vp -> vScrollPage vp Up
  V.EvKey (V.KChar 'v') [V.MCtrl] -> withScroll s $ \vp -> vScrollPage vp Down
  V.EvKey V.KHome [V.MCtrl] -> withScroll s $ \vp -> vScrollToBeginning vp
  V.EvKey V.KEnd [V.MCtrl] -> withScroll s $ \vp -> vScrollToEnd vp

  -- Column 2
  V.EvKey c [] | c == browserToHomeKey ->
    whenRepoSelected s $ \(Repo {repoHtmlUrl=(URL url)}) -> openBrowserToUrl (toString url)
  V.EvKey c [] | c == browserToIssuesKey ->
    whenRepoSelected s $ \(Repo {repoHtmlUrl=(URL url)}) -> openBrowserToUrl (toString url </> "issues")
  V.EvKey c [] | c == browserToPullsKey ->
    whenRepoSelected s $ \(Repo {repoHtmlUrl=(URL url)}) -> openBrowserToUrl (toString url </> "pulls")
  V.EvKey c [] | c == browserToActionsKey ->
    whenRepoSelected s $ \(Repo {repoHtmlUrl=(URL url)}) -> openBrowserToUrl (toString url </> "actions")

  V.EvKey c [] | c == refreshSelectedKey -> do
    whenJust (listSelectedElement (s ^. appMainList)) $ \(n, _el) ->
      atomically (nthChildVector n (s ^. appMainListVariable)) >>= \case
        Nothing -> return ()
        Just el -> refresh (s ^. appBaseContext) el
  V.EvKey c [] | c == refreshAllKey -> do
    liftIO $ runReaderT (refreshAll (s ^. appMainListVariable)) (s ^. appBaseContext)

  -- Column 3
  V.EvKey c [] | c `elem` [V.KEsc, exitKey] -> do
    -- Cancel everything and wait for cleanups
    -- liftIO $ mapM_ cancelNode (s ^. appRunTreeBase)
    -- forM_ (s ^. appRunTreeBase) (liftIO . waitForTree)
    halt

  ev -> zoom appMainList $ handleListEvent ev

-- Mouse events
appEvent _s (MouseDown (ListRow _i) V.BScrollUp _ _) = do
  vScrollBy (viewportScroll MainList) (-1)
appEvent _s (MouseDown (ListRow _i) V.BScrollDown _ _) = do
  vScrollBy (viewportScroll MainList) 1
appEvent s (MouseDown (ListRow i) V.BLeft _ _) = do
  put (s & appMainList %~ (listMoveTo i))

appEvent _ _ = return ()
