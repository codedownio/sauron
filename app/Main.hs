{-# LANGUAGE GADTs #-}

module Main (main) where

import Brick as B
import Brick.BChan
import Brick.Widgets.List
import Control.Concurrent.STM (retry)
import Control.Monad
import Data.Bifunctor
import Data.String.Interpolate
import GitHub
import qualified Graphics.Vty as V
import Sauron.Filter
import Sauron.Fix
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Draw
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.Exception
import UnliftIO.STM

-- import Data.Time


terminalUIRefreshPeriod :: Int
terminalUIRefreshPeriod = 100000

app :: App AppState AppEvent ClickableName
app = App {
  appDraw = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent = \event -> get >>= \s -> appEvent s event
  , appStartEvent = return ()
  , appAttrMap = const mainAttrMap
  }

appEvent :: AppState -> BrickEvent ClickableName AppEvent -> EventM ClickableName AppState ()
-- appEvent s (AppEvent (RunTreeUpdated newTree)) = do
--   now <- liftIO getCurrentTime
--   continue $ s
--     & appRunTree .~ newTree
--     & appTimeSinceStart .~ (diffUTCTime now (s ^. appStartTime))
--     & updateFilteredTree
appEvent _ _ = return ()

main :: IO ()
main = do
  user <- github' userInfoForR "thomasjm"
  putStrLn [i|user: #{user}|]

  -- repos <- github' $ organizationReposR "codedownio" RepoPublicityAll FetchAll
  -- putStrLn [i|repos: #{second (fmap repoName) repos}|]

  repos <- github' $ userReposR "thomasjm" RepoPublicityAll FetchAll
  putStrLn [i|thomasjm repos: #{second (fmap repoName) repos}|]

  -- startTime <- getCurrentTime

  -- setInitialFolding terminalUIInitialFolding rts

  let rts = []

  rtsFixed <- atomically $ mapM fixTree rts

  let initialState = updateFilteredTree $
        AppState {
          _appTreeBase = rts
          , _appTree = rtsFixed
          , _appMainList = list MainList mempty 1

          , _appSortBy = SortByStars
        }

  eventChan <- newBChan 10

  currentFixedTree <- newTVarIO rtsFixed
  eventAsync <- async $
    forever $ do
      handleAny (\e -> putStrLn [i|Got exception in event async: #{e}|] >> threadDelay terminalUIRefreshPeriod) $ do
        newFixedTree <- atomically $ do
          currentFixed <- readTVar currentFixedTree
          newFixed <- mapM fixTree rts
          when (fmap getCommons newFixed == fmap getCommons currentFixed) retry
          writeTVar currentFixedTree newFixed
          return newFixed
        writeBChan eventChan (TreeUpdated newFixedTree)
        threadDelay terminalUIRefreshPeriod

  let buildVty = do
        v <- V.mkVty V.defaultConfig
        let output = V.outputIface v
        when (V.supportsMode output V.Mouse) $
          V.setMode output V.Mouse True
        return v
  initialVty <- buildVty
  flip onException (cancel eventAsync) $
    void $ customMain initialVty buildVty (Just eventChan) app initialState
