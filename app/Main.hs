{-# LANGUAGE GADTs #-}

module Main (main) where

import Brick as B
import Data.Bifunctor
import Data.String.Interpolate
import GitHub
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Draw


app :: App AppState AppEvent ClickableName
app = App {
  appDraw = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent = \event -> get >>= \s -> appEvent s event
  , appStartEvent = return ()
  , appAttrMap = const mainAttrMap
  }

data AppEvent = AppEvent

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

  repos <- github' $ organizationReposR "codedownio" RepoPublicityAll FetchAll
  putStrLn [i|repos: #{second (fmap repoName) repos}|]

  repos <- github' $ userReposR "thomasjm" RepoPublicityAll FetchAll
  putStrLn [i|thomasjm repos: #{second (fmap repoName) repos}|]
