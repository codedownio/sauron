{-# LANGUAGE GADTs #-}

module Main (main) where

import Brick as B
import Brick.BChan
import Brick.Widgets.List
import Control.Concurrent.STM (retry)
import Control.Monad
import Data.Function
import Data.String.Interpolate
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import GitHub
import GitHub.Data.Name
import qualified Graphics.Vty.CrossPlatform as V
import qualified Graphics.Vty as V
import Lens.Micro
import Relude
import Sauron.Auth
import Sauron.Filter
import Sauron.Fix
import Sauron.Options
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Draw
import Sauron.UI.Keys
import System.IO.Error (userError)
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.Exception

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
appEvent s (AppEvent (TreeUpdated newTree)) = do
  put $ s
    & appTree .~ newTree
    & updateFilteredTree

appEvent _s (VtyEvent e) = case e of
  -- Column 3
  V.EvKey c [] | c `elem` [V.KEsc, exitKey] -> do
    -- Cancel everything and wait for cleanups
    -- liftIO $ mapM_ cancelNode (s ^. appRunTreeBase)
    -- forM_ (s ^. appRunTreeBase) (liftIO . waitForTree)
    halt

  ev -> zoom appMainList $ handleListEvent ev
appEvent _ _ = return ()

main :: IO ()
main = do
  args@(CliArgs {..}) <- parseCliArgs

  putStrLn [i|Got args: #{args}|]

  -------------------------------------------------------------

  maybeAuth <- case cliOAuthToken of
    Just t -> pure $ Just $ OAuth (encodeUtf8 t)
    Nothing -> tryDiscoverAuth

  auth <- case maybeAuth of
    Nothing -> throwIO $ userError [i|Couldn't figure out authentication.|]
    Just x -> pure x

  putStrLn [i|Got auth: #{auth}|]

  currentUser@(User {userLogin=(N userLoginUnwrapped)}) <- github auth userInfoCurrentR >>= \case
    Left err -> throwIO $ userError [i|Failed to fetch currently authenticated user: #{err}|]
    Right x -> pure x

  putStrLn [i|currentUser: #{currentUser}|]

  -------------------------------------------------------------

  repos <- case cliConfigFile of
    Nothing -> do
      -- Autodetect repos for user

      -- repos <- github' $ organizationReposR "codedownio" RepoPublicityAll FetchAll
      -- putStrLn [i|repos: #{second (fmap repoName) repos}|]

      (V.toList <$>) $ github auth (userReposR (N userLoginUnwrapped) RepoPublicityAll FetchAll) >>= \case
        Left err -> throwIO $ userError [i|Failed to fetch repos for '#{userLoginUnwrapped}': #{err}|]
        Right x -> return x


    Just configFile -> do
      Yaml.decodeFileEither configFile >>= \case
        Left err -> throwIO $ userError [i|Failed to decode config file '#{configFile}': #{err}|]
        Right (config@(Config {..}) :: Config) -> do
          putStrLn [i|Got config: #{config}|]

          (mconcat <$>) $ forM (fromMaybe [] configSections) $ \(ConfigSection {..}) ->
            forM sectionRepos $ \case
              (ConfigRepoSingle owner name) -> github auth (repositoryR (N owner) (N name)) >>= \case
                Left err -> throwIO $ userError [i|Failed to fetch repo '#{owner}/#{name}': #{err}|]
                Right repo -> pure repo
              (ConfigRepoWildcard owner) -> throwIO $ userError [i|Wildcard repos not supported yet (#{owner}/*)|]

  forM_ repos print

  -------------------------------------------------------------

  let rts = []

  rtsFixed <- atomically $ mapM fixTree rts

  let listElems = [
        (MainListElem {
            repo = r
            , depth = 0
            , toggled = False
            , open = False
            , status = NotStarted
            , ident = 0
            })
        | r <- repos
        ]
        & sortBy (comparing (negate . repoStargazersCount . repo))
        & V.fromList

  let initialState = updateFilteredTree $
        AppState {
          _appTreeBase = rts
          , _appTree = rtsFixed
          , _appUser = currentUser
          , _appMainList = list MainList listElems 1

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
