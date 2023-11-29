{-# LANGUAGE GADTs #-}

module Main (main) where

import Brick as B
import Brick.BChan
import Brick.Widgets.List
import Control.Concurrent.QSem
import Control.Concurrent.STM (retry)
import Control.Monad
import Control.Monad.IO.Unlift
import Data.Function
import Data.String.Interpolate
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import GitHub
import GitHub.Data.Name
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V
import Lens.Micro
import Relude hiding (Down)
import Sauron.Actions
import Sauron.Auth
import Sauron.Filter
import Sauron.Fix
import Sauron.Options
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Draw
import Sauron.UI.Keys
import System.FilePath
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
  V.EvKey c [] | c `elem` toggleKeys ->
    put $ s
        & over appMainList (listModify (over toggled not))

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
  V.EvKey c [] | c == browserToHomeKey -> do
    whenJust (listSelectedElement (s ^. appMainList)) $ \(_i, MainListElem {_repo=(Repo {repoHtmlUrl=(URL url)})}) ->
      openBrowserToUrl (toString url)
  V.EvKey c [] | c == browserToIssuesKey -> do
    whenJust (listSelectedElement (s ^. appMainList)) $ \(_i, MainListElem {_repo=(Repo {repoHtmlUrl=(URL url)})}) ->
      openBrowserToUrl (toString url </> "issues")
  V.EvKey c [] | c == browserToPullsKey -> do
    whenJust (listSelectedElement (s ^. appMainList)) $ \(_i, MainListElem {_repo=(Repo {repoHtmlUrl=(URL url)})}) ->
      openBrowserToUrl (toString url </> "pulls")
  V.EvKey c [] | c == browserToActionsKey -> do
    whenJust (listSelectedElement (s ^. appMainList)) $ \(_i, MainListElem {_repo=(Repo {repoHtmlUrl=(URL url)})}) ->
      openBrowserToUrl (toString url </> "actions")

  V.EvKey c [] | c == refreshSelectedKey -> do
    whenJust (listSelectedElement (s ^. appMainList)) $ \(_i, MainListElem {_repo}) ->
      liftIO $ flip runReaderT (s ^. appBaseContext) (refreshSelected _repo)
  V.EvKey c [] | c == refreshAllKey -> do
    refreshAll

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

  githubApiSemaphore <- newQSem cliConcurrentGithubApiLimit

  -------------------------------------------------------------

  maybeAuth <- case cliOAuthToken of
    Just t -> pure $ Just $ OAuth (encodeUtf8 t)
    Nothing -> tryDiscoverAuth

  auth <- case maybeAuth of
    Nothing -> throwIO $ userError [i|Couldn't figure out authentication.|]
    Just x -> pure x

  putStrLn [i|Got auth: #{auth}|]

  currentUser@(User {userLogin=(N userLoginUnwrapped)}) <- withGithubApiSemaphore' githubApiSemaphore (github auth userInfoCurrentR) >>= \case
    Left err -> throwIO $ userError [i|Failed to fetch currently authenticated user: #{err}|]
    Right x -> pure x

  putStrLn [i|currentUser: #{currentUser}|]

  -------------------------------------------------------------

  repos <- case cliConfigFile of
    Nothing -> do
      -- Autodetect repos for user

      -- repos <- github' $ organizationReposR "codedownio" RepoPublicityAll FetchAll
      -- putStrLn [i|repos: #{second (fmap repoName) repos}|]

      (V.toList <$>) $ withGithubApiSemaphore' githubApiSemaphore (github auth (userReposR (N userLoginUnwrapped) RepoPublicityAll FetchAll)) >>= \case
        Left err -> throwIO $ userError [i|Failed to fetch repos for '#{userLoginUnwrapped}': #{err}|]
        Right x -> return x


    Just configFile -> do
      Yaml.decodeFileEither configFile >>= \case
        Left err -> throwIO $ userError [i|Failed to decode config file '#{configFile}': #{err}|]
        Right (config@(Config {..}) :: Config) -> do
          putStrLn [i|Got config: #{config}|]

          (mconcat <$>) $ forM (fromMaybe [] configSections) $ \(ConfigSection {..}) ->
            forConcurrently sectionRepos $ \repo ->
              withGithubApiSemaphore' githubApiSemaphore $ case repo of
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
            _repo = r
            , _depth = 0
            , _toggled = False
            , _status = NotStarted
            , _ident = 0
            })
        | r <- repos
        ]
        & sortBy (comparing (negate . repoStargazersCount . _repo))
        & V.fromList

  let initialState = updateFilteredTree $
        AppState {
          _appTreeBase = rts
          , _appTree = rtsFixed
          , _appUser = currentUser
          , _appBaseContext = BaseContext {
              requestSemaphore = githubApiSemaphore
              , auth = auth
              }
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
