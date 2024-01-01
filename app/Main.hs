{-# LANGUAGE GADTs #-}

module Main (main) where

import Brick as B
import Brick.BChan
import Brick.Widgets.List
import Control.Concurrent.QSem
import Control.Concurrent.STM (retry)
import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Writer
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


refreshPeriod :: Int
refreshPeriod = 100000


app :: App AppState AppEvent ClickableName
app = App {
  appDraw = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent = \event -> get >>= \s -> appEvent s event
  , appStartEvent = return ()
  , appAttrMap = const mainAttrMap
  }

appEvent :: AppState -> BrickEvent ClickableName AppEvent -> EventM ClickableName AppState ()
appEvent _s (AppEvent (FullUpdate s')) = do
  put s'

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
  V.EvKey c [] | c == browserToHomeKey ->
    whenRepoSelected s $ \(Repo {repoHtmlUrl=(URL url)}) -> openBrowserToUrl (toString url)
  V.EvKey c [] | c == browserToIssuesKey ->
    whenRepoSelected s $ \(Repo {repoHtmlUrl=(URL url)}) -> openBrowserToUrl (toString url </> "issues")
  V.EvKey c [] | c == browserToPullsKey ->
    whenRepoSelected s $ \(Repo {repoHtmlUrl=(URL url)}) -> openBrowserToUrl (toString url </> "pulls")
  V.EvKey c [] | c == browserToActionsKey ->
    whenRepoSelected s $ \(Repo {repoHtmlUrl=(URL url)}) -> openBrowserToUrl (toString url </> "actions")

  V.EvKey c [] | c == refreshSelectedKey -> do
    whenJust (listSelectedElement (s ^. appMainList)) $ \(_i, el) -> case el of
      MainListElemRepo {_repo} -> liftIO $ runReaderT (refreshSelected _repo) (s ^. appBaseContext)
      MainListElemHeading {} -> return () -- TODO
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

  listElems :: V.Vector MainListElemVariable <- case cliConfigFile of
    Nothing -> do
      -- Autodetect repos for user

      -- repos <- github' $ organizationReposR "codedownio" RepoPublicityAll FetchAll
      -- putStrLn [i|repos: #{second (fmap repoName) repos}|]

      repos <- withGithubApiSemaphore' githubApiSemaphore (github auth (userReposR (N userLoginUnwrapped) RepoPublicityAll FetchAll)) >>= \case
        Left err -> throwIO $ userError [i|Failed to fetch repos for '#{userLoginUnwrapped}': #{err}|]
        Right x -> return x

      (V.fromList <$>) $ forM (V.toList repos) $ \r -> do
        workflowsVar <- newTVarIO Nothing
        statusVar <- newTVarIO Ready
        return $ MainListElemRepo {
          _repo = r,
          _workflows = workflowsVar,
          _depth = 0,
          _toggled = False,
          _status = statusVar,
          _ident = 0
          }

    Just configFile -> do
      Yaml.decodeFileEither configFile >>= \case
        Left err -> throwIO $ userError [i|Failed to decode config file '#{configFile}': #{err}|]
        Right (config@(Config {..}) :: Config) -> do
          putStrLn [i|Got config: #{config}|]

          (V.fromList <$>) $ execWriterT $ forM (fromMaybe [] configSections) $ \(ConfigSection {..}) -> do
            repoDepth <- case sectionDisplayName of
              Nothing -> pure 0
              Just l -> do
                statusVar <- newTVarIO Ready
                tell [MainListElemHeading {
                  _label = l
                  , _depth = 0
                  , _toggled = True
                  , _status = statusVar
                  , _ident = 0
                  }]
                pure 1

            repos <- lift $ forConcurrently sectionRepos $ \r ->
              withGithubApiSemaphore' githubApiSemaphore $ case r of
                (ConfigRepoSingle owner name) -> github auth (repositoryR (N owner) (N name)) >>= \case
                  Left err -> throwIO $ userError [i|Failed to fetch repo '#{owner}/#{name}': #{err}|]
                  Right r' -> pure r'
                (ConfigRepoWildcard owner) -> throwIO $ userError [i|Wildcard repos not supported yet (#{owner}/*)|]

            forM_ repos $ \r -> do
              workflowsVar <- newTVarIO Nothing
              statusVar <- newTVarIO Ready
              tell [MainListElemRepo {
                _repo = r
                , _workflows = workflowsVar
                , _depth = repoDepth
                , _toggled = False
                , _status = statusVar
                , _ident = 0
                }]

  -------------------------------------------------------------

  listElemsFixed :: V.Vector MainListElem <- atomically $ mapM fixMainListElem listElems

  let initialState =
        AppState {
          _appUser = currentUser
          , _appBaseContext = BaseContext {
              requestSemaphore = githubApiSemaphore
              , auth = auth
              }
          , _appMainListVariable = listElems
          , _appMainList = list MainList listElemsFixed 1

          , _appSortBy = SortByStars
        }


  eventChan <- newBChan 10

  listElemsVar <- newTVarIO listElemsFixed

  eventAsync <- async $
    forever $ do
      handleAny (\e -> putStrLn [i|Got exception in event async: #{e}|] >> threadDelay refreshPeriod) $ do
        newFixed <- atomically $ do
          currentFixed <- readTVar listElemsVar
          newFixed <- mapM fixMainListElem listElems
          when (newFixed == currentFixed) retry

          writeTVar listElemsVar newFixed
          return newFixed
        writeBChan eventChan (ListUpdate newFixed)
        threadDelay refreshPeriod

  let buildVty = do
        v <- V.mkVty V.defaultConfig
        let output = V.outputIface v
        when (V.supportsMode output V.Mouse) $
          V.setMode output V.Mouse True
        return v
  initialVty <- buildVty
  flip onException (cancel eventAsync) $
    void $ customMain initialVty buildVty (Just eventChan) app initialState


fixMainListElem :: MainListElemVariable -> STM MainListElem
fixMainListElem (MainListElemHeading {..}) = do
  statusFixed <- readTVar _status

  return $ MainListElemHeading {
    _label = _label
    , _depth = _depth
    , _toggled = _toggled
    , _status = statusFixed
    , _ident = _ident
    }
fixMainListElem (MainListElemRepo {..}) = do
  workflowsFixed <- readTVar _workflows
  statusFixed <- readTVar _status

  return $ MainListElemRepo {
    _repo = _repo
    , _workflows = workflowsFixed
    , _depth = _depth
    , _toggled = _toggled
    , _status = statusFixed
    , _ident = _ident
    }
