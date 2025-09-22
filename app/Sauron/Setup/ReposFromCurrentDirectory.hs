{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Sauron.Setup.ReposFromCurrentDirectory (
  isContainedInGitRepo
  , reposFromCurrentDirectory
  ) where

import Control.Lens hiding ((:>), (<.>))
import Control.Lens.Regex.Text
import Control.Monad
import Control.Monad.IO.Unlift
import Data.Function
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import GitHub
import GitHub.Data.Name
import Relude hiding (Down)
import Sauron.Actions
import Sauron.HealthCheck
import Sauron.Options
import Sauron.Setup.Common (newRepoNode)
import Sauron.Types
import System.Exit
import System.FilePath
import Text.GitConfig.Parser
import UnliftIO.Directory
import UnliftIO.Process


-- | Determine if the current directory is a git repository.
-- If so, return the owner and repo name.
isContainedInGitRepo :: IO (Maybe (Name Owner, Name Repo))
isContainedInGitRepo = runMaybeT $ do
  cwd <- getCurrentDirectory
  parentDir <- MaybeT $ findParentDir (\candidate -> doesDirectoryExist (candidate </> ".git")) cwd
  let gitDir = parentDir </> ".git"
  putStrLn [i|Found .git dir: #{gitDir}|]

  let gitConfigFile = gitDir </> "config"
  guardM (doesPathExist gitConfigFile)

  gitConfig <- (parseConfig <$> liftIO (T.readFile gitConfigFile)) >>= \case
    Left err -> do
      putStrLn [i|Failed to parse #{gitConfigFile}: #{err}|]
      MaybeT $ pure Nothing
    Right x -> pure x

  (exitCode, sout, serr) <- readCreateProcessWithExitCode (proc "git" ["branch", "--show-current"]) ""
  currentBranch <- case exitCode of
    ExitSuccess -> pure $ T.strip (toText sout)
    ExitFailure n -> do
      putStrLn [i|Couldn't determine current git branch with "git branch --show-current". Exit code #{n}. Stdout: #{sout}. Stderr: #{serr}.|]
      MaybeT $ pure Nothing
  putStrLn [i|Got current git branch: #{currentBranch}|]

  Section _ branchMap <- MaybeT $ pure $ L.find (\(Section initial _) -> initial == ["branch", currentBranch]) gitConfig
  remoteName <- MaybeT $ pure $ HM.lookup "remote" branchMap
  putStrLn [i|Got remote name: #{remoteName}|]

  Section _ remoteMap <- MaybeT $ pure $ L.find (\(Section initial _) -> initial == ["remote", remoteName]) gitConfig
  putStrLn [i|Got remote info: #{remoteMap}|]

  remoteUrl <- MaybeT $ pure $ HM.lookup "url" remoteMap
  putStrLn [i|Got remote url: #{remoteUrl}|]

  -- We can parse an HTTPS remote URL as a URI, but not a git one.
  -- Instead, we use the regexes below.
  -- uri@(URI {..}) <- MaybeT $ pure $ parseURI (toString remoteUrl)
  -- putStrLn [i|Parsed remote URI: #{uri}|]

  case remoteUrl of
    ((^? [regex|https://github.com/([^/]+)/([^.]+).git|] . groups) -> Just [namespace, name]) -> pure (N namespace, N name)
    ((^? [regex|git@github.com:([^/]+)/([^.]+).git|] . groups) -> Just [namespace, name]) -> pure (N namespace, N name)
    _ -> MaybeT $ pure Nothing


-- | Autodetect repos for user
reposFromCurrentDirectory :: BaseContext -> PeriodSpec -> (Name Owner, Name Repo) -> IO (V.Vector (MainListElem' Variable RepoT))
reposFromCurrentDirectory baseContext defaultHealthCheckPeriodUs nsName = do
  repoVar <- newTVarIO NotFetched
  healthCheckVar <- newTVarIO NotFetched
  let period = defaultHealthCheckPeriodUs
  hcThread <- newHealthCheckThread baseContext nsName repoVar healthCheckVar period
  node@(RepoNode (EntityData {..})) <- newRepoNode nsName repoVar healthCheckVar (Just hcThread) 0 (getIdentifier baseContext)

  atomically $ writeTVar _toggled True

  refresh baseContext node ((SomeMainListElem node) :| [])

  return $ V.fromList [node]


-- | Traverse all parent directories starting from a given directory with a callback.
-- | Stops traversing when the callback returns True.
findParentDir :: MonadUnliftIO m => (FilePath -> m Bool) -> FilePath -> m (Maybe FilePath)
findParentDir callback dir = do
  doesDirectoryExist dir >>= \case
    False -> return Nothing
    True -> callback dir >>= \case
      True -> return (Just dir)
      False -> do
        let parent = takeDirectory dir
        if parent /= dir
        then findParentDir callback parent
        else return Nothing
