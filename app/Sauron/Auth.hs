
module Sauron.Auth (
  tryDiscoverAuth
  ) where

import Control.Monad.IO.Class
import Data.Text as T
import GitHub.Auth
import Relude
import System.Exit
import System.Process
import UnliftIO.Directory


tryDiscoverAuth :: MonadIO m => m (Maybe Auth)
tryDiscoverAuth = usingGhToken


usingGhToken :: MonadIO m => m (Maybe Auth)
usingGhToken = runMaybeT $ do
  gh <- MaybeT $ findExecutable "gh"
  liftIO (readCreateProcessWithExitCode (proc gh ["auth", "token"]) "") >>= \case
    (ExitSuccess, sout, _) -> pure $ OAuth $ encodeUtf8 $ T.strip (fromString sout)
    _ -> MaybeT $ return Nothing
