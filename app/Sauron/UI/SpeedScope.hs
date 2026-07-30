{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A small in-process HTTP server that hosts the speedscope web app (fetched and cached
-- from npm) and serves profile JSONs pulled from workflow-run artifacts. Modeled on
-- sandwich's TerminalUI speedscope support, but serving one profile per workflow run
-- (keyed by run id) instead of a live test timer.
module Sauron.UI.SpeedScope (
  openSpeedScope
  ) where

import Control.Exception.Safe (throwString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.String.Interpolate
import qualified Data.Text as T
import Network.HTTP.Types.URI (urlEncode)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Relude
import Sauron.Actions.Util (openBrowserToUrl)
import Sauron.Logging
import Sauron.Types
import System.Exit (ExitCode(..))
import System.FilePath
import UnliftIO.Async (async)
import UnliftIO.Directory
import UnliftIO.Exception (finally, handle)
import UnliftIO.MVar (modifyMVar_)
import UnliftIO.Process (readProcessWithExitCode)
import UnliftIO.Temporary (withSystemTempDirectory)


speedScopeVersion :: String
speedScopeVersion = "1.25.0"

speedScopeUrl :: String
speedScopeUrl = [i|https://registry.npmjs.org/speedscope/-/speedscope-#{speedScopeVersion}.tgz|]

-- | Register @profileBytes@ under @profileId@, starting the shared server (and fetching the
-- speedscope bundle) on first use, then open the browser at that profile. Throws on failure;
-- the caller is expected to catch and surface it as a toast.
openSpeedScope :: BaseContext -> MVar (Maybe SpeedScopeServer) -> Text -> Text -> BL.ByteString -> IO ()
openSpeedScope bc serverVar profileId title profileBytes =
  modifyMVar_ serverVar $ \maybeServer -> do
    server <- case maybeServer of
      Just s -> pure s
      Nothing -> do
        bundleDir <- ensureSpeedScopeBundle bc
        s <- startServer bundleDir
        info' bc [i|Serving speedscope on port #{speedScopeServerPort s}|]
        pure s

    atomically $ modifyTVar' (speedScopeServerProfiles server) (Map.insert profileId profileBytes)
    let url = speedScopeUrlFor server profileId title
    info' bc [i|Opening speedscope: #{url}|]
    openBrowserToUrl url
    pure (Just server)

profilePathFor :: Text -> String
profilePathFor profileId = [i|/profiles/#{profileId}.json|]

speedScopeUrlFor :: SpeedScopeServer -> Text -> Text -> String
speedScopeUrlFor server profileId title = case speedScopeServerBundleDir server of
  Just _ -> [i|http://127.0.0.1:#{port}/index.html\#profileURL=#{profPath}&title=#{enc title}|]
  -- No local bundle: hand the profile to speedscope.app. Won't work on Safari, which treats
  -- the http profile URL as mixed content.
  Nothing -> [i|https://www.speedscope.app/\#profileURL=#{enc (toText localUrl)}&title=#{enc title}|]
  where
    port = speedScopeServerPort server
    profPath = profilePathFor profileId
    localUrl = [i|http://127.0.0.1:#{port}#{profPath}|] :: String
    enc t = BS8.unpack (urlEncode True (encodeUtf8 t))

-- * Bundle

ensureSpeedScopeBundle :: BaseContext -> IO (Maybe FilePath)
ensureSpeedScopeBundle bc = handle logAndFallback $ do
  cacheDir <- getXdgDirectory XdgCache ("sauron" </> ("speedscope-" <> speedScopeVersion))
  doesFileExist (cacheDir </> "index.html") >>= \case
    True -> pure (Just cacheDir)
    False -> withSystemTempDirectory "sauron-speedscope" $ \tmpDir -> do
      let tarball = tmpDir </> "speedscope.tgz"
      info' bc [i|Downloading #{speedScopeUrl}|]
      run "curl" ["-sSL", speedScopeUrl, "-o", tarball]
      run "tar" ["xzf", tarball, "-C", tmpDir]

      -- The npm tarball puts the self-contained app in package/dist/release.
      let releaseDir = tmpDir </> "package" </> "dist" </> "release"
      doesFileExist (releaseDir </> "index.html") >>= \case
        False -> do
          info' bc [i|No index.html in the speedscope tarball; falling back to speedscope.app|]
          pure Nothing
        True -> do
          createDirectoryIfMissing True cacheDir
          files <- listDirectory releaseDir
          forM_ files $ \f -> copyFile (releaseDir </> f) (cacheDir </> f)
          info' bc [i|Unpacked speedscope to #{cacheDir}|]
          pure (Just cacheDir)
  where
    logAndFallback (e :: SomeException) = do
      logError' bc [i|Couldn't fetch speedscope (#{e}); falling back to speedscope.app|]
      pure Nothing

    run cmd args = readProcessWithExitCode cmd args "" >>= \case
      (ExitSuccess, _, _) -> pure ()
      (code, _, stderr') -> throwString [i|#{cmd} failed (#{code}): #{stderr'}|]

-- * Server

startServer :: Maybe FilePath -> IO SpeedScopeServer
startServer bundleDir = do
  profiles <- newTVarIO Map.empty
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  listen sock 16
  port <- socketPort sock

  asy <- async $ forever $ do
    (conn, _) <- accept sock
    void $ async $ handleRequest bundleDir profiles conn `finally` close conn

  pure $ SpeedScopeServer {
    speedScopeServerPort = port
    , speedScopeServerBundleDir = bundleDir
    , speedScopeServerProfiles = profiles
    , speedScopeServerAsync = asy
    }

handleRequest :: Maybe FilePath -> TVar (Map Text BL.ByteString) -> Socket -> IO ()
handleRequest bundleDir profiles conn = handle (\(_ :: SomeException) -> pure ()) $
  requestPath conn >>= \case
    Nothing -> respond conn "400 Bad Request" "text/plain" "Bad request"
    Just p -> case parseProfileId p of
      Just pid -> (Map.lookup pid <$> readTVarIO profiles) >>= \case
        Just contents -> respond conn "200 OK" "application/json" contents
        Nothing -> respond conn "404 Not Found" "text/plain" "No such profile"
      Nothing -> case bundleDir of
        Nothing -> respond conn "404 Not Found" "text/plain" "Not found"
        Just dir -> case bundleFile dir p of
          Nothing -> respond conn "404 Not Found" "text/plain" "Not found"
          Just file -> doesFileExist file >>= \case
            False -> respond conn "404 Not Found" "text/plain" "Not found"
            True -> do
              contents <- BL.readFile file
              respond conn "200 OK" (contentType file) contents
  where
    -- @/profiles/<id>.json@ -> Just id
    parseProfileId :: String -> Maybe Text
    parseProfileId path = do
      let clean = T.takeWhile (`notElem` ("?#" :: String)) (T.pack path)
      rest <- T.stripPrefix "/profiles/" clean
      T.stripSuffix ".json" rest

    bundleFile :: FilePath -> String -> Maybe FilePath
    bundleFile dir path = case segments of
      [] -> Just (dir </> "index.html")
      [file] | isValid file, not (isAbsolute file), file /= ".." -> Just (dir </> file)
      _ -> Nothing
      where
        segments = filter (`notElem` ["", ".", "/"]) $ splitDirectories $
          takeWhile (`notElem` ("?#" :: String)) path

    requestPath :: Socket -> IO (Maybe String)
    requestPath c = go mempty
      where
        go acc
          | BS.length acc > 16384 = pure Nothing
          | otherwise = case BS8.lines acc of
              (firstLine:_:_) -> pure $ case BS8.words firstLine of
                (_method:path:_) -> Just (BS8.unpack path)
                _ -> Nothing
              _ -> recv c 4096 >>= \chunk -> if BS.null chunk then pure Nothing else go (acc <> chunk)

    respond :: Socket -> String -> String -> BL.ByteString -> IO ()
    respond c status contentType' body = do
      sendAll c $ BS8.pack $ L.intercalate "\r\n" [
        [i|HTTP/1.1 #{status}|]
        , [i|Content-Type: #{contentType'}|]
        , [i|Content-Length: #{BL.length body}|]
        -- So that speedscope.app can fetch the profile from us, when we're falling back to it.
        , "Access-Control-Allow-Origin: *"
        , "Cache-Control: no-store"
        , "Connection: close"
        , "", ""
        ]
      mapM_ (sendAll c) (BL.toChunks body)

    contentType :: FilePath -> String
    contentType file = case takeExtension file of
      ".html" -> "text/html; charset=utf-8"
      ".js" -> "text/javascript"
      ".css" -> "text/css"
      ".json" -> "application/json"
      ".wasm" -> "application/wasm"
      ".woff2" -> "font/woff2"
      ".png" -> "image/png"
      ".ico" -> "image/x-icon"
      ".txt" -> "text/plain"
      ".md" -> "text/plain"
      _ -> "application/octet-stream"
