{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- | Faithful copy of the Sauron.UI.SpeedScope server logic (minus the BaseContext
-- logging), used to exercise the real speedscope bundle + profile-serving over HTTP.
-- Run: stack runghc prototype/SpeedScopeServerTest.hs
module Main (main) where

import Control.Monad (forM_, forever, void)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.List qualified as L
import Data.Map.Strict qualified as Map
import Data.String.Interpolate
import Data.Text (Text)
import Data.Text qualified as T
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Exit
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import UnliftIO.Async (async)
import UnliftIO.Directory
import UnliftIO.Exception (finally, handle, SomeException)
import Control.Concurrent (threadDelay)
import System.IO (hSetBuffering, BufferMode(..), stdout)
import Control.Concurrent.STM

speedScopeVersion :: String
speedScopeVersion = "1.25.0"

speedScopeUrl :: String
speedScopeUrl = [i|https://registry.npmjs.org/speedscope/-/speedscope-#{speedScopeVersion}.tgz|]

ensureBundle :: IO (Maybe FilePath)
ensureBundle = handle (\(e :: SomeException) -> putStrLn ("bundle fetch failed: " <> show e) >> pure Nothing) $ do
  cacheDir <- getXdgDirectory XdgCache ("sauron" </> ("speedscope-" <> speedScopeVersion))
  doesFileExist (cacheDir </> "index.html") >>= \case
    True -> pure (Just cacheDir)
    False -> withSystemTempDirectory "sauron-speedscope" $ \tmpDir -> do
      let tarball = tmpDir </> "speedscope.tgz"
      run "curl" ["-sSL", speedScopeUrl, "-o", tarball]
      run "tar" ["xzf", tarball, "-C", tmpDir]
      let releaseDir = tmpDir </> "package" </> "dist" </> "release"
      doesFileExist (releaseDir </> "index.html") >>= \case
        False -> pure Nothing
        True -> do
          createDirectoryIfMissing True cacheDir
          files <- listDirectory releaseDir
          forM_ files $ \f -> copyFile (releaseDir </> f) (cacheDir </> f)
          pure (Just cacheDir)
  where
    run cmd args = readProcessWithExitCode cmd args "" >>= \case
      (ExitSuccess, _, _) -> pure ()
      (code, _, err) -> ioError (userError [i|#{cmd} failed (#{code}): #{err}|])

startServer :: Maybe FilePath -> TVar (Map.Map Text BL.ByteString) -> IO PortNumber
startServer bundleDir profiles = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  listen sock 16
  port <- socketPort sock
  void $ async $ forever $ do
    (conn, _) <- accept sock
    void $ async $ handleRequest bundleDir profiles conn `finally` close conn
  pure port

handleRequest :: Maybe FilePath -> TVar (Map.Map Text BL.ByteString) -> Socket -> IO ()
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
    respond c status ct body = do
      sendAll c $ BS8.pack $ L.intercalate "\r\n" [
        [i|HTTP/1.1 #{status}|], [i|Content-Type: #{ct}|], [i|Content-Length: #{BL.length body}|]
        , "Access-Control-Allow-Origin: *", "Cache-Control: no-store", "Connection: close", "", ""]
      mapM_ (sendAll c) (BL.toChunks body)

    contentType :: FilePath -> String
    contentType file = case takeExtension file of
      ".html" -> "text/html; charset=utf-8"
      ".js" -> "text/javascript"
      ".json" -> "application/json"
      _ -> "application/octet-stream"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  bundleDir <- ensureBundle
  putStrLn ("bundle dir: " <> show bundleDir)
  profiles <- newTVarIO (Map.singleton "test" "{\"profiles\":[],\"name\":\"hi\"}")
  port <- startServer bundleDir profiles
  putStrLn ("PORT=" <> show port)
  forever (threadDelay 1000000)
