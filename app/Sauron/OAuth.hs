{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Sauron.OAuth (
  authenticateWithGitHub
  , saveToken
  , loadSavedToken
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.String.Interpolate
import Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import GitHub.Auth
import Network.HTTP.Client
import Network.HTTP.Conduit (tlsManagerSettings)
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Relude hiding (ByteString, lookupEnv)
import System.FilePath ((</>))
import qualified System.IO as SIO
import System.IO.Error (userError)
import UnliftIO.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import UnliftIO.Environment (lookupEnv)


githubCliClientId :: Text
githubCliClientId = "Ov23ctc5CLqumglgHaeZ"

data DeviceCodeResponse = DeviceCodeResponse {
  device_code :: Text
  , user_code :: Text
  , verification_uri :: Text
  , expires_in :: Int
  , interval :: Int
  } deriving (Show, Generic)

instance FromJSON DeviceCodeResponse where
  parseJSON = withObject "DeviceCodeResponse" $ \o -> DeviceCodeResponse
    <$> o .: "device_code"
    <*> o .: "user_code"
    <*> o .: "verification_uri"
    <*> o .: "expires_in"
    <*> o .: "interval"

data TokenResponse = TokenResponse {
  access_token :: Text
  , token_type :: Text
  , scope :: Text
  } deriving (Show, Generic)

instance FromJSON TokenResponse where
  parseJSON = withObject "TokenResponse" $ \o -> TokenResponse
    <$> o .: "access_token"
    <*> o .: "token_type"
    <*> o .: "scope"

data TokenError = TokenError {
  error :: Text
  , error_description :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON TokenError where
  parseJSON = withObject "TokenError" $ \o -> TokenError
    <$> o .: "error"
    <*> o .:? "error_description"

authenticateWithGitHub :: IO Auth
authenticateWithGitHub = do
  putStrLn "\nAuthenticating with GitHub...\n"

  manager <- newManager tlsManagerSettings

  -- Step 1: Request device code
  deviceResp <- requestDeviceCode manager

  -- Step 2: Show user the code and URL
  putStrLn [i|Please visit: #{verification_uri deviceResp}|]
  putStrLn [i|Enter code: #{user_code deviceResp}\n|]

  configDir <- getConfigDir
  putStrLn [__i|This will ask for full access to GitHub.
                Don't worry, it just generates an OAuth token that will be stored locally in #{configDir}.
                (This is also how tools like "gh auth" work.)\n|]

  putStr "Waiting for authentication..."
  SIO.hFlush stdout

  -- Step 3: Poll for access token
  token <- pollForToken manager deviceResp

  putStrLn " ✓"
  putStrLn "Successfully authenticated with GitHub!"

  saveToken token

  return $ OAuth (TE.encodeUtf8 token)

requestDeviceCode :: Manager -> IO DeviceCodeResponse
requestDeviceCode manager = do
  initReq <- parseRequest "POST https://github.com/login/device/code"
  let req = initReq {
        requestHeaders = [
            (hAccept, "application/json")
            , (hContentType, "application/json")
            , (hUserAgent, "sauron")
            ]
        , requestBody = RequestBodyLBS $ encode $ object [
            "client_id" .= githubCliClientId
            , "scope" .= ("repo user" :: Text)
            ]
        }

  response <- httpLbs req manager

  case responseStatus response of
    status | status == status200 -> do
      case eitherDecode (responseBody response) of
        Left err -> throwIO $ userError [i|Failed to parse device code response: #{err}|]
        Right deviceResp -> return deviceResp
    _ -> throwIO $ userError [i|Failed to request device code: #{responseStatus response}|]

pollForToken :: Manager -> DeviceCodeResponse -> IO Text
pollForToken manager deviceResp = do
  let pollInterval = interval deviceResp * 1_000_000

  let loop = do
        threadDelay pollInterval

        initReq <- parseRequest "POST https://github.com/login/oauth/access_token"
        let req = initReq {
              requestHeaders = [
                  (hAccept, "application/json")
                  , (hContentType, "application/json")
                  , (hUserAgent, "sauron")
                  ]
              , requestBody = RequestBodyLBS $ encode $ object [
                  "client_id" .= githubCliClientId
                  , "device_code" .= device_code deviceResp
                  , "grant_type" .= ("urn:ietf:params:oauth:grant-type:device_code" :: Text)
                  ]
              }

        response <- httpLbs req manager

        case responseStatus response of
          status | status == status200 -> do
            -- Try parsing as error first (GitHub returns 200 for authorization_pending, etc.)
            case eitherDecode (responseBody response) of
              Right (TokenError errorCode _) ->
                case errorCode of
                  "authorization_pending" -> loop -- Keep polling
                  "slow_down" -> do
                    threadDelay (pollInterval * 2) -- Slow down
                    loop
                  "access_denied" -> throwIO $ userError "GitHub authentication was denied"
                  "expired_token" -> throwIO $ userError "GitHub device code expired, please try again"
                  other -> throwIO $ userError [i|GitHub authentication error: #{other}|]
              Left _ -> do
                -- If not an error, try parsing as success token response
                case eitherDecode (responseBody response) of
                  Left err -> throwIO $ userError [i|Failed to parse token response: #{err}|]
                  Right (tokenResp :: TokenResponse) -> return $ access_token tokenResp
          _ -> do
            -- Non-200 status - check if it's an expected error
            case eitherDecode (responseBody response) of
              Right (TokenError errorCode _) ->
                case errorCode of
                  "authorization_pending" -> loop -- Keep polling
                  "slow_down" -> do
                    threadDelay (pollInterval * 2) -- Slow down
                    loop
                  "access_denied" -> throwIO $ userError "GitHub authentication was denied"
                  "expired_token" -> throwIO $ userError "GitHub device code expired, please try again"
                  other -> throwIO $ userError [i|GitHub authentication error: #{other}|]
              Left _ -> throwIO $ userError $ [i|Failed to get access token #{responseStatus response}|]

  loop

getConfigDir :: MonadIO m => m FilePath
getConfigDir = lookupEnv "XDG_CONFIG_HOME" >>= \case
  Just xdgConfig -> return $ xdgConfig </> "sauron"
  Nothing -> do
    homeDir <- getHomeDirectory
    return $ homeDir </> ".config" </> "sauron"

getTokenPath :: MonadIO m => m FilePath
getTokenPath = (</> "token") <$> getConfigDir

saveToken :: MonadIO m => Text -> m ()
saveToken token = do
  tokenPath <- getTokenPath
  configDir <- getConfigDir
  createDirectoryIfMissing True configDir
  liftIO $ T.writeFile tokenPath token
  putStrLn [i|Token saved to #{tokenPath}|]

loadSavedToken :: MonadIO m => m (Maybe Auth)
loadSavedToken = do
  tokenPath <- getTokenPath
  doesFileExist tokenPath >>= \case
    False -> return Nothing
    True -> do
      tokenText <- liftIO $ T.readFile tokenPath
      return $ Just $ OAuth (encodeUtf8 $ T.strip tokenText)
