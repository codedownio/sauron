{-# LANGUAGE OverloadedStrings #-}

module Sauron.Fetch.NotificationStates (
  fetchNotificationSubjectStates
  , notificationToQueryItem
  , NotificationQueryItem(..)
  ) where

import Control.Exception.Safe (try)
import Data.Aeson
import Data.Aeson.KeyMap (toMapText)
import Data.Char (isDigit)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import GitHub
import Network.HTTP.Conduit (responseTimeoutMicro)
import Network.HTTP.Simple
import Network.URI (parseURI, uriPath)
import Relude
import Sauron.GraphQL (githubGraphQLEndpoint, GraphQLError(..))
import Sauron.Logging (withLogToModal, logError, LogLevel(..))
import Sauron.Types


data NotificationQueryItem = NotificationQueryItem {
  nqiAlias :: Text
  , nqiOwner :: Text
  , nqiRepo :: Text
  , nqiNumber :: Int
  , nqiSubjectType :: Text
  , nqiStateVar :: TVar NotificationState
  }

notificationToQueryItem :: Text -> TVar NotificationState -> Notification -> Maybe NotificationQueryItem
notificationToQueryItem alias stateVar notification = do
  let Subject {..} = notificationSubject notification
  let RepoRef {repoRefOwner=(SimpleOwner {..}), ..} = notificationRepo notification
  guard (subjectType == "Issue" || subjectType == "PullRequest")
  subUrl <- subjectURL
  num <- extractNumberFromApiUrl (toString $ getUrl subUrl)
  return NotificationQueryItem {
    nqiAlias = alias
    , nqiOwner = untagName simpleOwnerLogin
    , nqiRepo = untagName repoRefRepo
    , nqiNumber = num
    , nqiSubjectType = subjectType
    , nqiStateVar = stateVar
    }
  where
    extractNumberFromApiUrl :: String -> Maybe Int
    extractNumberFromApiUrl url = do
      uri <- parseURI url
      let segments = filter (not . T.null) $ T.splitOn "/" $ toText (uriPath uri)
      case reverse (toList segments) of
        (idStr:_) | T.all isDigit idStr -> readMaybe (toString idStr)
        _ -> Nothing

buildSubjectStatesQuery :: [NotificationQueryItem] -> Text
buildSubjectStatesQuery items = "{ " <> T.intercalate " " (map buildAlias items) <> " }"
  where
    buildAlias NotificationQueryItem {..} =
      nqiAlias <> ": repository(owner: " <> quote nqiOwner <> ", name: " <> quote nqiRepo <> ") { "
      <> case nqiSubjectType of
           "Issue" -> "issue(number: " <> show nqiNumber <> ") { state }"
           _ -> "pullRequest(number: " <> show nqiNumber <> ") { state isDraft }"
      <> " }"
    quote t = "\"" <> t <> "\""

parseSubjectState :: Text -> Value -> Maybe SubjectState
parseSubjectState "Issue" val = do
  obj <- parseObj val
  issueObj <- M.lookup "issue" obj >>= parseObj
  stateStr <- M.lookup "state" issueObj >>= parseStr
  case stateStr of
    "OPEN" -> Just IssueOpen
    "CLOSED" -> Just IssueClosed
    _ -> Nothing
parseSubjectState _ val = do
  obj <- parseObj val
  prObj <- M.lookup "pullRequest" obj >>= parseObj
  stateStr <- M.lookup "state" prObj >>= parseStr
  let isDraft = M.lookup "isDraft" prObj >>= parseBool
  case (isDraft, stateStr) of
    (Just True, _) -> Just PullDraft
    (_, "OPEN") -> Just PullOpen
    (_, "MERGED") -> Just PullMerged
    (_, "CLOSED") -> Just PullClosed
    _ -> Nothing
  where
    parseBool (Bool b) = Just b
    parseBool _ = Nothing

parseObj :: Value -> Maybe (M.Map Text Value)
parseObj (Object km) = Just (toMapText km)
parseObj _ = Nothing

parseStr :: Value -> Maybe Text
parseStr (String t) = Just t
parseStr _ = Nothing

data SubjectStatesResponse = SubjectStatesResponse {
  ssrData :: Maybe (M.Map Text Value)
  , ssrErrors :: Maybe [GraphQLError]
  }
instance FromJSON SubjectStatesResponse where
  parseJSON = withObject "SubjectStatesResponse" $ \o -> SubjectStatesResponse
    <$> (fmap (fmap toMapText) (o .:? "data"))
    <*> o .:? "errors"

fetchNotificationSubjectStates :: (MonadIO m, MonadReader BaseContext m) => [NotificationQueryItem] -> m ()
fetchNotificationSubjectStates items = do
  bc <- ask
  case getAuthToken bc of
    Nothing -> return ()
    Just authToken -> do
      let queryText = buildSubjectStatesQuery items
      let body = object ["query" .= queryText]
      result :: Either SomeException SubjectStatesResponse <-
        withLogToModal bc LevelInfo ("Notifications: fetching subject states for " <> show (length items) <> " items via GraphQL") $
          liftIO $ try $ do
            initialRequest <- parseRequest githubGraphQLEndpoint
            let httpRequest = initialRequest
                            & setRequestBodyJSON body
                            & setRequestResponseTimeout (responseTimeoutMicro (30 * 1000000))
                            & setRequestHeader "User-Agent" ["sauron-app"]
                            & setRequestHeader "Content-Type" ["application/json"]
                            & setRequestHeader "Authorization" ["Bearer " <> encodeUtf8 authToken]
                            & setRequestMethod "POST"
            getResponseBody <$> httpJSON httpRequest
      case result of
        Left ex -> logError ("Notifications: GraphQL request failed: " <> show ex)
        Right (SubjectStatesResponse {ssrErrors=Just errs}) ->
          logError ("Notifications: GraphQL errors: " <> T.intercalate ", " (map message errs))
        Right (SubjectStatesResponse {ssrData=Nothing}) -> return ()
        Right (SubjectStatesResponse {ssrData=Just dataMap}) ->
          forM_ items $ \NotificationQueryItem {..} ->
            case M.lookup nqiAlias dataMap of
              Nothing -> return ()
              Just val -> case parseSubjectState nqiSubjectType val of
                Nothing -> return ()
                Just subjectState -> atomically $ modifyTVar' nqiStateVar $ \ns ->
                  ns { notificationStateSubjectState = Just subjectState }
  where
    getAuthToken :: BaseContext -> Maybe Text
    getAuthToken bc = case auth bc of
      OAuth token -> Just $ decodeUtf8 token
      _ -> Nothing
