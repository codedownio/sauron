{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Sauron.Options (
  Command(..)
  , CliArgs(..)
  , parseCommand

  , RepoSettings(..)

  , PeriodSpec(..)

  , Config(..)
  , ConfigNode(..)
  ) where

import Data.Aeson as A
import Data.Aeson.TH
import Data.String.Interpolate
import qualified Graphics.Vty as V
import Options.Applicative
import Relude
import Sauron.Aeson


-- * CLI

data Command
  = RunApp CliArgs
  | InitConfig { initConfigWrite :: Bool }
  deriving (Show)

data CliArgs = CliArgs {
  cliOAuthToken :: Maybe Text
  , cliConfigFile :: Maybe FilePath
  , cliConcurrentGithubApiLimit :: Int
  , cliDebugFile :: Maybe FilePath
  , cliForceAuth :: Bool
  , cliShowAllRepos :: Bool
  , cliColorMode :: Maybe V.ColorMode
  , cliSplitLogs :: Bool
  } deriving (Show)

cliArgsParser :: Parser CliArgs
cliArgsParser = CliArgs
  <$> optional (strOption (long "token" <> help "OAuth token to auth to GitHub" <> metavar "STRING"))
  <*> optional (strOption (long "config" <> short 'c' <> help "Config file path" <> metavar "STRING"))
  <*> option auto (long "github-concurrent-requests" <> short 'r' <> showDefault <> help "Maximum number of concurrent requests to GitHub" <> value 10 <> metavar "INT")
  <*> optional (strOption (long "debug-file" <> help "Debug file path (for optional logging)" <> metavar "STRING"))
  <*> switch (long "auth" <> help "Force OAuth authentication flow")
  <*> switch (long "all" <> help "Show all repositories for the authenticated user")
  <*> optional (option (maybeReader parseColorMode) (long "color-mode" <> help "Force a specific color mode (full, 240, 16, 8, none)" <> metavar "MODE"))
  <*> switch (long "split-logs" <> help "Split terminal view: app on left, logs on right")

initConfigParser :: Parser Command
initConfigParser = InitConfig
  <$> switch (long "write" <> short 'w' <> help "Write the sample config to the default config location")

commandParser :: Parser Command
commandParser = subparser (
  command "init-config" (info (initConfigParser <**> helper) (progDesc "Print a sample config file to stdout"))
  ) <|> (RunApp <$> cliArgsParser)

parseCommand :: IO Command
parseCommand = execParser opts
  where
    opts = info (commandParser <**> helper) (
      fullDesc
      <> progDesc "A dark lord's-eye view of your GitHub repos"
      <> header "hello - a test for optparse-applicative"
      )

-- * Color mode

parseColorMode :: String -> Maybe V.ColorMode
parseColorMode "full" = Just V.FullColor
parseColorMode "240" = Just (V.ColorMode240 240)
parseColorMode "16" = Just V.ColorMode16
parseColorMode "8" = Just V.ColorMode8
parseColorMode "none" = Just V.NoColor
parseColorMode _ = Nothing

-- * Config file

newtype PeriodSpec = PeriodSpec Int
  deriving (Show, Eq)
instance FromJSON PeriodSpec where
  parseJSON (A.Number n) = pure (PeriodSpec (round n))
  -- TODO: parse k8s-style time specs like 5m
  parseJSON x = fail [i|Can't parse period spec: #{x}|]

data RepoSettings = RepoSettings {
  repoSettingsCheckPeriod :: Maybe PeriodSpec
  } deriving (Show, Eq)
$(deriveFromJSON toSnake2 ''RepoSettings)

data ConfigNode
  = ConfigNodeNotifications { configNodeNotificationsOpen :: Maybe Bool }
  | ConfigNodeIssues { configNodeIssuesDisplayName :: Text, configNodeIssuesQuery :: Text, configNodeIssuesOpen :: Maybe Bool }
  | ConfigNodePulls { configNodePullsDisplayName :: Text, configNodePullsQuery :: Text, configNodePullsOpen :: Maybe Bool }
  | ConfigNodeHeading { configNodeHeadingDisplayName :: Text, configNodeHeadingChildren :: [ConfigNode], configNodeHeadingOpen :: Maybe Bool }
  | ConfigNodeRepo { configNodeRepoName :: Text, configNodeRepoSettings :: Maybe RepoSettings, configNodeRepoOpen :: Maybe Bool }
  deriving (Show, Eq)

instance FromJSON ConfigNode where
  parseJSON (A.String s) = pure $ ConfigNodeRepo s Nothing Nothing
  parseJSON o = flip (withObject "ConfigNode") o $ \obj -> do
    typ <- obj .:? "type" .!= ("repo" :: Text)
    case typ of
      "notifications" -> ConfigNodeNotifications
        <$> obj .:? "open"
      "issues" -> ConfigNodeIssues
        <$> obj .: "display_name"
        <*> obj .: "query"
        <*> obj .:? "open"
      "pulls" -> ConfigNodePulls
        <$> obj .: "display_name"
        <*> obj .: "query"
        <*> obj .:? "open"
      "heading" -> ConfigNodeHeading
        <$> obj .: "display_name"
        <*> obj .: "children"
        <*> obj .:? "open"
      "repo" -> ConfigNodeRepo
        <$> obj .: "name"
        <*> obj .:? "settings"
        <*> obj .:? "open"
      other -> fail [i|Unknown node type: "#{other}"|]

data Config = Config {
  configSettings :: Maybe RepoSettings
  , configNodes :: Maybe [ConfigNode]
  } deriving (Show)
$(deriveFromJSON toSnake1 ''Config)
