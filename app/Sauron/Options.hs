
module Sauron.Options where

import Data.Text
import Options.Applicative
import Relude


data CliArgs = CliArgs {
  cliOAuthToken :: Maybe Text
  } deriving (Show)

cliArgsParser :: Parser CliArgs
cliArgsParser = CliArgs
  <$> optional (strOption (long "token" <> help "OAuth token to auth to GitHub" <> metavar "STRING"))

parseCliArgs :: IO CliArgs
parseCliArgs = execParser opts
  where
    opts = info (cliArgsParser <**> helper) (
      fullDesc
      <> progDesc "Print a greeting for TARGET"
      <> header "hello - a test for optparse-applicative"
      )
