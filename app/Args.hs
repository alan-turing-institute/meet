module Args (Args (..), getArgs) where

import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative

data Args = Args
  { emails :: [Text],
    interval :: Int,
    duration :: Int
  } deriving (Eq, Show)

parseArgs :: Parser Args
parseArgs =
  Args
    <$> some (argument readEmail (metavar "EMAILS..." <> help "Email addresses of the people you want to stalk. If you don't include @turing.ac.uk, it will be appended for you."))
    <*> option
      auto
      ( long "interval"
          <> short 'i'
          <> metavar "MINUTES"
          <> help "Granularity of schedule fetched. Defaults to 15 minutes."
          <> value 15
      )
    <*> option
      auto
      ( long "duration"
          <> short 'd'
          <> metavar "MINUTES"
          <> help "Duration of the meeting. Defaults to 60 minutes."
          <> value 60
      )

readEmail :: ReadM Text
readEmail = do
  val <- str
  pure $
    if '@' `T.elem` val
      then val
      else val <> "@turing.ac.uk"

opts :: ParserInfo Args
opts = info (parseArgs <**> helper) (fullDesc <> progDesc "Schedule a meeting with the given emails." <> header "meet - a tool to schedule a meeting")

getArgs :: IO Args
getArgs = execParser opts
