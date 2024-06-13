module Args (Args (..), getArgs) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Options.Applicative
import Text.Read (readMaybe)

data Args = Args
  { argsEmails :: [Text],
    argsInterval :: Int,
    argsDuration :: Int,
    argsStartDate :: Maybe Day,
    argsTimespan :: Int,
    argsInPerson :: Int,
    argsFeelingLucky :: Bool
  }
  deriving (Eq, Show)

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
    <*> optional
      ( option
          readDate
          ( long "startDate"
              <> short 's'
              <> metavar "YYYY-MM-DD"
              <> help "First day to start searching for a meeting on"
          )
      )
    <*> option
      auto
      ( long "timespan"
          <> short 't'
          <> metavar "DAYS"
          <> help "Number of days to look ahead when searching for meeting slots"
          <> value 7
      )
    <*> option
      auto
      ( long "people"
          <> short 'p'
          <> metavar "PEOPLE"
          <> help "Number of people who will be attending in person"
          <> value 0
      )
    <*> switch
      ( long "lucky"
          <> short 'l'
          <> help "Suggest the three best meeting times"
      )

readDate :: ReadM Day
readDate = do
  s <- str
  case readMaybe s of
    Just d -> pure d
    Nothing -> error "Date must be specified in YYYY-MM-DD format"

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
