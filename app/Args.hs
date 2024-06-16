module Args (Days (..), Minutes (..), Args (..), getArgs) where

import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Entities (Person (..))
import Options.Applicative
import Text.Read (readMaybe)

newtype Minutes = Minutes {unMinutes :: Int}
  deriving (Eq, Show)

newtype Days = Days {unDays :: Int}
  deriving (Eq, Show)

data Args = Args
  { argsEmails :: [Person],
    argsInterval :: Minutes,
    argsDuration :: Minutes,
    argsStartDate :: Maybe Day,
    argsTimespan :: Days,
    argsInPerson :: Int,
    argsFeelingLucky :: Bool
  }
  deriving (Eq, Show)

parseArgs :: Parser Args
parseArgs =
  Args
    <$> some (argument readPerson (metavar "EMAILS..." <> help "Email addresses of the people you want to stalk. If you don't include @turing.ac.uk, it will be appended for you."))
    <*> option
      (Minutes <$> auto)
      ( long "interval"
          <> short 'i'
          <> metavar "MINUTES"
          <> help "Granularity of schedule fetched. Defaults to 15 minutes."
          <> value (Minutes 15)
      )
    <*> option
      (Minutes <$> auto)
      ( long "duration"
          <> short 'd'
          <> metavar "MINUTES"
          <> help "Duration of the meeting. Defaults to 60 minutes."
          <> value (Minutes 60)
      )
    <*> optional
      ( option
          readDate
          ( long "startDate"
              <> short 's'
              <> metavar "YYYY-MM-DD"
              <> help "First day to start searching for a meeting on."
          )
      )
    <*> option
      (Days <$> auto)
      ( long "timespan"
          <> short 't'
          <> metavar "DAYS"
          <> help "Number of days to look ahead when searching for meeting slots. Defaults to a week."
          <> value (Days 7)
      )
    <*> option
      auto
      ( long "people"
          <> short 'p'
          <> metavar "PEOPLE"
          <> help "Number of people who will be attending in person."
          <> value 0
      )
    <*> switch
      ( long "lucky"
          <> short 'l'
          <> help "Make the app suggest a single best meeting time (and room if needed)."
      )

readDate :: ReadM Day
readDate = do
  s <- str
  case readMaybe s of
    Just d -> pure d
    Nothing -> error "Date must be specified in YYYY-MM-DD format"

readPerson :: ReadM Person
readPerson = do
  val <- str
  pure $
    Person $
      if '@' `T.elem` val
        then val
        else val <> "@turing.ac.uk"

opts :: ParserInfo Args
opts = info (parseArgs <**> helper) (fullDesc <> progDesc "Schedule a meeting with the given emails." <> header "meet - a tool to schedule a meeting")

getArgs :: IO Args
getArgs = execParser opts
