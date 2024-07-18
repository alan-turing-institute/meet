module Args (Args (..), getArgs) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Version (showVersion)
import Data.Word (Word8)
import Meet.Args (colorFlag, localSwitch, startDateFlag)
import Meet.Entities (Days (..), Minutes (..), Person (..))
import Options.Applicative
import Paths_meet (version)

data Args = Args
  { argsEmails :: [Person],
    argsInterval :: Minutes,
    argsDuration :: Minutes,
    argsStartDate :: Maybe Day,
    argsTimespan :: Days,
    argsInPerson :: Int,
    argsFeelingLucky :: Bool,
    argsShowLocalTime :: Bool,
    argsColors :: Maybe (NonEmpty Word8)
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
    <*> startDateFlag
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
    <*> localSwitch
    <*> colorFlag

readPerson :: ReadM Person
readPerson = do
  val <- str
  pure $
    Person $
      if '@' `T.elem` val
        then val
        else val <> "@turing.ac.uk"

opts :: ParserInfo Args
opts =
  info
    (parseArgs <**> helper <**> simpleVersioner ("meet version " ++ showVersion version))
    (fullDesc <> progDesc "Schedule a meeting with the given emails." <> header ("meet - a tool to schedule a meeting"))

getArgs :: IO Args
getArgs = execParser opts
