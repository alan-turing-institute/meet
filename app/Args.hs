module Args (Args (..), getArgs) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Version (showVersion)
import Data.Word (Word8)
import Entities (Days (..), Minutes (..), Person (..))
import Options.Applicative
import PackageInfo_meet (name, version)
import Text.Read (readMaybe)

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
    <*> switch
      ( long "local"
          <> help "Display meeting times in your local timezone. By default, times are shown in London time."
      )
    <*> option
      readColors
      ( long "color"
          <> short 'c'
          <> metavar "COLOR"
          <> help "Colours to be used for output table formatting. Colours here refer to 256-colour terminal colours, and are specified as a comma-separated list of integers between 0 and 255 (inclusive). Pass a value of 'none' to remove colours. Defaults to '35,128', which is green and purple."
          <> value (Just $ NE.fromList [35, 128])
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

readColors :: ReadM (Maybe (NonEmpty Word8))
readColors = do
  val <- str
  pure $ case val of
    "none" -> Nothing
    _ -> Just $ NE.fromList $ map (read . T.unpack) $ T.splitOn "," val

opts :: ParserInfo Args
opts =
  info
    (parseArgs <**> helper <**> simpleVersioner (name ++ " version " ++ showVersion version))
    (fullDesc <> progDesc "Schedule a meeting with the given emails." <> header (name ++ " - a tool to schedule a meeting"))

getArgs :: IO Args
getArgs = execParser opts
