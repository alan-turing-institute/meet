module Args (Args (..), getArgs) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Word (Word8)
import Entities (Days (..))
import Options.Applicative
import Text.Read (readMaybe)

data Args = Args
  { argsStartDate :: Maybe Day,
    argsTimespan :: Days,
    argsCapacity :: Int,
    argsShowLocalTime :: Bool,
    argsColors :: NonEmpty Word8,
    argsShowVersion :: Bool
  }
  deriving (Eq, Show)

parseArgs :: Parser Args
parseArgs =
  Args
    <$> optional
      ( option
          readDate
          ( long "startDate"
              <> short 's'
              <> metavar "YYYY-MM-DD"
              <> help "First day to start searching for a meeting room on."
          )
      )
    <*> option
      (Days <$> auto)
      ( long "timespan"
          <> short 't'
          <> metavar "DAYS"
          <> help "Number of days to look ahead when searching for meeting slots. Defaults to 1, i.e., search only on the start date."
          <> value (Days 1)
      )
    <*> option
      auto
      ( long "capacity"
          <> short 'c'
          <> metavar "PEOPLE"
          <> help "Minimum capacity needed for the meeting room. Defaults to 0."
          <> value 0
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
          <> value (NE.fromList [35, 128])
      )
    <*> switch
      ( long "version"
          <> help "Display version number and exit."
      )

readDate :: ReadM Day
readDate = do
  s <- str
  case readMaybe s of
    Just d -> pure d
    Nothing -> error "Date must be specified in YYYY-MM-DD format"

readColors :: ReadM (NonEmpty Word8)
readColors = do
  val <- str
  pure $ case val of
    "none" -> NE.fromList [0]
    _ -> NE.fromList $ map (read . T.unpack) $ T.splitOn "," val

opts :: ParserInfo Args
opts = info (parseArgs <**> helper) (fullDesc <> progDesc "Find a meeting room" <> header "meet-rooms - find a meeting room for a pre-existing meeting")

getArgs :: IO Args
getArgs = execParser opts
