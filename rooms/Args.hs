module Args (Args (..), getArgs) where

import Data.List.NonEmpty (NonEmpty)
import Data.Time.Calendar (Day)
import Data.Version (showVersion)
import Data.Word (Word8)
import Meet.Args (colorFlag, localSwitch, startDateFlag)
import Meet.Entities (Days (..))
import Options.Applicative
import Paths_meet (version)

data Args = Args
  { argsStartDate :: Maybe Day,
    argsTimespan :: Days,
    argsCapacity :: Int,
    argsShowLocalTime :: Bool,
    argsColors :: Maybe (NonEmpty Word8)
  }
  deriving (Eq, Show)

parseArgs :: Parser Args
parseArgs =
  Args
    <$> startDateFlag
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
    <*> localSwitch
    <*> colorFlag

opts :: ParserInfo Args
opts =
  info
    (parseArgs <**> helper <**> simpleVersioner ("meet-rooms version " ++ showVersion version))
    (fullDesc <> progDesc "Find a meeting room" <> header ("meet-rooms - find a meeting room for a pre-existing meeting"))

getArgs :: IO Args
getArgs = execParser opts
