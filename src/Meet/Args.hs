-- | Shared argument parsing behaviour
module Meet.Args (localSwitch, colorFlag, startDateFlag) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Word (Word8)
import Options.Applicative
import Text.Read (readMaybe)

localSwitch :: Parser Bool
localSwitch =
  switch
    ( long "local"
        <> help "Display meeting times in your local timezone. By default, times are shown in London time."
    )

colorFlag :: Parser (Maybe (NonEmpty Word8))
colorFlag =
  option
    readColors
    ( long "color"
        <> short 'c'
        <> metavar "COLOR"
        <> help "Colours to be used for output table formatting. Colours here refer to 256-colour terminal colours, and are specified as a comma-separated list of integers between 0 and 255 (inclusive). Pass a value of 'none' to remove colours. Defaults to '35,128', which is green and purple."
        <> value (Just $ NE.fromList [35, 128])
    )

startDateFlag :: Parser (Maybe Day)
startDateFlag =
  optional
    ( option
        readDate
        ( long "startDate"
            <> short 's'
            <> metavar "YYYY-MM-DD"
            <> help "First day to start searching for a meeting on."
        )
    )

readDate :: ReadM Day
readDate = do
  s <- str
  case readMaybe s of
    Just d -> pure d
    Nothing -> error "Date must be specified in YYYY-MM-DD format"

readColors :: ReadM (Maybe (NonEmpty Word8))
readColors = do
  val <- str
  pure $ case val of
    "none" -> Nothing
    _ -> Just $ NE.fromList $ map (read . T.unpack) $ T.splitOn "," val
