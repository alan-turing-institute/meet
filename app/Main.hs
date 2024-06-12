module Main where

import Args
import Azure
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import Data.Time.LocalTime (LocalTime (..))
import Meetings (findRelativeMeetings)
import System.Exit (exitFailure)
import System.IO (hFlush, isEOF, stdout)
import Types (toPerson)
import Utils

main :: IO ()
main = do
  args <- getArgs
  let emailAddrs = emails args
      meetingInterval = interval args
      meetingDuration = duration args
      searchStartDate = startDate args
      searchSpan = timespan args

  startDate' <- case searchStartDate of
    Just d -> pure d
    Nothing -> localDay <$> getCurrentLondonTime

  print startDate'
  error "don't continue"

  chunks <- gracefulDivide meetingDuration meetingInterval -- exits early if duration < interval
  eitherToken <- getToken
  case eitherToken of
    Left err -> print err >> exitFailure
    Right token -> do
      now <- getCurrentTime
      let twoWeeksLater = addUTCTime (secondsToNominalDiffTime 3600 * 24) now
      strings <- getAvailabilityString token emailAddrs now twoWeeksLater meetingInterval
      let people = map toPerson strings
      let relativeMeetings = findRelativeMeetings people chunks
      print relativeMeetings
