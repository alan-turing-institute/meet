module Main where

import Args
import Azure
import Data.Time.Clock (addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import Data.Time.LocalTime (LocalTime (..))
import Meetings (absolutiseMeetings, findRelativeMeetings, indicesToTimes)
import System.Exit (exitFailure)
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

  -- TODO: Construct startTime' from startDate'.
  startTime' <- getCurrentTime
  -- TODO: COnstruct endTime' from startDate' and searchSpan.
  let endTime' = addUTCTime (secondsToNominalDiffTime 3600 * 24) startTime'

  chunks <- gracefulDivide meetingDuration meetingInterval -- exits early if duration < interval
  eitherToken <- getToken
  case eitherToken of
    Left err -> print err >> exitFailure
    Right token -> do
      strings <- getAvailabilityString token emailAddrs startTime' endTime' meetingInterval
      let relativeMeetings = findRelativeMeetings (map toPerson strings) chunks
      let timeList = indicesToTimes startTime' meetingInterval (quot (24 * 60) meetingInterval)
      let meetings = map (absolutiseMeetings timeList) relativeMeetings
      print meetings
