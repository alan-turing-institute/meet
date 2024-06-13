module Main where

import Args
import Azure
import Data.Time.Clock (addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import Data.Time.LocalTime (LocalTime (..), getCurrentTimeZone)
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
    Nothing -> localDay <$> getCurrentLocalTime

  -- TODO: Construct startTime' from startDate'.
  -- Proposal: ALWAYS start at 9am on startDate', to make the maths simpler. If
  -- run at 2pm, this will suggest meetings that are in the past. However, we
  -- can filter this out.
  startTime' <- getCurrentTime
  -- TODO: Construct endTime' from startDate' and searchSpan.
  -- Proposal: ALWAYS end at 5pm on endDate', to make the maths simpler.
  let endTime' = addUTCTime (secondsToNominalDiffTime 3600 * 24) startTime'
  -- TODO: Calculate this correctly based on (endTime' - startTime').
  let timeListLength = quot (24 * 60) meetingInterval

  localTz <- getCurrentTimeZone
  chunks <- gracefulDivide meetingDuration meetingInterval -- exits early if duration < interval

  eitherToken <- getToken
  case eitherToken of
    Left err -> print err >> exitFailure
    Right token -> do
      strings <- getAvailabilityString token emailAddrs startTime' endTime' meetingInterval
      let relativeMeetings = findRelativeMeetings (map toPerson strings) chunks
      let timeList = indicesToTimes startTime' meetingInterval timeListLength
      let meetings = map (absolutiseMeetings timeList localTz) relativeMeetings
      print meetings
