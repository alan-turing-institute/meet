{-# LANGUAGE TypeApplications #-}

module Main where

import Args
import Azure
import Control.Monad (when)
import Data.List (partition)
import qualified Data.Map as M
import qualified Data.Text.IO as T
import Data.Time.Calendar (addDays)
import Data.Time.Clock (diffUTCTime, nominalDiffTimeToSeconds)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..), getCurrentTimeZone, localTimeToUTC)
import Meetings
import Print (infoPrint, prettyPrint)
import System.Exit (exitFailure)
import Types
import Utils

partitionEither' :: [(a, Either b c)] -> ([(a, c)], [(a, b)])
partitionEither' = foldr f ([], [])
  where
    f (a, e) (successes, failures) = case e of
      Left failure -> (successes, (a, failure) : failures)
      Right success -> ((a, success) : successes, failures)

main :: IO ()
main = do
  args <- getArgs
  let emailAddrs = argsEmails args
      meetingInterval = argsInterval args
      meetingDuration = argsDuration args
      searchStartDate = argsStartDate args
      searchSpan = argsTimespan args
      inPerson = argsInPerson args

  startDate' <- case searchStartDate of
    Just d -> pure d
    Nothing -> localDay <$> getCurrentLocalTime

  localTz <- getCurrentTimeZone
  let startTime' = localTimeToUTC localTz $ LocalTime startDate' (TimeOfDay 8 30 0)
  let endDate' = addDays (toInteger searchSpan - 1) startDate'
      endTime' = localTimeToUTC localTz $ LocalTime endDate' (TimeOfDay 17 30 0)
      timeListSeconds = floor @Double . realToFrac . nominalDiffTimeToSeconds $ diffUTCTime endTime' startTime'
  let timeListLength = quot (timeListSeconds `quot` 60) meetingInterval

  let roomIsOk :: (a, Int, b) -> Bool
      roomIsOk (_, cap, _) = cap >= inPerson
  let roomEmailAddrs = M.keys $ M.filter roomIsOk allRooms

  nChunks <- gracefulDivide meetingDuration meetingInterval -- exits early if duration < interval
  eitherToken <- getToken
  case eitherToken of
    Left err -> print err >> exitFailure
    Right token -> do
      strings <- getAvailabilityText token (emailAddrs ++ roomEmailAddrs) startTime' endTime' meetingInterval
      let (successStrings, failureStrings) = partitionEither' strings
      when (not $ null failureStrings) $ do
        T.putStrLn "Failed to get availability for the following email addresses. Check if they exist:"
        mapM_ (\(email, message) -> T.putStrLn $ " - " <> email <> " (Message: " <> message <> ")") failureStrings
        exitFailure
      let schedules = map toSchedule successStrings
          (personSchedules, roomSchedules) = partition isPerson schedules
          relativeMeetings = findRelativeMeetings personSchedules nChunks
          relativeMeetingsWithRooms = map (addRoomsToMeeting roomSchedules) relativeMeetings
          timeList = indicesToTimes startTime' meetingInterval timeListLength
          meetings = map (absolutiseMeetings timeList localTz) relativeMeetingsWithRooms
          goodMeetings = filter (isMeetingGood inPerson) meetings -- enforce working hours :)
      if argsFeelingLucky args
        then infoPrint (take 3 $ chooseTopMeetings goodMeetings) inPerson
        else prettyPrint goodMeetings
