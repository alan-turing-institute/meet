{-# LANGUAGE TypeApplications #-}

module Main where

import Args
import Azure (getAvailabilityTextThrow, getTokenThrow)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Text.IO as T
import Data.Time.Calendar (addDays)
import Data.Time.Clock (diffUTCTime, nominalDiffTimeToSeconds)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..), getCurrentTimeZone, localTimeToUTC)
import Meetings (chooseBestMeeting, getMeetings)
import Print (infoPrint, prettyPrint)
import Types
import Utils

main :: IO ()
main = do
  args <- getArgs
  let emailAddrs = argsEmails args
      meetingInterval = argsInterval args
      meetingDuration = argsDuration args
      searchStartDate = argsStartDate args
      searchSpan = argsTimespan args
      inPerson = argsInPerson args
  nChunks <- gracefulDivide meetingDuration meetingInterval

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

  token <- getTokenThrow
  schedules <- getAvailabilityTextThrow token (emailAddrs ++ roomEmailAddrs) startTime' endTime' meetingInterval
  let goodMeetings = getMeetings schedules inPerson nChunks startTime' meetingInterval timeListLength localTz

  case NE.nonEmpty goodMeetings of
    Nothing -> T.putStrLn "No meetings were available. :("
    Just ms ->
      if argsFeelingLucky args
        then infoPrint (chooseBestMeeting ms) inPerson
        else prettyPrint goodMeetings
