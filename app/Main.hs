{-# LANGUAGE TypeApplications #-}

module Main where

import Args
import Azure (getAvailabilityTextThrow, getTokenThrow)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Text.IO as T
import Data.Time.Calendar (addDays)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..), getCurrentTimeZone, localTimeToUTC)
import Meetings (chooseBestMeeting, getMeetings)
import Print (infoPrint, prettyPrint)
import Types
import Utils

main :: IO ()
main = do
  args <- getArgs
  let emailAddrs = argsEmails args
      intervalMinutes = argsInterval args
      durationMinutes = argsDuration args
      searchStartDate = argsStartDate args
      searchSpanDays = argsTimespan args
      inPerson = argsInPerson args
  nChunks <- gracefulDivide durationMinutes intervalMinutes

  startDate' <- case searchStartDate of
    Just d -> pure d
    Nothing -> localDay <$> getCurrentLocalTime

  localTz <- getCurrentTimeZone
  let startTime' = localTimeToUTC localTz $ LocalTime startDate' (TimeOfDay 8 30 0)
  let endDate' = addDays (fromIntegral $ unDays searchSpanDays - 1) startDate'
      endTime' = localTimeToUTC localTz $ LocalTime endDate' (TimeOfDay 17 30 0)

  let roomIsOk :: (a, Int, b) -> Bool
      roomIsOk (_, cap, _) = cap >= inPerson
  let roomEmailAddrs = M.keys $ M.filter roomIsOk allRooms

  token <- getTokenThrow
  schedules <- getAvailabilityTextThrow token (emailAddrs ++ roomEmailAddrs) startTime' endTime' intervalMinutes
  let goodMeetings = getMeetings schedules inPerson nChunks startTime' intervalMinutes localTz

  case NE.nonEmpty goodMeetings of
    Nothing -> T.putStrLn "No meetings were available. :("
    Just ms ->
      if argsFeelingLucky args
        then infoPrint (chooseBestMeeting ms) inPerson
        else prettyPrint goodMeetings
