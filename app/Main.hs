module Main where

import Args (Args (..), getArgs)
import Azure (fetchSchedules, getToken)
import Config (peopleAndGroups, readConfig)
import Control.Monad (when)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.IO as T
import Data.Time.Calendar (addDays)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..), getCurrentTimeZone, localTimeToUTC)
import Entities (Days (..), Room (..), allRooms)
import Meetings (chooseBestMeeting, getMeetings)
import Print (infoPrint, prettyPrint)
import System.Exit (exitSuccess)
import Utils

main :: IO ()
main = do
  args <- getArgs
  let ppl = argsEmails args
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

  let okRooms = filter ((>= inPerson) . capacity) allRooms
  when (null okRooms) $ do
    T.putStrLn "No rooms that meet your criteria were available. :("
    T.putStrLn "Perhaps try reducing the number of people who need to be in-person?"
    exitSuccess

  config <- readConfig $ argsConfigPath args
  ppl' <- peopleAndGroups config ppl -- Some info printouts are done here.
  token <- getToken
  (personSchs, roomSchs) <- fetchSchedules token ppl' okRooms startTime' endTime' intervalMinutes
  let goodMeetings = getMeetings personSchs roomSchs inPerson nChunks startTime' intervalMinutes localTz

  case NE.nonEmpty goodMeetings of
    Nothing -> T.putStrLn "No meetings were available. :("
    Just ms ->
      if argsFeelingLucky args
        then infoPrint (chooseBestMeeting ms) inPerson
        else prettyPrint goodMeetings
