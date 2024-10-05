module Main where

import Args (Args (..), getArgs)
import Control.Monad (when)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Calendar (addDays)
import Data.Time.LocalTime
  ( LocalTime (..),
    TimeOfDay (..),
    getCurrentTimeZone,
    localTimeToUTC,
    timeZoneOffsetString,
  )
import Meet.Azure (fetchSchedules)
import Meet.Entities (Days (..), Minutes (..), Room (..), allRooms)
import Meet.Meetings (chooseBestMeeting, getMeetings)
import Meet.Print (infoPrint, prettyPrint, prettyThrow, prettyWarn)
import Meet.Utils
import System.Exit (exitSuccess)

main :: IO ()
main = do
  -- Parse arguments
  args <- getArgs
  let ppl = argsEmails args
      intervalMinutes = argsInterval args
      durationMinutes = argsDuration args
      searchStartDate = argsStartDate args
      searchSpanDays = argsTimespan args
      inPerson = argsInPerson args
  nChunks <- gracefulDivide durationMinutes intervalMinutes

  -- Default start date is today but in London
  startDate' <- case searchStartDate of
    Just d -> pure d
    Nothing -> localDay <$> getCurrentLondonTime

  -- Only consider meetings between 8:30 and 17:30 in London
  londonTz <- getCurrentLondonTZ
  let startTime' = localTimeToUTC londonTz $ LocalTime startDate' (TimeOfDay 8 30 0)
  let endDate' = addDays (fromIntegral $ unDays searchSpanDays - 1) startDate'
      endTime' = localTimeToUTC londonTz $ LocalTime endDate' (TimeOfDay 17 30 0)

  -- Find the rooms which can accommodate the number of in-person attendees
  let okRooms = filter ((>= inPerson) . capacity) allRooms
  when (null okRooms) $ do
    T.putStrLn "No rooms that meet your criteria were available. :("
    T.putStrLn "Perhaps try reducing the number of people who need to be in-person?"
    exitSuccess

  -- Calculate suitable meeting times
  (personSchs, roomSchs) <- fetchSchedules ppl okRooms startTime' endTime' intervalMinutes
  let goodMeetings = getMeetings personSchs roomSchs inPerson nChunks startTime' intervalMinutes londonTz

  -- Determine time zone for output (London unless otherwise specified)
  displayTz <- if argsShowLocalTime args then getCurrentTimeZone else pure londonTz
  let displayTzText = T.pack $ "UTC" <> timeZoneOffsetString displayTz

  -- Print the results
  case NE.nonEmpty goodMeetings of
    Nothing -> T.putStrLn "No meetings were available. :("
    Just ms -> do
      if argsFeelingLucky args
        then infoPrint displayTz (chooseBestMeeting ms) inPerson
        else prettyPrint (argsColors args) displayTz goodMeetings
      T.putStrLn $ "All times are in " <> displayTzText <> "."

-- | `gracefulDivide duration interval` calculates the number of time intervals
-- that fit into the meeting duration.
--
-- If the result is 0, it throws an error. If the result is not an integer, it
-- truncates the duration to the nearest multiple of the interval and warns the
-- user. Otherwise, it returns the number of intervals.
gracefulDivide :: Minutes -> Minutes -> IO Int
gracefulDivide (Minutes duration) (Minutes interval) = do
  case quotRem duration interval of
    (0, _) ->
      prettyThrow $
        T.concat
          [ "Meeting duration of ",
            tshow duration,
            " minutes is shorter than the meeting interval of ",
            tshow interval,
            " minutes."
          ]
    (q, 0) -> pure q
    (q, _) -> do
      prettyWarn $
        T.concat
          [ "Meeting duration of ",
            tshow duration,
            " minutes is not a multiple of the meeting interval of ",
            tshow interval,
            " minutes. Proceeding with a ",
            tshow (q * interval),
            "-minute meeting instead."
          ]
      pure q
