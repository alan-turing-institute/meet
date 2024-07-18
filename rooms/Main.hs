{-# LANGUAGE TypeApplications #-}

module Main where

import Args (Args (..), getArgs)
import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Calendar (addDays)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..), getCurrentTimeZone, localTimeToUTC, timeZoneOffsetString)
import Meet.Azure (fetchSchedules, getToken)
import Meet.Entities (Days (..), Minutes (..), Room (..), allRooms, schedule)
import Meet.Meetings (getRoomMeetings)
import Meet.Print (prettyPrint)
import Meet.Utils
import System.Exit (exitSuccess)

main :: IO ()
main = do
  args <- getArgs
  let searchStartDate = argsStartDate args
      searchSpanDays = argsTimespan args
      minCapacity = argsCapacity args
      showInLocalTime = argsShowLocalTime args

  -- Default start date is today but in London
  startDate' <- case searchStartDate of
    Just d -> pure d
    Nothing -> localDay <$> getCurrentLondonTime

  -- Only count meetings between 8:30 and 17:30 in London
  londonTz <- getCurrentLondonTZ
  let startTime' = localTimeToUTC londonTz $ LocalTime startDate' (TimeOfDay 8 30 0)
  let endDate' = addDays (fromIntegral $ unDays searchSpanDays - 1) startDate'
      endTime' = localTimeToUTC londonTz $ LocalTime endDate' (TimeOfDay 17 30 0)

  let okRooms = filter ((>= minCapacity) . capacity) allRooms
  when (null okRooms) $ do
    T.putStrLn "No rooms that meet your criteria were available. :("
    T.putStrLn "Perhaps try reducing the number of people who need to be in-person?"
    exitSuccess

  token <- getToken
  (_, roomSchs) <- fetchSchedules token [] okRooms startTime' endTime' (Minutes 30)
  let totalChunks = case roomSchs of
        [] -> 0
        s : _ -> length (schedule s)
  let goodMeetings = getRoomMeetings roomSchs minCapacity totalChunks startTime' (Minutes 30) londonTz

  -- Display times in London unless otherwise specified
  displayTz <- if showInLocalTime then getCurrentTimeZone else pure londonTz
  let displayTzText = T.pack $ "UTC" <> timeZoneOffsetString displayTz

  case goodMeetings of
    [] -> T.putStrLn "No rooms were available. :("
    _ -> do
      prettyPrint (argsColors args) displayTz goodMeetings
      T.putStrLn $ "All times are in " <> displayTzText <> "."
