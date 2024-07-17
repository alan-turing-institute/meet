{-# LANGUAGE TypeApplications #-}

module Main where

import Args (Args (..), getArgs)
import Azure (fetchSchedules, getToken)
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
import Data.Version (showVersion)
import Entities (Days (..), Room (..), allRooms)
import Meetings (chooseBestMeeting, getMeetings)
import PackageInfo_meet (name, version)
import Print (infoPrint, prettyPrint)
import System.Exit (exitSuccess)
import Utils

main :: IO ()
main = do
  args <- getArgs
  when (argsShowVersion args) $ do
    putStrLn $ name <> " version " <> (showVersion version)
    exitSuccess

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

  -- Only count meetings between 8:30 and 17:30 in London
  londonTz <- getCurrentLondonTZ
  let startTime' = localTimeToUTC londonTz $ LocalTime startDate' (TimeOfDay 8 30 0)
  let endDate' = addDays (fromIntegral $ unDays searchSpanDays - 1) startDate'
      endTime' = localTimeToUTC londonTz $ LocalTime endDate' (TimeOfDay 17 30 0)

  let okRooms = filter ((>= inPerson) . capacity) allRooms
  when (null okRooms) $ do
    T.putStrLn "No rooms that meet your criteria were available. :("
    T.putStrLn "Perhaps try reducing the number of people who need to be in-person?"
    exitSuccess

  token <- getToken
  (personSchs, roomSchs) <- fetchSchedules token ppl okRooms startTime' endTime' intervalMinutes
  let goodMeetings = getMeetings personSchs roomSchs inPerson nChunks startTime' intervalMinutes londonTz

  -- Display times in London unless otherwise specified
  displayTz <- if argsShowLocalTime args then getCurrentTimeZone else pure londonTz
  let displayTzText = T.pack $ "UTC" <> timeZoneOffsetString displayTz

  case NE.nonEmpty goodMeetings of
    Nothing -> T.putStrLn "No meetings were available. :("
    Just ms -> do
      if argsFeelingLucky args
        then infoPrint displayTz (chooseBestMeeting ms) inPerson
        else prettyPrint (argsColors args) displayTz goodMeetings
      T.putStrLn $ "All times are in " <> displayTzText <> "."
