{-# LANGUAGE TypeApplications #-}

module Main where

import Args (Args (..), getArgs)
import Azure (fetchSchedules, getToken)
import Control.Monad (when)
import qualified Data.Text.IO as T
import Data.Time.Calendar (addDays)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..), getCurrentTimeZone, localTimeToUTC)
import Entities (Days (..), Minutes (..), Room (..), schedule, allRooms)
import Meetings (getRoomMeetings)
import Print (prettyPrint)
import System.Exit (exitSuccess)
import Utils

main :: IO ()
main = do
  args <- getArgs
  let searchStartDate = argsStartDate args
      searchSpanDays = argsTimespan args
      minCapacity = argsCapacity args

  startDate' <- case searchStartDate of
    Just d -> pure d
    Nothing -> localDay <$> getCurrentLocalTime

  localTz <- getCurrentTimeZone
  let startTime' = localTimeToUTC localTz $ LocalTime startDate' (TimeOfDay 8 30 0)
  let endDate' = addDays (fromIntegral $ unDays searchSpanDays - 1) startDate'
      endTime' = localTimeToUTC localTz $ LocalTime endDate' (TimeOfDay 17 30 0)

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
  let goodMeetings = getRoomMeetings roomSchs minCapacity totalChunks startTime' (Minutes 30) localTz

  case goodMeetings of
    [] -> T.putStrLn "No meetings were available. :("
    _ -> prettyPrint goodMeetings
