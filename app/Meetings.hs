module Meetings where

import Data.List (transpose)
import Data.Time.Clock
import Data.Time.LocalTime (TimeZone, utcToZonedTime)
import Types

data RelativeMeeting = RelativeMeeting
  { startIndex :: Int,
    endIndex :: Int,
    relPeople :: [Entity]
  }
  deriving (Eq, Show)

-- window 2 [1, 2, 3, 4] = [[1, 2], [2, 3], [3, 4]]
window :: Int -> [a] -> [[a]]
window n xs =
  if n > length xs
    then []
    else take n xs : window n (tail xs)

isAllAvailable :: [[Availability]] -> Bool
isAllAvailable window = all (== Free) (concat window)

getAvailabilityWindows :: [Schedule] -> Int -> [[[Availability]]]
getAvailabilityWindows schs n = window n (transpose (map schedule schs))

findRelativeMeetings :: [Schedule] -> Int -> [RelativeMeeting]
findRelativeMeetings schs n =
  let windows = getAvailabilityWindows schs n
      windowAvailabilities = map isAllAvailable windows
      indicesAndAvailabilities = zip [0 ..] windowAvailabilities
      indicesWhereTrue = map fst (filter snd indicesAndAvailabilities)
      es = map entity schs
   in map (makeRelativeMeeting n es) indicesWhereTrue

makeRelativeMeeting :: Int -> [Entity] -> Int -> RelativeMeeting
makeRelativeMeeting n es index =
  RelativeMeeting
    { startIndex = index,
      endIndex = index + n,
      relPeople = es
    }

intSecondsToNDT :: Int -> NominalDiffTime
intSecondsToNDT = secondsToNominalDiffTime . fromIntegral

indicesToTimes :: UTCTime -> Int -> Int -> [UTCTime]
indicesToTimes startTime' interval len =
  let addTime :: Int -> UTCTime
      addTime idx = addUTCTime (intSecondsToNDT $ interval * 60 * idx) startTime'
   in map addTime [0 .. len]

absolutiseMeetings :: [UTCTime] -> TimeZone -> RelativeMeetingWithRooms -> Meeting
absolutiseMeetings times tz rmwr =
  Meeting
    { startTime = utcToZonedTime tz (times !! startIndex (rm rmwr)),
      endTime = utcToZonedTime tz (times !! endIndex (rm rmwr)),
      people = relPeople (rm rmwr),
      rooms = roomsR rmwr
    }

isRoomAvailable :: Int -> Int -> Schedule -> Bool
isRoomAvailable start end sch =
  let period = take (end - start) $ drop start $ schedule sch
   in all (== Free) period

findMeetingRooms :: [Schedule] -> RelativeMeeting -> [Entity]
findMeetingRooms rschedules m =
  let freeSchedules = filter (isRoomAvailable (startIndex m) (endIndex m)) rschedules
   in map entity freeSchedules

data RelativeMeetingWithRooms = RelativeMeetingWithRooms
  { rm :: RelativeMeeting,
    roomsR :: [Entity]
  }

addRoomsToMeeting :: [Schedule] -> RelativeMeeting -> RelativeMeetingWithRooms
addRoomsToMeeting rSchedules rm' = RelativeMeetingWithRooms rm' (findMeetingRooms rSchedules rm')
