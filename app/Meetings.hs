module Meetings (chooseBestMeeting, getMeetings) where

import Data.Foldable1 (maximumBy)
import Data.List (findIndices, partition, transpose)
import Data.List.NonEmpty (NonEmpty)
import Data.Time.Clock (NominalDiffTime, UTCTime (..), addUTCTime, secondsToNominalDiffTime)
import Data.Time.LocalTime (TimeZone, utcToZonedTime, zonedTimeToUTC)
import Types

data RelativeMeeting = RelativeMeeting
  { startIndex :: Int,
    endIndex :: Int,
    relPeople :: [Entity]
  }
  deriving (Eq, Show)

-- makeWindows 2 [1, 2, 3, 4] = [[1, 2], [2, 3], [3, 4]]
-- makeWindows 3 [1, 2, 3, 4] = [[1, 2, 3], [2, 3, 4]]
makeWindows :: Int -> [a] -> [[a]]
makeWindows n xs =
  if n > length xs
    then []
    else take n xs : makeWindows n (drop 1 xs)

isAllAvailable :: [[Availability]] -> Bool
isAllAvailable = all (all (== Free))

getAvailabilityWindows :: [Schedule] -> Int -> [[[Availability]]]
getAvailabilityWindows schs n = makeWindows n (transpose (map schedule schs))

makeRelativeMeeting :: Int -> [Entity] -> Int -> RelativeMeeting
makeRelativeMeeting n es index =
  RelativeMeeting
    { startIndex = index,
      endIndex = index + n,
      relPeople = es
    }

findRelativeMeetings :: [Schedule] -> Int -> [RelativeMeeting]
findRelativeMeetings schs n =
  let windows = getAvailabilityWindows schs n
      indicesWhereTrue = findIndices isAllAvailable windows
      es = map entity schs
   in map (makeRelativeMeeting n es) indicesWhereTrue

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
  all (== Free) . take (end - start) . drop start $ schedule sch

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

-- | Ranks the 'quality' of a meeting, with lower numbers being better.
meetingScore :: Meeting -> Int
meetingScore m = case length (people m) of
  0 -> 0
  l -> case rooms m of
    [] -> 10000
    rs -> minimum $ map (roomSizeDiff l) rs
      where
        -- How far away a room's capacity is from the number of people who need it
        roomSizeDiff :: Int -> Entity -> Int
        roomSizeDiff n r = abs (capacity r - n)

-- | Compare two meetings. The meeting with the lower score is placed first.
-- Ties are broken by start time (i.e. earlier is better).
compareMeeting :: Meeting -> Meeting -> Ordering
compareMeeting m1 m2 = case meetingScore m1 `compare` meetingScore m2 of
  EQ -> zonedTimeToUTC (startTime m1) `compare` zonedTimeToUTC (startTime m2)
  x -> x

chooseBestMeeting :: NonEmpty Meeting -> Meeting
chooseBestMeeting ms = maximumBy compareMeeting ms

getMeetings :: [Schedule] -> Int -> Int -> UTCTime -> Int -> Int -> TimeZone -> [Meeting]
getMeetings schedules inPerson nChunks startTime' meetingInterval timeListLength localTz =
  let (personSchedules, roomSchedules) = partition isPerson schedules
      relativeMeetings = findRelativeMeetings personSchedules nChunks
      relativeMeetingsWithRooms = map (addRoomsToMeeting roomSchedules) relativeMeetings
      timeList = indicesToTimes startTime' meetingInterval timeListLength
      meetings = map (absolutiseMeetings timeList localTz) relativeMeetingsWithRooms
   in filter (isMeetingGood inPerson) meetings
