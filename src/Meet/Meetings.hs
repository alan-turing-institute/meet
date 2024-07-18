module Meet.Meetings (Meeting (..), chooseBestMeeting, getMeetings, getRoomMeetings) where

import Data.Foldable1 (maximumBy)
import Data.List (findIndices, transpose)
import Data.List.NonEmpty (NonEmpty)
import Data.Time.Calendar (DayOfWeek (..), dayOfWeek)
import Data.Time.Clock
  ( NominalDiffTime,
    UTCTime (..),
    addUTCTime,
    secondsToNominalDiffTime,
  )
import Data.Time.LocalTime
  ( TimeOfDay (..),
    TimeZone,
    ZonedTime (..),
    localDay,
    localTimeOfDay,
    utcToZonedTime,
    zonedTimeToLocalTime,
    zonedTimeToUTC,
  )
import Meet.Entities (Availability (..), Minutes (..), Person (..), Room (..), Schedule (..))

data RelativeMeeting = RelativeMeeting
  { startIndex :: Int,
    endIndex :: Int,
    relPeople :: [Person]
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

getAvailabilityWindows :: [Schedule Person] -> Int -> [[[Availability]]]
getAvailabilityWindows schs n = makeWindows n (transpose (map schedule schs))

makeRelativeMeeting :: Int -> [Person] -> Int -> RelativeMeeting
makeRelativeMeeting n ppl index =
  RelativeMeeting
    { startIndex = index,
      endIndex = index + n,
      relPeople = ppl
    }

findRelativeMeetings :: [Schedule Person] -> Int -> [RelativeMeeting]
findRelativeMeetings schs n =
  let windows = getAvailabilityWindows schs n
      indicesWhereTrue = findIndices isAllAvailable windows
      ppl = map entity schs
   in map (makeRelativeMeeting n ppl) indicesWhereTrue

intSecondsToNDT :: Int -> NominalDiffTime
intSecondsToNDT = secondsToNominalDiffTime . fromIntegral

isRoomAvailable :: Int -> Int -> Schedule Room -> Bool
isRoomAvailable start end sch =
  all (== Free) . take (end - start) . drop start $ schedule sch

findMeetingRooms :: [Schedule Room] -> RelativeMeeting -> [Room]
findMeetingRooms rschedules m =
  let freeSchedules = filter (isRoomAvailable (startIndex m) (endIndex m)) rschedules
   in map entity freeSchedules

data RelativeMeetingWithRooms = RelativeMeetingWithRooms
  { rm :: RelativeMeeting,
    roomsR :: [Room]
  }

addRoomsToMeeting :: [Schedule Room] -> RelativeMeeting -> RelativeMeetingWithRooms
addRoomsToMeeting rSchedules rm' = RelativeMeetingWithRooms rm' (findMeetingRooms rSchedules rm')

isMeetingGood :: Int -> Meeting -> Bool
isMeetingGood inPerson m =
  after830 && before1730 && not onWeekend && startsAndEndsSameDay && not inPersonButNoRoom
  where
    after830 = localTimeOfDay (zonedTimeToLocalTime $ startTime m) >= TimeOfDay 8 30 0
    before1730 = localTimeOfDay (zonedTimeToLocalTime $ endTime m) <= TimeOfDay 17 30 0
    startsAndEndsSameDay = localDay (zonedTimeToLocalTime $ startTime m) == localDay (zonedTimeToLocalTime $ endTime m)
    onWeekend = case dayOfWeek $ localDay $ zonedTimeToLocalTime $ startTime $ m of
      Saturday -> True
      Sunday -> True
      _ -> False
    inPersonButNoRoom = null (rooms m) && inPerson > 0

-- | Ranks the 'quality' of a meeting, with lower numbers being better.
meetingScore :: Meeting -> Int
meetingScore m = case length (people m) of
  0 -> 0
  l -> case rooms m of
    [] -> 10000
    rs -> minimum $ map (roomSizeDiff l) rs
      where
        -- How far away a room's capacity is from the number of people who need it
        roomSizeDiff :: Int -> Room -> Int
        roomSizeDiff n r = abs (capacity r - n)

-- | Compare two meetings. The meeting with the lower score is placed first.
-- Ties are broken by start time (i.e. earlier is better).
compareMeeting :: Meeting -> Meeting -> Ordering
compareMeeting m1 m2 = case meetingScore m1 `compare` meetingScore m2 of
  EQ -> zonedTimeToUTC (startTime m1) `compare` zonedTimeToUTC (startTime m2)
  x -> x

chooseBestMeeting :: NonEmpty Meeting -> Meeting
chooseBestMeeting ms = maximumBy compareMeeting ms

data Meeting = Meeting
  { startTime :: ZonedTime,
    endTime :: ZonedTime,
    people :: [Person],
    rooms :: [Room]
  }
  deriving (Show)

instance Eq Meeting where
  m1 == m2 =
    (people m1 == people m2)
      && (rooms m1 == rooms m2)
      && (zonedTimeToUTC (startTime m1) == zonedTimeToUTC (startTime m2))
      && (zonedTimeToUTC (endTime m1) == zonedTimeToUTC (endTime m2))

absolutiseMeetings :: UTCTime -> Minutes -> TimeZone -> RelativeMeetingWithRooms -> Meeting
absolutiseMeetings startTime' intervalMinutes tz rmwr =
  Meeting
    { startTime = utcToZonedTime tz (indexToTime $ startIndex $ rm rmwr),
      endTime = utcToZonedTime tz (indexToTime $ endIndex $ rm rmwr),
      people = relPeople (rm rmwr),
      rooms = roomsR rmwr
    }
  where
    indexToTime :: Int -> UTCTime
    indexToTime idx = addUTCTime (intSecondsToNDT $ (unMinutes intervalMinutes) * 60 * idx) startTime'

getMeetings :: [Schedule Person] -> [Schedule Room] -> Int -> Int -> UTCTime -> Minutes -> TimeZone -> [Meeting]
getMeetings personSchedules roomSchedules inPerson nChunks startTime' intervalMinutes londonTz =
  let relativeMeetings = findRelativeMeetings personSchedules nChunks
      relativeMeetingsWithRooms = map (addRoomsToMeeting roomSchedules) relativeMeetings
      meetings = map (absolutiseMeetings startTime' intervalMinutes londonTz) relativeMeetingsWithRooms
   in filter (isMeetingGood inPerson) meetings

getRoomMeetings :: [Schedule Room] -> Int -> Int -> UTCTime -> Minutes -> TimeZone -> [Meeting]
getRoomMeetings roomSchedules inPerson totalChunks startTime' intervalMinutes localTz =
  let relativeMeetings = map (makeRelativeMeeting 1 []) [0 .. totalChunks - 1]
      relativeMeetingsWithRooms = map (addRoomsToMeeting roomSchedules) relativeMeetings
      meetings = map (absolutiseMeetings startTime' intervalMinutes localTz) relativeMeetingsWithRooms
   in filter (isMeetingGood inPerson) meetings
