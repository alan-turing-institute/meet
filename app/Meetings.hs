module Meetings where

import Data.List (sortOn, transpose)
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
makeWindows :: Int -> [a] -> [[a]]
makeWindows n xs =
  if n > length xs
    then []
    else take n xs : makeWindows n (tail xs)

isAllAvailable :: [[Availability]] -> Bool
isAllAvailable = all (all (== Free))

getAvailabilityWindows :: [Schedule] -> Int -> [[[Availability]]]
getAvailabilityWindows schs n = makeWindows n (transpose (map schedule schs))

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

roomSizeDiff :: Int -> Entity -> Int
roomSizeDiff n r = abs (capacity r - n)

meetingRank :: Meeting -> Int
meetingRank m = case length (people m) of
  0 -> 0 -- Nobody available to meet :(
  l -> case rooms m of
    [] -> 10000
    rs -> minimum $ map (roomSizeDiff l) rs

chooseTopMeetings :: [Meeting] -> [Meeting]
chooseTopMeetings = sortOn meetingRank

-- chooseTopMeetings n meets =

-- sortBy
--   first: check if you can get a meeting room ~ numPeople
