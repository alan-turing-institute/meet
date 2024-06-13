module Meetings where

import Data.Fixed (Fixed (..))
import Data.List (transpose)
import Data.Time.Clock
import Data.Time.LocalTime (TimeZone, utcToZonedTime)
import Types

data RelativeMeeting = RelativeMeeting
  { startIndex :: Int,
    endIndex :: Int,
    relEmails :: [String]
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

getAvailabilityWindows :: [Person] -> Int -> [[[Availability]]]
getAvailabilityWindows people n = window n (transpose (map schedule people))

findRelativeMeetings :: [Person] -> Int -> [RelativeMeeting]
findRelativeMeetings people n =
  let windows = getAvailabilityWindows people n
      windowAvailabilities :: [Bool]
      windowAvailabilities = map isAllAvailable windows
      indicesAndAvailabilities = zip [(0 :: Int) ..] windowAvailabilities
      indicesWhereTrue = map fst (filter snd indicesAndAvailabilities)
      emails' = map personEmail people
   in map (makeRelativeMeeting n emails') indicesWhereTrue

makeRelativeMeeting :: Int -> [String] -> Int -> RelativeMeeting
makeRelativeMeeting n e index =
  RelativeMeeting
    { startIndex = index,
      endIndex = index + n,
      relEmails = e
    }

intSecondsToNDT :: Int -> NominalDiffTime
intSecondsToNDT = secondsToNominalDiffTime . fromIntegral

indicesToTimes :: UTCTime -> Int -> Int -> [UTCTime]
indicesToTimes startTime' interval len =
  let addTime :: Int -> UTCTime
      addTime idx = addUTCTime (intSecondsToNDT $ interval * 60 * idx) startTime'
   in map addTime [0 .. len]

absolutiseMeetings :: [UTCTime] -> TimeZone -> RelativeMeeting -> Meeting
absolutiseMeetings times tz rm =
  Meeting
    { startTime = utcToZonedTime tz (times !! startIndex rm),
      endTime = utcToZonedTime tz (times !! endIndex rm),
      emails = relEmails rm
    }

-------------------------------
-- Meeting rooms (TBD later) --
-------------------------------

data Entity = ERoom Int String | EPerson String

email :: Entity -> String
email (ERoom _ s) = s
email (EPerson s) = s

findMeetingTimes :: [Entity] -> [Meeting]
findMeetingTimes = undefined

-- do a find meeting request with all of the pre-defined meeting rooms
findMeetingRoom :: [Meeting] -> Int -> [Meeting]
findMeetingRoom = undefined
