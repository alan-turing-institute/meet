module Types
  ( Entity (..),
    Meeting (..),
    Availability (..),
    Schedule (..),
    getEntity,
    getEmail,
    toSchedule,
    isPerson,
    allRooms,
    isMeetingGood,
  )
where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Time.Calendar (DayOfWeek (..), dayOfWeek)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..), ZonedTime (..), zonedTimeToUTC)

data Availability = Free | Tentative | Busy | OutOfOffice | WorkingElsewhere
  deriving (Eq, Show)

parseAvailabilityString :: String -> [Availability]
parseAvailabilityString = map parseChar
  where
    parseChar :: Char -> Availability
    parseChar c = case c of
      '0' -> Free
      '1' -> Tentative
      '2' -> Busy
      '3' -> OutOfOffice
      '4' -> WorkingElsewhere
      _ -> error $ "unexpected character returned by MS Graph API: " ++ [c]

data RoomLocation = FirstFloor | SecondFloor | FourthFloor deriving (Show, Eq)

data Entity
  = Person {personEmail :: String}
  | Room {roomEmail :: String, location :: RoomLocation, capacity :: Int}
  deriving (Eq)

instance Show Entity where
  show (Person e) = takeWhile (/= '@') e
  show (Room e _ _) = takeWhile (/= '@') e

data Schedule = Schedule
  { entity :: Entity,
    schedule :: [Availability]
  }

isPerson :: Schedule -> Bool
isPerson s = case entity s of
  Person _ -> True
  _ -> False

allRooms :: Map String (RoomLocation, Int)
allRooms =
  M.fromList
    [ ("enigma@turing.ac.uk", (FirstFloor, 45)),
      ("margarethamilton@turing.ac.uk", (FirstFloor, 12)),
      ("ada@turing.ac.uk", (FirstFloor, 12)),
      ("lovelace@turing.ac.uk", (FirstFloor, 10)),
      ("ursulafranklin@turing.ac.uk", (FirstFloor, 6)),
      ("joanclarke@turing.ac.uk", (FirstFloor, 5)),
      ("marianrejewski@turing.ac.uk", (FirstFloor, 5)),
      ("projectspace@turing.ac.uk", (FirstFloor, 4)),
      ("florencenightingale@turing.ac.uk", (FirstFloor, 6)),
      ("maejemison@turing.ac.uk", (FirstFloor, 7)),
      ("cipher@turing.ac.uk", (FirstFloor, 6)),
      ("davidblackwell@turing.ac.uk", (FirstFloor, 8)),
      ("jackgood@turing.ac.uk", (FirstFloor, 8)),
      ("maryshelley@turing.ac.uk", (SecondFloor, 12)),
      ("isaacasimov@turing.ac.uk", (SecondFloor, 4)),
      ("helensharman@turing.ac.uk", (SecondFloor, 4)),
      ("ace@turing.ac.uk", (FourthFloor, 4)),
      ("banburismus@turing.ac.uk", (FourthFloor, 4)),
      ("delilah@turing.ac.uk", (FourthFloor, 6)),
      ("turingery@turing.ac.uk", (FourthFloor, 6))
    ]

getEntity :: String -> Entity
getEntity email = case M.lookup email allRooms of
  Just (location, capacity) -> Room email location capacity
  Nothing -> Person email

getEmail :: Entity -> String
getEmail (Person e) = e
getEmail (Room e _ _) = e

toSchedule :: (String, String) -> Schedule
toSchedule t =
  Schedule
    { entity = getEntity (fst t),
      schedule = parseAvailabilityString (snd t)
    }

data Meeting = Meeting
  { startTime :: ZonedTime,
    endTime :: ZonedTime,
    people :: [Entity],
    rooms :: [Entity]
  }
  deriving (Show)

onWeekend :: Meeting -> Bool
onWeekend m = case dayOfWeek $ localDay $ zonedTimeToLocalTime $ startTime $ m of
  Saturday -> True
  Sunday -> True
  _ -> False

noRoomsButInPerson :: Meeting -> Int -> Bool
noRoomsButInPerson m n = null (rooms m) && n > 0

isMeetingGood :: Int -> Meeting -> Bool
isMeetingGood inPerson m =
  (localTimeOfDay (zonedTimeToLocalTime (startTime m)) >= TimeOfDay 8 30 0)
    && (localTimeOfDay (zonedTimeToLocalTime (endTime m)) <= TimeOfDay 17 30 0)
    && not (onWeekend m)
    && localDay (zonedTimeToLocalTime (startTime m)) == localDay (zonedTimeToLocalTime (endTime m))
    && not (noRoomsButInPerson m inPerson)

instance Eq Meeting where
  m1 == m2 =
    (people m1 == people m2)
      && (rooms m1 == rooms m2)
      && (zonedTimeToUTC (startTime m1) == zonedTimeToUTC (startTime m2))
      && (zonedTimeToUTC (endTime m1) == zonedTimeToUTC (endTime m2))
