module Types
  ( Entity (..),
    Meeting (..),
    Availability (..),
    Schedule (..),
    getEntity,
    getEmail,
    getShortName,
    toSchedule,
    isPerson,
    allRooms,
    isMeetingGood,
  )
where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (DayOfWeek (..), dayOfWeek)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..), ZonedTime (..), zonedTimeToUTC)

data Availability = Free | Tentative | Busy | OutOfOffice | WorkingElsewhere
  deriving (Eq, Show)

parseAvailabilityText :: Text -> [Availability]
parseAvailabilityText = map parseChar . T.unpack
  where
    parseChar :: Char -> Availability
    parseChar c = case c of
      '0' -> Free
      '1' -> Tentative
      '2' -> Busy
      '3' -> OutOfOffice
      '4' -> WorkingElsewhere
      _ -> error $ "unexpected character returned by MS Graph API: " ++ [c]

data RoomLocation = FirstFloor | SecondFloor | FourthFloor deriving (Show, Eq, Ord)

data Entity
  = Person {personEmail :: Text}
  | Room {location :: RoomLocation, capacity :: Int, roomEmail :: Text}
  deriving (Eq, Ord)

instance Show Entity where
  show (Person e) = T.unpack $ T.takeWhile (/= '@') e
  show (Room _ _ e) = T.unpack $ T.takeWhile (/= '@') e

data Schedule = Schedule
  { entity :: Entity,
    schedule :: [Availability]
  }

isPerson :: Schedule -> Bool
isPerson s = case entity s of
  Person _ -> True
  _ -> False

allRooms :: Map Text (RoomLocation, Int, Text)
allRooms =
  M.fromList
    [ ("enigma@turing.ac.uk", (FirstFloor, 45, "Eni")),
      ("margarethamilton@turing.ac.uk", (FirstFloor, 12, "MHa")),
      ("ada@turing.ac.uk", (FirstFloor, 12, "Ada")),
      ("lovelace@turing.ac.uk", (FirstFloor, 10, "Lov")),
      ("ursulafranklin@turing.ac.uk", (FirstFloor, 6, "UFr")),
      ("joanclarke@turing.ac.uk", (FirstFloor, 5, "JCl")),
      ("marianrejewski@turing.ac.uk", (FirstFloor, 5, "MRe")),
      ("projectspace@turing.ac.uk", (FirstFloor, 4, "PSp")),
      ("florencenightingale@turing.ac.uk", (FirstFloor, 6, "FNi")),
      ("maejemison@turing.ac.uk", (FirstFloor, 7, "MJe")),
      ("cipher@turing.ac.uk", (FirstFloor, 6, "Cip")),
      ("davidblackwell@turing.ac.uk", (FirstFloor, 8, "DBl")),
      ("jackgood@turing.ac.uk", (FirstFloor, 8, "JGo")),
      ("maryshelley@turing.ac.uk", (SecondFloor, 12, "MSh")),
      ("isaacasimov@turing.ac.uk", (SecondFloor, 4, "IAs")),
      ("helensharman@turing.ac.uk", (SecondFloor, 4, "HSh")),
      ("ace@turing.ac.uk", (FourthFloor, 4, "Ace")),
      ("banburismus@turing.ac.uk", (FourthFloor, 4, "Ban")),
      ("delilah@turing.ac.uk", (FourthFloor, 6, "Del")),
      ("turingery@turing.ac.uk", (FourthFloor, 6, "Tur"))
    ]

getShortName :: Entity -> Text
getShortName en = let email = getEmail en in case M.lookup email allRooms of
  Just (_, _, short) -> short
  Nothing -> email

getEntity :: Text -> Entity
getEntity email = case M.lookup email allRooms of
  Just (l, c, _) -> Room l c email
  Nothing -> Person email

getEmail :: Entity -> Text
getEmail (Person e) = e
getEmail (Room _ _ e) = e

toSchedule :: (Text, Text) -> Schedule
toSchedule t =
  Schedule
    { entity = getEntity (fst t),
      schedule = parseAvailabilityText (snd t)
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
