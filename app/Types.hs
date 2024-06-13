module Types
  ( Person (..),
    Meeting (..),
    Room (..),
    Availability (..),
    toPerson,
  )
where

import Data.Time.LocalTime (ZonedTime (..), zonedTimeToUTC)

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

toPerson :: (String, String) -> Person
toPerson t =
  Person
    { personEmail = fst t,
      schedule = parseAvailabilityString (snd t)
    }

data Person = Person
  { personEmail :: String,
    schedule :: [Availability]
  }
  deriving (Eq, Show)

data Meeting = Meeting
  { startTime :: ZonedTime,
    endTime :: ZonedTime,
    emails :: [String]
  }
  deriving (Show)

instance Eq Meeting where
  m1 == m2 =
    (emails m1 == emails m2)
      && (zonedTimeToUTC (startTime m1) == zonedTimeToUTC (startTime m2))
      && (zonedTimeToUTC (endTime m1) == zonedTimeToUTC (endTime m2))

data Room = Room
  { roomEmail :: String,
    roomName :: String,
    floor :: Int,  -- TODO: Use a sum type
    size :: Int
  }
