module Types
  ( Person (..),
    Meeting (..),
    Room (..),
    Availability (..),
    toPerson,
  )
where

import Data.Time.Clock (UTCTime (..))

data Availability = Free | Tentative | Busy | OutOfOffice | WorkingElsewhere
  deriving (Eq, Show)

parseAvailabilityString :: [Char] -> [Availability]
parseAvailabilityString = map parseChar
  where
    parseChar :: Char -> Availability
    parseChar c = case c of
      '0' -> Free
      '1' -> Tentative
      '2' -> Busy
      '3' -> OutOfOffice
      '4' -> WorkingElsewhere
      _anythingElse -> error "bad definition of availability"

toPerson :: (String, String) -> Person
toPerson t =
  Person
    { personEmail = fst t,
      name = Nothing,
      schedule = parseAvailabilityString (snd t)
    }

data Person = Person
  { personEmail :: String,
    name :: Maybe String,
    schedule :: [Availability]
  }
  deriving (Eq, Show)

data Meeting = Meeting
  { startTime :: UTCTime,
    endTime :: UTCTime,
    emails :: [String]
  }
  deriving (Eq, Show)

data Room = Room
  { roomEmail :: String,
    roomName :: String,
    floor :: Int,
    size :: Int
  }
