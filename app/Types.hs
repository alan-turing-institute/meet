module Types (Schedule (..), Person (..), Meeting (..), Room (..)) where

import Data.Time.Clock (UTCTime (..))

data Availability = Free | Tentative | Busy | OutOfOffice | WorkingElsewhere

data Person = Person
  { email :: String,
    name :: Maybe String,
    schedule :: [Int]
  }
  deriving (Eq, Show)

data Meeting = Meeting
  { startTime :: UTCTime,
    endTime :: UTCTime,
    people :: [Person]
  }
  deriving (Eq, Show)

data Room = Room {
    email :: String,
    floor :: Int,
    size :: Int
}
