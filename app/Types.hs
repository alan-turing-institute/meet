module Types (Schedule (..), Person (..), Meeting (..), Room (..)) where

import Data.Time.Clock (UTCTime (..))

-- TODO: define
data Schedule = Schedule
  deriving (Eq, Show)

data Person = Person
  { email :: String,
    name :: Maybe String,
    schedule :: Schedule
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
