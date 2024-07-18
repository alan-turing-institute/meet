module Meet.Entities
  ( Availability (..),
    Schedule (..),
    HasSchedule,
    Person (..),
    Room (..),
    Minutes (..),
    Days (..),
    allRooms,
  )
where

import Data.Text (Text)

newtype Minutes = Minutes {unMinutes :: Int}
  deriving (Eq, Show)

newtype Days = Days {unDays :: Int}
  deriving (Eq, Show)

data Availability = Free | Tentative | Busy | OutOfOffice | WorkingElsewhere
  deriving (Eq, Show)

data RoomLocation = FirstFloor | SecondFloor | FourthFloor deriving (Show, Eq, Ord)

newtype Person = Person {personEmail :: Text}
  deriving (Eq, Ord, Show)

data Room = Room
  { location :: RoomLocation,
    capacity :: Int,
    roomEmail :: Text,
    roomShortName :: Text
  }
  deriving (Eq, Ord, Show)

class HasSchedule a

instance HasSchedule Person

instance HasSchedule Room

data (HasSchedule a) => Schedule a = Schedule
  { entity :: a,
    schedule :: [Availability]
  }

allRooms :: [Room]
allRooms =
  [ Room FirstFloor 45 "enigma@turing.ac.uk" "Eni",
    Room FirstFloor 12 "margarethamilton@turing.ac.uk" "MHa",
    Room FirstFloor 12 "ada@turing.ac.uk" "Ada",
    Room FirstFloor 10 "lovelace@turing.ac.uk" "Lov",
    Room FirstFloor 6 "ursulafranklin@turing.ac.uk" "UFr",
    Room FirstFloor 5 "joanclarke@turing.ac.uk" "JCl",
    Room FirstFloor 5 "marianrejewski@turing.ac.uk" "MRe",
    Room FirstFloor 4 "projectspace@turing.ac.uk" "PSp",
    Room FirstFloor 6 "florencenightingale@turing.ac.uk" "FNi",
    Room FirstFloor 7 "maejemison@turing.ac.uk" "MJe",
    Room FirstFloor 6 "cipher@turing.ac.uk" "Cip",
    Room FirstFloor 8 "davidblackwell@turing.ac.uk" "DBl",
    Room FirstFloor 8 "jackgood@turing.ac.uk" "JGo",
    Room SecondFloor 12 "maryshelley@turing.ac.uk" "MSh",
    Room SecondFloor 4 "isaacasimov@turing.ac.uk" "IAs",
    Room SecondFloor 4 "helensharman@turing.ac.uk" "HSh",
    Room FourthFloor 4 "ace@turing.ac.uk" "Ace",
    Room FourthFloor 4 "banburismus@turing.ac.uk" "Ban",
    Room FourthFloor 6 "delilah@turing.ac.uk" "Del",
    Room FourthFloor 6 "turingery@turing.ac.uk" "Tur"
  ]
