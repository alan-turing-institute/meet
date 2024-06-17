{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Args (Args (..), getArgs)
import Azure (fetchSchedules, getToken)
import Config (Config (..), Group (..), readConfig)
import Control.Applicative
import Control.Monad (when)
import qualified Data.List.NonEmpty as NE
import Data.Text (isInfixOf, splitOn, toLower)
import qualified Data.Text.IO as T
import Data.Time.Calendar (addDays)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..), getCurrentTimeZone, localTimeToUTC)
import Entities (Days (..), Person (..), Room (..), allRooms)
import Meetings (chooseBestMeeting, getMeetings)
import Print (infoPrint, prettyPrint)
import System.Exit (exitSuccess)
import Utils
import Prelude -- Ensure Applicative is in scope and we have no warnings, before/after AMP.

main :: IO ()
main = do
  args <- getArgs
  let ppl = argsEmails args
      intervalMinutes = argsInterval args
      durationMinutes = argsDuration args
      searchStartDate = argsStartDate args
      searchSpanDays = argsTimespan args
      inPerson = argsInPerson args
  nChunks <- gracefulDivide durationMinutes intervalMinutes

  startDate' <- case searchStartDate of
    Just d -> pure d
    Nothing -> localDay <$> getCurrentLocalTime

  localTz <- getCurrentTimeZone
  let startTime' = localTimeToUTC localTz $ LocalTime startDate' (TimeOfDay 8 30 0)
  let endDate' = addDays (fromIntegral $ unDays searchSpanDays - 1) startDate'
      endTime' = localTimeToUTC localTz $ LocalTime endDate' (TimeOfDay 17 30 0)

  let okRooms = filter ((>= inPerson) . capacity) allRooms
  when (null okRooms) $ do
    T.putStrLn "No rooms that meet your criteria were available. :("
    T.putStrLn "Perhaps try reducing the number of people who need to be in-person?"
    exitSuccess

  config <- readConfig $ argsConfigPath args
  -- Add group emails to the list of people, but only if that group is specified in the list of people.
  -- We then create a new list that contains the emails of the groups specified in the list of people.
  -- This will check if the group name is an infix of the corresponding person entry,
  -- since "@turing.ac.uk" is appended to the email address if it's not present,
  -- and then add the email addresses of the group to the list of people.
  let lowercaseEmails = map (toLower . personEmail) ppl
      validGroups = filter (\g -> any (isInfixOf (toLower (groupName g))) lowercaseEmails) (groups config)
      validGroupEmails = concatMap groupEmails validGroups
      validGroupNames = map (toLower . groupName) validGroups
      ppl' = filter (\p -> any (isInfixOf (toLower (personEmail p))) validGroupNames) ppl ++ map Person validGroupEmails
  if validGroups /= []
    then T.putStrLn "Adding emails from the following groups:" >> mapM_ T.putStrLn validGroupNames
    else T.putStrLn "No groups were specified in the list of people."

  token <- getToken
  (personSchs, roomSchs) <- fetchSchedules token ppl' okRooms startTime' endTime' intervalMinutes
  let goodMeetings = getMeetings personSchs roomSchs inPerson nChunks startTime' intervalMinutes localTz

  case NE.nonEmpty goodMeetings of
    Nothing -> T.putStrLn "No meetings were available. :("
    Just ms ->
      if argsFeelingLucky args
        then infoPrint (chooseBestMeeting ms) inPerson
        else prettyPrint goodMeetings
