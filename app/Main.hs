module Main where

import Azure
import Data.Maybe (catMaybes)
import Data.Time.Clock (addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)
import Types

chooseMeeting :: [Person] -> Meeting
chooseMeeting = undefined

main :: IO ()
main = do
  eitherToken <- getToken
  case eitherToken of
    Left err -> print err >> exitFailure
    Right token -> do
      putStrLn $ "Got token! " <> show token
      putStr "Enter the email address of the person you want to stalk: > "
      hFlush stdout
      emailAddr <- getLine
      now <- getCurrentTime
      let oneHourLater = addUTCTime (secondsToNominalDiffTime 3600) now
      printFreeBusySchedule token [emailAddr] now oneHourLater

-- let emails = ["me@turing.ac.uk", "you@turing.ac.uk", "etc@turing.ac.uk"]
-- maybePeople <- mapM getPerson emails
-- let actualPeople = catMaybes maybePeople
-- let meeting = chooseMeeting actualPeople
-- print meeting
