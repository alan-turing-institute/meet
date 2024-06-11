module Main where

import Azure
import Data.Maybe (catMaybes)
import System.Exit (exitFailure)
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
      printAuthenticatedUserName token

-- let emails = ["me@turing.ac.uk", "you@turing.ac.uk", "etc@turing.ac.uk"]
-- maybePeople <- mapM getPerson emails
-- let actualPeople = catMaybes maybePeople
-- let meeting = chooseMeeting actualPeople
-- print meeting
