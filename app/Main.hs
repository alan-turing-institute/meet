module Main where

import Azure
import Control.Monad (forM_)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import System.Exit (exitFailure)
import System.IO (hFlush, isEOF, stdout)
import Types


getEmails :: IO [Text]
getEmails = do
  putStrLn "Enter the email addresses of the people you want to stalk (no need to include @turing.ac.uk)."
  let prompt = do
        putStr "> "
        hFlush stdout
        done <- isEOF
        if done
          then pure []
          else do
            emailAddr <- getLine
            rest <- prompt
            pure (T.pack emailAddr : rest)
  emails <- prompt
  pure $ map (<> "@turing.ac.uk") . filter (not . T.null) . map T.strip $ emails

main :: IO ()
main = do
  eitherToken <- getToken
  case eitherToken of
    Left err -> print err >> exitFailure
    Right token -> do
      emailAddrs <- getEmails
      now <- getCurrentTime
      let twoWeeksLater = addUTCTime (secondsToNominalDiffTime 3600 * 24 * 14) now
      strings <- getAvailabilityString token emailAddrs now twoWeeksLater
      forM_ strings $ \(s1, s2) -> do
        putStrLn ""
        putStrLn $ replicate 80 '-'
        putStrLn s1
        putStrLn ""
        putStrLn s2
        putStrLn ""

-- let emails = ["me@turing.ac.uk", "you@turing.ac.uk", "etc@turing.ac.uk"]
-- maybePeople <- mapM getPerson emails
-- let actualPeople = catMaybes maybePeople
-- let meeting = chooseMeeting actualPeople
-- print meeting
