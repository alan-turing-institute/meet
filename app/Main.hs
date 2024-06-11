module Main where

import Args
import Azure
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import Meetings (findRelativeMeetings)
import System.Exit (exitFailure)
import System.IO (hFlush, isEOF, stdout)
import Types (toPerson)

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
  args <- getArgs
  putStrLn $ "arguments received: " ++ show args

  eitherToken <- getToken
  case eitherToken of
    Left err -> print err >> exitFailure
    Right token -> do
      emailAddrs <- getEmails
      now <- getCurrentTime
      let twoWeeksLater = addUTCTime (secondsToNominalDiffTime 3600 * 24 * 14) now
      strings <- getAvailabilityString token emailAddrs now twoWeeksLater
      let people = map toPerson strings
      let relativeMeetings = findRelativeMeetings people 2
      print relativeMeetings
