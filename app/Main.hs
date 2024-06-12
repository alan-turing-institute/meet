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
import Utils (gracefulDivide)

-- getEmails :: IO [Text]
-- getEmails = do
--   putStrLn "Enter the email addresses of the people you want to stalk (no need to include @turing.ac.uk)."
--   let prompt = do
--         putStr "> "
--         hFlush stdout
--         done <- isEOF
--         if done
--           then pure []
--           else do
--             emailAddr <- getLine
--             rest <- prompt
--             pure (T.pack emailAddr : rest)
--   emails <- prompt
--   pure $ map (<> "@turing.ac.uk") . filter (not . T.null) . map T.strip $ emails

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ "arguments received: " ++ show args
  let meetingInterval = interval args
  let meetingDuration = duration args
  let chunks = gracefulDivide meetingDuration meetingInterval  -- exits early if duration < interval
  print chunks  -- temporary fix to materialize the value of chunks _before_ getting token
  eitherToken <- getToken
  case eitherToken of
    Left err -> print err >> exitFailure
    Right token -> do
      let emailAddrs = emails args
      now <- getCurrentTime
      let twoWeeksLater = addUTCTime (secondsToNominalDiffTime 3600 * 24 * 14) now
      strings <- getAvailabilityString token emailAddrs now twoWeeksLater meetingInterval
      let people = map toPerson strings
      let relativeMeetings = findRelativeMeetings people chunks
      print relativeMeetings
