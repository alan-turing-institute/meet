module Utils where

import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (LocalTime (..), getTimeZone, utcToLocalTime)
import System.Environment (setEnv)

getCurrentLondonTime :: IO LocalTime
getCurrentLondonTime = do
  now <- getCurrentTime
  setEnv "TZ" "Europe/London"
  tz <- getTimeZone now
  pure $ utcToLocalTime tz now

gracefulDivide :: Int -> Int -> IO Int
gracefulDivide numerator denominator = do
  case quotRem numerator denominator of
    (0, _) ->
      error $
        concat
          [ "Meeting duration of ",
            show numerator,
            " minutes is shorter than the meeting interval of ",
            show denominator,
            " minutes."
          ]
    (q, 0) -> pure q
    (q, r) -> do
      putStrLn $
        concat
          [ "WARNING: Meeting duration of ",
            show numerator,
            " minutes divides by ",
            show denominator,
            " with ",
            show r,
            " minutes remaining, so we will book a ",
            show (q * denominator),
            " minute meeting."
          ]
      pure q
