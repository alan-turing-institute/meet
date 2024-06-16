module Utils where

import Args (Minutes (..))
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (LocalTime (..), getCurrentTimeZone, utcToLocalTime)

getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = do
  now <- getCurrentTime
  tz <- getCurrentTimeZone
  pure $ utcToLocalTime tz now

gracefulDivide :: Minutes -> Minutes -> IO Int
gracefulDivide (Minutes numerator) (Minutes denominator) = do
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

partitionTupledEither :: [(a, Either b c)] -> ([(a, b)], [(a, c)])
partitionTupledEither = foldr acc ([], [])
  where
    acc (a, e) (fs, ss) = case e of
      Left f -> ((a, f) : fs, ss)
      Right s -> (fs, (a, s) : ss)
