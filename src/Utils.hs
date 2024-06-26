module Utils where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (LocalTime (..), TimeZone (..), getCurrentTimeZone, utcToLocalTime)
import Data.Time.Zones (loadTZFromDB, timeZoneForUTCTime, utcToLocalTimeTZ)
import Entities (Minutes (..))
import Print (prettyThrow, prettyWarn)

tshow :: (Show a) => a -> Text
tshow = T.pack . show

-- | Returns the current timezone in London (either GMT or BST).
getCurrentLondonTZ :: IO TimeZone
getCurrentLondonTZ = do
  now <- getCurrentTime
  london <- loadTZFromDB "Europe/London"
  pure $ timeZoneForUTCTime london now

-- | Returns the current time in London.
getCurrentLondonTime :: IO LocalTime
getCurrentLondonTime = do
  now <- getCurrentTime
  london <- loadTZFromDB "Europe/London"
  pure $ utcToLocalTimeTZ london now

-- | Returns the current local time, according to the user's system timezone.
getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = do
  now <- getCurrentTime
  tz <- getCurrentTimeZone
  pure $ utcToLocalTime tz now

gracefulDivide :: Minutes -> Minutes -> IO Int
gracefulDivide (Minutes numerator) (Minutes denominator) = do
  case quotRem numerator denominator of
    (0, _) ->
      prettyThrow $
        T.concat
          [ "Meeting duration of ",
            tshow numerator,
            " minutes is shorter than the meeting interval of ",
            tshow denominator,
            " minutes."
          ]
    (q, 0) -> pure q
    (q, _) -> do
      prettyWarn $
        T.concat
          [ "Meeting duration of ",
            tshow numerator,
            " minutes is not a multiple of the meeting interval of ",
            tshow denominator,
            " minutes. Proceeding with a ",
            tshow (q * denominator),
            "-minute meeting instead."
          ]
      pure q
