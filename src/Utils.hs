module Utils where

import Entities (Minutes (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (LocalTime (..), getCurrentTimeZone, utcToLocalTime)
import Print (prettyThrow, prettyWarn)

tshow :: (Show a) => a -> Text
tshow = T.pack . show

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
