module Print (prettyPrint) where

import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime (LocalTime (..), ZonedTime (..))
import Types (Meeting (..), allRooms, getEntity)

prettyPrint :: [Meeting] -> IO ()
prettyPrint ms =
  let headerRow :: Text
      headerRow =
        T.intercalate
          "|"
          ( [ "Date      ",
              "Time    "
            ]
              ++ map (T.pack . take 3) (M.keys allRooms)
          )
      makeMeetingRow :: Meeting -> Text
      makeMeetingRow m =
        T.intercalate
          "|"
          ( [ T.pack (iso8601Show . localDay . zonedTimeToLocalTime . startTime $ m),
              T.pack (iso8601Show . localTimeOfDay . zonedTimeToLocalTime . startTime $ m)
            ]
              ++ map (\room -> if getEntity room `elem` rooms m then " * " else "   ") (M.keys allRooms)
          )
   in T.putStrLn $ T.unlines $ headerRow : map makeMeetingRow ms
