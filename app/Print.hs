module Print (prettyPrint, infoPrint) where

import Data.List (intercalate)
import Data.List.Split (chunksOf)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime (LocalTime (..), ZonedTime (..))
import Types (Meeting (..), allRooms, getEmail, getEntity)

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
      rowLength = T.length headerRow
      sepRow = T.replicate rowLength "-"
      makeMeetingRow :: Meeting -> Text
      makeMeetingRow m =
        T.intercalate
          "|"
          ( [ T.pack (iso8601Show . localDay . zonedTimeToLocalTime . startTime $ m),
              T.pack (iso8601Show . localTimeOfDay . zonedTimeToLocalTime . startTime $ m)
            ]
              ++ map (\room -> if getEntity room `elem` rooms m then " * " else "   ") (M.keys allRooms)
          )
      meetingRows = chunksOf 5 (map makeMeetingRow ms)
   in T.putStrLn $ T.unlines $ headerRow : sepRow : intercalate [sepRow] meetingRows

infoPrint :: [Meeting] -> IO ()
infoPrint ms = do
  let m = head ms

  case rooms m of
    [] -> do
      putStrLn
        ( "The first meeting available runs from "
            ++ show (startTime m)
            ++ " to "
            ++ show (endTime m)
            ++ "."
        )
    r : _ -> do
      putStrLn
        ( "The first meeting available with a room size that fits your needs runs from "
            ++ show (startTime m)
            ++ " to "
            ++ show (endTime m)
            ++ " in "
            ++ getEmail r
            ++ "."
        )
