module Print (prettyPrint, infoPrint, prettyThrow, prettyWarn) where

import Control.Monad (forM_)
import Data.List (nub, sort, transpose)
import Data.List.Extra (groupOn)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Calendar (Day, dayOfWeek, toGregorian)
import qualified Data.Time.Calendar as C
import Data.Time.LocalTime
  ( LocalTime (..),
    TimeOfDay (..),
    TimeZone (..),
    ZonedTime (..),
    utcToZonedTime,
    zonedTimeToUTC,
  )
import Data.Word (Word8)
import Entities (Room (..))
import Meetings (Meeting (..))
import System.Console.ANSI
import System.Exit (exitFailure)
import System.IO (stderr)

tshow :: (Show a) => a -> Text
tshow = T.pack . show

showDateWithDay :: Day -> Text
showDateWithDay d =
  let dayOfWeekShort = T.take 3 $ tshow $ dayOfWeek d
      (_, m, d') = toGregorian d
      monthShort = case m of
        C.January -> "Jan"
        C.February -> "Feb"
        C.March -> "Mar"
        C.April -> "Apr"
        C.May -> "May"
        C.June -> "Jun"
        C.July -> "Jul"
        C.August -> "Aug"
        C.September -> "Sep"
        C.October -> "Oct"
        C.November -> "Nov"
        C.December -> "Dec"
      dayShort = T.pack (show d')
   in dayShort <> " " <> monthShort <> " (" <> dayOfWeekShort <> ")"

-- | Convert a ZonedTime to another ZonedTime
convertZone :: TimeZone -> ZonedTime -> ZonedTime
convertZone tz zt = utcToZonedTime tz $ zonedTimeToUTC zt

showTime :: TimeZone -> ZonedTime -> ZonedTime -> Text
showTime tz start end =
  let tshowPad :: Int -> Text
      tshowPad n = T.justifyRight 2 '0' (tshow n)
      show24Hr :: ZonedTime -> Text
      show24Hr zt =
        T.concat
          [ tshowPad . todHour . localTimeOfDay . zonedTimeToLocalTime $ convertZone tz zt,
            tshowPad . todMin . localTimeOfDay . zonedTimeToLocalTime $ convertZone tz zt
          ]
   in show24Hr start <> "—" <> show24Hr end

tsetSGRCode :: [SGR] -> Text
tsetSGRCode = T.pack . setSGRCode

styleText :: [SGR] -> Text -> Text
styleText sgrs t = tsetSGRCode sgrs <> t <> tsetSGRCode [Reset]

padWithStyles :: [SGR] -> Int -> Text -> Text
padWithStyles sgrs n t =
  let dw = T.length t
   in styleText sgrs $ t <> T.replicate (n - dw) " "

prettyPrint :: Maybe (NonEmpty Word8) -> TimeZone -> [Meeting] -> IO ()
prettyPrint colors tz ms = do
  let colSepChar = "│"
      rowSepChar = "─"
      doubleRowSepChar = "═"
      doubleleftIntersectionChar = "╞"
      doubleMiddleIntersectionChar = "╪"
      doubleRightIntersectionChar = "╡"
      middleIntersectionChar = "┼"
      topIntersectionChar = "┬"
      bottomIntersectionChar = "┴"
      leftIntersectionChar = "├"
      rightIntersectionChar = "┤"
      topLeftCornerChar = "┌"
      topRightCornerChar = "┐"
      bottomLeftCornerChar = "└"
      bottomRightCornerChar = "┘"
  let relevantMeetingRooms = sort $ nub $ concatMap rooms ms
      headerRow =
        "Date"
          : "Time"
          : map roomShortName relevantMeetingRooms
      makeMeetingRow m =
        [ showDateWithDay . localDay . zonedTimeToLocalTime $ convertZone tz (startTime m),
          showTime tz (startTime m) (endTime m)
        ]
          ++ map (\room -> if room `elem` rooms m then " * " else "") relevantMeetingRooms
      meetingRows = map makeMeetingRow ms
      allRows = headerRow : meetingRows
      colWidths = map (maximum . map T.length) (transpose allRows)
      -- Separator rows
      topSepRow =
        topLeftCornerChar
          <> T.intercalate topIntersectionChar (map (`T.replicate` rowSepChar) colWidths)
          <> topRightCornerChar
      doubleMiddleSepRow =
        doubleleftIntersectionChar
          <> T.intercalate doubleMiddleIntersectionChar (map (`T.replicate` doubleRowSepChar) colWidths)
          <> doubleRightIntersectionChar
      middleSepRow =
        leftIntersectionChar
          <> T.intercalate middleIntersectionChar (map (`T.replicate` rowSepChar) colWidths)
          <> rightIntersectionChar
      bottomSepRow =
        bottomLeftCornerChar
          <> T.intercalate bottomIntersectionChar (map (`T.replicate` rowSepChar) colWidths)
          <> bottomRightCornerChar
      -- Print a row with styles -- one list of styles per column
      printRow :: [[SGR]] -> [Text] -> IO ()
      printRow sgrs ts =
        T.putStrLn $
          colSepChar
            <> T.intercalate colSepChar (zipWith3 padWithStyles sgrs colWidths ts)
            <> colSepChar
      -- Print a single row but leave out the first column. Useful for omitting
      -- dates.
      printRowNoFirst :: [[SGR]] -> [Text] -> IO ()
      printRowNoFirst sgrs [] = printRow sgrs []
      printRowNoFirst sgrs (_ : ts) = printRow sgrs ("" : ts)
      -- Group meetings by date
      groupedMeetings = groupOn (localDay . zonedTimeToLocalTime . startTime) ms
  T.putStrLn topSepRow
  printRow (repeat [SetConsoleIntensity BoldIntensity]) headerRow
  forM_ (zip [0 :: Int ..] groupedMeetings) $ \(i, group) -> do
    case group of
      [] -> pure ()
      (g : gs) -> do
        -- Horizontal line
        T.putStrLn $ if i == 0 then doubleMiddleSepRow else middleSepRow
        -- Header row: date is in bold, the rest in the first colour
        let firstRowStyles = case colors of
              Just cs -> [SetConsoleIntensity BoldIntensity] : repeat [SetPaletteColor Foreground (NE.head cs)]
              Nothing -> [SetConsoleIntensity BoldIntensity] : repeat []
        printRow firstRowStyles (makeMeetingRow g)
        -- Remaining rows: alternate between the remaining colours
        let remainingRowStyles = case colors of
              Just cs ->
                let shiftedColors = NE.tail cs ++ [NE.head cs]
                 in cycle $ map (\c -> repeat [SetPaletteColor Foreground c]) shiftedColors
              Nothing -> repeat (repeat [])
        mapM_ (uncurry printRowNoFirst) (zip remainingRowStyles (map makeMeetingRow gs))
  T.putStrLn bottomSepRow

infoPrint :: TimeZone -> Meeting -> Int -> IO ()
infoPrint tz m inPerson =
  let bold = styleText [SetConsoleIntensity BoldIntensity]
      showMeetingBasic = do
        T.putStrLn
          ( "The first timeslot available is:"
              <> "\n - on "
              <> bold (showDateWithDay $ localDay $ zonedTimeToLocalTime $ convertZone tz (startTime m))
              <> "\n - at "
              <> bold (showTime tz (startTime m) (endTime m))
          )
      showMeetingWithRoom r = do
        T.putStrLn
          ( "The first timeslot available with a room size that fits your needs is:"
              <> "\n - on "
              <> bold (showDateWithDay $ localDay $ zonedTimeToLocalTime $ startTime m)
              <> "\n - at "
              <> bold (showTime tz (startTime m) (endTime m))
              <> "\n - in room "
              <> bold (roomEmail r)
          )
   in case (rooms m, inPerson) of
        (_, 0) -> showMeetingBasic
        ([], _) -> showMeetingBasic
        (r : _, _) -> showMeetingWithRoom r

prettyThrow :: Text -> IO a
prettyThrow t = do
  T.hPutStrLn
    stderr
    ( styleText [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity] "Error: "
        <> styleText [SetColor Foreground Vivid Red] t
    )
  exitFailure

prettyWarn :: Text -> IO ()
prettyWarn t = do
  T.hPutStrLn
    stderr
    ( styleText [SetColor Foreground Vivid Yellow, SetConsoleIntensity BoldIntensity] "Warning: "
        <> styleText [SetColor Foreground Vivid Yellow] t
    )
