{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Meet.Azure
  ( getToken,
    fetchSchedules,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (unless, void, when)
import Data.Aeson
import Data.Aeson.Types
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Data.Vector as V
import GHC.Generics
import Meet.Entities (Availability (..), HasSchedule (..), Minutes (..), Person (..), Room (..), Schedule (..))
import Meet.Print (prettyThrow)
import Network.HTTP.Req
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory)
import System.Posix.Files (setFileMode)
import System.Process (readProcessWithExitCode, spawnCommand)

-- * OAuth2 token management

-- | Representation of an OAuth2 token from Azure AD.
data Token = Token
  { -- | The token itself, used to query the API
    accessToken :: Text,
    -- | The refresh token, which can be used to get a new access token
    refreshToken :: Text,
    -- | The time at which the token expires
    expiresAt :: UTCTime
  }
  deriving (Eq, Generic)

instance FromJSON Token

instance ToJSON Token

instance Show Token where
  show t =
    T.unpack $
      T.concat
        ["Access token: ", T.take 10 (accessToken t), "...; Refresh token: ", T.take 10 (refreshToken t), "...; Expires at: ", T.pack (show $ expiresAt t)]

-- | The token is cached at ~/.cache/meet/token.json (on Unix systems).
getTokenJsonFilePath :: IO FilePath
getTokenJsonFilePath = getXdgDirectory XdgCache "meet/token.json"

-- | Serialise the token to the cache file.
serialiseToken :: Token -> IO ()
serialiseToken token = do
  fp <- getTokenJsonFilePath
  createDirectoryIfMissing True (takeDirectory fp)
  encodeFile fp token
  setFileMode fp 0o600

-- | Deserialise the token from the cache file.
deserialiseToken :: IO (Maybe Token)
deserialiseToken = do
  fp <- getTokenJsonFilePath
  exists <- doesFileExist fp
  if not exists
    then pure Nothing
    else do
      now <- getCurrentTime
      getTokenJsonFilePath >>= decodeFileStrict

-- | Refresh a token (usually an expired one, but technically it doesn't have to
-- be) using the refresh token stored inside it.
refresh :: Token -> IO (Maybe Token)
refresh token = do
  let httpConfigNoThrow = defaultHttpConfig {httpConfigCheckResponse = \_ _ _ -> Nothing}
  resp <- runReq httpConfigNoThrow $ do
    let tokenUrl = https "login.microsoftonline.com" /: rumTenantId /: "oauth2" /: "v2.0" /: "token"
    let body =
          "grant_type" =: ("refresh_token" :: Text)
            <> "scope" =: scope
            <> "client_id" =: rumClientId
            <> "refresh_token" =: refreshToken token
    req POST tokenUrl (ReqBodyUrlEnc body) jsonResponse mempty
  now <- getCurrentTime
  let parser :: Value -> Parser (Either Text Token)
      parser = withObject "TokenRefreshResponse" $ \o -> do
        accessTokenText <- o .:? "access_token"
        refreshTokenText <- o .:? "refresh_token"
        expiresIn :: Maybe Int <- o .:? "expires_in" -- Seconds
        errorText <- o .:? "error"
        pure $ case (accessTokenText, refreshTokenText, expiresIn, errorText) of
          (Just at, Just rt, Just ei, _) ->
            -- Take off a minute to be safe
            let expiresAt = addUTCTime (fromIntegral ei - 60) now
             in Right $ Token at rt expiresAt
          (_, _, _, Just err) -> Left err
  pure $ case parseEither parser (responseBody resp) of
    Right (Right token) -> Just token
    _ -> Nothing

-- | Attach an active token to a HTTPS request.
withToken :: Token -> Option 'Https
withToken token = oAuth2Bearer (TE.encodeUtf8 $ accessToken token)

-- * Device code flow

-- See:
-- https://learn.microsoft.com/en-us/entra/identity-platform/v2-oauth2-device-code

-- ** Device code, part 1: Getting the device code

-- The first step is to get the device code itself. This is done by querying a
-- static Azure URL.

-- | Azure client ID, taken from https://github.com/alan-turing-institute/rum.
-- Getting one of these requires registering an app in the Azure portal, which
-- in turn requires permission from IT.
rumClientId :: Text
rumClientId = "a462354f-fd23-4fdf-94f5-5cce5a6c27c7"

-- | Azure tenant ID for the ATI. Taken from
-- https://github.com/alan-turing-institute/rum
rumTenantId :: Text
rumTenantId = "4395f4a7-e455-4f95-8a9f-1fbaef6384f9"

-- | The scopes that the token will have access to.
--
-- Note that these only allow read access to calendars, so the app cannot be
-- used to actually create a meeting. For a discussion of extra permissions that
-- would be helpful, see
-- https://github.com/alan-turing-institute/meet/issues/10. However, these
-- permissions would need to be granted by IT.
--
-- `offline_access` is required to get a refresh token.
scope :: Text
scope = "user.read calendars.read.shared offline_access"

-- | The response from the Azure API when requesting a device code.
data DeviceCode = DeviceCode
  { -- | The device code itself, used to poll for the token.
    deviceCode :: Text,
    -- | The user code is the string that the user must enter on the website.
    userCode :: Text,
    -- | The URL that the user must visit to enter the user code.
    verificationUrl :: Text,
    _expiresIn :: Int,
    _interval :: Int,
    _message :: Text
  }
  deriving (Eq, Show)

-- | Parse the JSON response from the device code request.
parseDeviceCode :: Value -> Either String DeviceCode
parseDeviceCode = parseEither . withObject "parseDeviceCodeResponse" $ \o -> do
  DeviceCode
    <$> o .: "device_code"
    <*> o .: "user_code"
    <*> o .: "verification_uri"
    <*> o .: "expires_in"
    <*> o .: "interval"
    <*> o .: "message"

-- | Request a device code from the /v2.0/devicecode endpoint.
getDeviceCode :: IO (Either String DeviceCode)
getDeviceCode = do
  respJson <- runReq defaultHttpConfig $ do
    let url = https "login.microsoftonline.com" /: rumTenantId /: "oauth2" /: "v2.0" /: "devicecode"
    req POST url (ReqBodyUrlEnc ("client_id" =: rumClientId <> "scope" =: scope)) jsonResponse mempty
  pure $ parseDeviceCode (responseBody respJson)

-- ** Device code, part 2: Getting the token

-- Once the user has signed in on the website and entered the user code, we can
-- obtain a token. We do this by repeatedly polling the Azure API using the
-- device code obtained previously.

-- | The possible ways in which the device code flow can fail (or, strictly
-- speaking, not succeed, since `AuthorisationPending` means it has not _yet_
-- succeeded).
data DeviceCodeError
  = AuthorisationDeclined
  | AuthorisationPending
  | ExpiredToken
  | BadVerificationCode
  | -- | Should not happen; indicates a malformed response from Azure
    UnknownResponseError Text
  | -- | Should not happen; indicates a malformed response from Azure
    AesonError Text
  deriving (Eq, Show)

-- | Parse the JSON response from polling the token endpoint.
parsePollResponse :: Value -> UTCTime -> Either DeviceCodeError Token
parsePollResponse val now =
  let parser :: Value -> Parser (Either DeviceCodeError Token)
      parser = withObject "DeviceCodeResponse" $ \o -> do
        accessTokenText <- o .:? "access_token"
        refreshTokenText <- o .:? "refresh_token"
        expiresIn :: Maybe Int <- o .:? "expires_in" -- Seconds
        errorText <- o .:? "error"
        pure $ case (accessTokenText, refreshTokenText, expiresIn, errorText) of
          (Just at, Just rt, Just ei, _) ->
            -- Take off a minute to be safe
            let expiresAt = addUTCTime (fromIntegral ei - 60) now
             in Right $ Token at rt expiresAt
          (_, _, _, Just "authorization_pending") -> Left AuthorisationPending
          (_, _, _, Just "authorization_declined") -> Left AuthorisationDeclined
          (_, _, _, Just "expired_token") -> Left ExpiredToken
          (_, _, _, Just "bad_verification_code") -> Left BadVerificationCode
          (_, _, _, Just err) -> Left $ UnknownResponseError err
          _ -> Left $ UnknownResponseError "Unknown response"
   in case parseEither parser val of
        Left err -> Left $ AesonError (T.pack err)
        Right (Left err) -> Left err
        Right (Right token) -> Right token

-- | Poll the /v2.0/token endpoint to see whether the device code has been
-- entered.
pollForToken :: Text -> IO (Either DeviceCodeError Token)
pollForToken code = do
  let httpConfigNoThrow = defaultHttpConfig {httpConfigCheckResponse = \_ _ _ -> Nothing}
  resp <- runReq httpConfigNoThrow $ do
    let poll_url = https "login.microsoftonline.com" /: rumTenantId /: "oauth2" /: "v2.0" /: "token"
    let body =
          "grant_type" =: ("urn:ietf:params:oauth:grant-type:device_code" :: Text)
            <> "client_id" =: rumClientId
            <> "device_code" =: code
    req POST poll_url (ReqBodyUrlEnc body) jsonResponse mempty
  currentTime <- getCurrentTime
  case parsePollResponse (responseBody resp) currentTime of
    Left AuthorisationPending -> do
      threadDelay 250000
      pollForToken code
    Left err -> pure $ Left err
    Right token -> pure $ Right token

-- * Put it all together

-- | Get a token using the device code flow.
getTokenViaDeviceCode :: IO Token
getTokenViaDeviceCode = do
  eitherCode <- getDeviceCode
  case eitherCode of
    Left err -> prettyThrow $ T.pack $ "Failed to parse response from /devicecode: " <> err
    Right code -> do
      (copyExitCode, _, _) <- readProcessWithExitCode "pbcopy" [] (T.unpack $ userCode code)
      T.putStrLn $ "Please visit " <> verificationUrl code <> " and enter code " <> userCode code <> "."
      when (copyExitCode == ExitSuccess) $ do
        T.putStrLn "The code has also been copied to your clipboard."
      void $ spawnCommand ("open " <> T.unpack (verificationUrl code))
      eitherToken <- pollForToken (deviceCode code)
      case eitherToken of
        Left err -> prettyThrow $ T.pack $ show err
        Right token -> pure token

-- | Get a token, attempting the following methods in sequence:
--
-- 1. Deserialising a cached token and using it if it is still valid.
-- 2. Refreshing the cached token if it has expired.
-- 3. Getting a new token using the device code flow.
getToken :: IO Token
getToken = do
  tokenFromFile <- deserialiseToken
  case tokenFromFile of
    Nothing -> do
      T.putStrLn "No cached token found. Getting new token..."
      newToken <- getTokenViaDeviceCode
      serialiseToken newToken
      pure newToken
    Just token -> do
      now <- getCurrentTime
      if expiresAt token > now
        then do
          T.putStrLn "Cached token is still valid, reusing..."
          pure token
        else do
          T.putStrLn "Cached token has expired. Refreshing..."
          refreshedToken <- refresh token
          case refreshedToken of
            Just t' -> do
              serialiseToken t'
              pure t'
            Nothing -> do
              T.putStrLn "Failed to refresh cached token. Getting new token..."
              tokenFromCode <- getTokenViaDeviceCode
              serialiseToken tokenFromCode
              pure tokenFromCode

-- * Calendar availability API

-- | Azure API representation of a date-time with a time zone.
--
-- https://learn.microsoft.com/en-us/graph/api/resources/datetimetimezone?view=graph-rest-1.0
data DateTimeTimeZone = DateTimeTimeZone
  { dateTime :: Text,
    timeZone :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON DateTimeTimeZone

-- | Convert a 'UTCTime' to a 'DateTimeTimeZone' with the time zone set to
-- "UTC".
dttzFromUTCTime :: UTCTime -> DateTimeTimeZone
dttzFromUTCTime t =
  DateTimeTimeZone
    { dateTime = T.pack $ iso8601Show t,
      timeZone = "UTC"
    }

-- | POST body for the /v1.0/me/calendar/getSchedule endpoint.
data SchedulePostBody = SchedulePostBody
  { schedules :: [Text],
    startTime :: UTCTime,
    endTime :: UTCTime,
    availabilityViewInterval :: Minutes
  }
  deriving (Eq, Show, Generic)

instance ToJSON SchedulePostBody where
  toJSON (SchedulePostBody schedules startTime endTime availabilityViewInterval) =
    object
      [ "schedules" .= schedules,
        "startTime" .= dttzFromUTCTime startTime,
        "endTime" .= dttzFromUTCTime endTime,
        "availabilityViewInterval" .= unMinutes availabilityViewInterval
      ]

-- | Responses from the Azure API that indicate an error fetching a schedule for
-- a particular entity.
data ScheduleError ent = ScheduleError
  { -- | Person or Room
    errorEntity :: ent,
    errorMessage :: Text
  }
  deriving (Eq, Show)

-- | Parse the availability text returned by the MS Graph API. The text is a
-- string containing one number per interval, where the number represents the
-- availability status of that entity.
availabilityParser :: Value -> Parser (Text, Either Text [Availability])
availabilityParser = withObject "response.value" $ \o -> do
  -- Pure function to parse a single character of the availability string.
  let parseAvailabilityChar :: Char -> Either Text Availability
      parseAvailabilityChar c = case c of
        '0' -> Right Free
        '1' -> Right Tentative
        '2' -> Right Busy
        '3' -> Right OutOfOffice
        '4' -> Right WorkingElsewhere
        _ -> Left $ T.snoc "unexpected character returned by MS Graph API: " c
  -- Run parsing on JSON response
  email <- o .: "scheduleId"
  maybeAvailabilityText <- o .:? "availabilityView"
  case maybeAvailabilityText of
    Just a -> case traverse parseAvailabilityChar a of
      Left error -> pure (email, Left error)
      Right avails -> pure (email, Right avails)
    Nothing -> do
      e <- o .: "error"
      message <- e .: "message"
      pure (email, Left message)

-- | Parse the actual API response which contains availability texts for
-- multiple entities at the same time.
availabilitiesParser :: Value -> Parser [(Text, Either Text [Availability])]
availabilitiesParser = withObject "calendar getSchedule response" $ \o -> do
  value <- o .: "value"
  V.toList <$> withArray "value" (mapM availabilityParser) value

-- | Fetch the schedules for a list of people and rooms, returning either a
-- ScheduleError or a Schedule for each of them.
--
-- We have to bundle the people and rooms together (rather than using e.g. mapM
-- over a list of people and rooms) because that lets us make a single request
-- to the Azure API, which is more efficient.
getSchedulesWithErrors ::
  [Person] ->
  [Room] ->
  UTCTime ->
  UTCTime ->
  Minutes ->
  IO ([Either (ScheduleError Person) (Schedule Person)], [Either (ScheduleError Room) (Schedule Room)])
getSchedulesWithErrors ppl rooms start end itvl = do
  token <- getToken
  resp <- runReq defaultHttpConfig $ do
    let calendarUrl = https "graph.microsoft.com" /: "v1.0" /: "me" /: "calendar" /: "getSchedule"
    let postBody =
          SchedulePostBody
            { schedules = map getEmail ppl <> map getEmail rooms,
              startTime = start,
              endTime = end,
              availabilityViewInterval = itvl
            }
    req POST calendarUrl (ReqBodyJson postBody) jsonResponse (withToken token)
  case parseEither availabilitiesParser (responseBody resp) of
    Left err -> prettyThrow $ T.pack err
    Right entries -> do
      -- Need to check that the Azure response contains all the same entities
      -- that we queried for.
      let checkEntry :: (HasSchedule ent) => ent -> Either (ScheduleError ent) (Schedule ent)
          checkEntry ent = case find ((== getEmail ent) . fst) entries of
            Just (_, Right avails) -> Right $ Schedule ent avails
            Just (_, Left err) -> Left $ ScheduleError ent err
            Nothing -> Left $ ScheduleError ent "MS Graph API did not return any data for this email. This should not happen."
      pure (map checkEntry ppl, map checkEntry rooms)

-- | Throw errors based on any failed schedule fetching.
throwScheduleErrors :: [ScheduleError Person] -> [ScheduleError Room] -> IO ()
throwScheduleErrors peopleErrors roomErrors = do
  let errMsgPeople =
        T.intercalate
          "\n"
          ( "Failed to get calendar availability for the following person(s)."
              : map (\fs -> "<" <> getEmail (errorEntity fs) <> "> (message: " <> errorMessage fs <> ")") peopleErrors
          )
      errMsgRooms =
        T.intercalate
          "\n"
          ( "Failed to get calendar availability for the following room(s)."
              : map (\fs -> "<" <> getEmail (errorEntity fs) <> "> (message: " <> errorMessage fs <> ")") roomErrors
          )
  prettyThrow $ T.strip (errMsgPeople <> "\n" <> errMsgRooms)

-- | Fetch schedules from the Azure API for a list of people and rooms.
fetchSchedules ::
  [Person] ->
  [Room] ->
  UTCTime ->
  UTCTime ->
  Minutes ->
  IO ([Schedule Person], [Schedule Room])
fetchSchedules ppl rooms start end itvl = do
  (personEitherSchedules, roomEitherSchedules) <- getSchedulesWithErrors ppl rooms start end itvl
  let (failurePeople, successPeople) = partitionEithers personEitherSchedules
      (failureRooms, successRooms) = partitionEithers roomEitherSchedules
  unless (null failurePeople && null failureRooms) $
    throwScheduleErrors failurePeople failureRooms
  pure (successPeople, successRooms)
