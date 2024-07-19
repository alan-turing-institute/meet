{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Meet.Azure
  ( getToken,
    getAvailabilityText,
    fetchSchedules,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (void, when)
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
import Meet.Entities (Availability (..), HasSchedule, Minutes (..), Person (..), Room (..), Schedule (..))
import Meet.Print (prettyThrow)
import Network.HTTP.Req
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory)
import System.Posix.Files (setFileMode)
import System.Process (readProcessWithExitCode, spawnCommand)

data Token = Token
  { accessToken :: Text,
    refreshToken :: Text,
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

getTokenJsonFilePath :: IO FilePath
getTokenJsonFilePath = getXdgDirectory XdgCache "meet/token.json"

serialiseToken :: Token -> IO ()
serialiseToken token = do
  fp <- getTokenJsonFilePath
  createDirectoryIfMissing True (takeDirectory fp)
  encodeFile fp token
  setFileMode fp 0o600

deserialiseToken :: IO (Maybe Token)
deserialiseToken = do
  fp <- getTokenJsonFilePath
  exists <- doesFileExist fp
  if not exists
    then pure Nothing
    else do
      now <- getCurrentTime
      getTokenJsonFilePath >>= decodeFileStrict

getTokenFromRefreshToken :: Token -> IO (Maybe Token)
getTokenFromRefreshToken token = do
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

withToken :: Token -> Option 'Https
withToken token = oAuth2Bearer (TE.encodeUtf8 $ accessToken token)

rumClientId :: Text
rumClientId = "a462354f-fd23-4fdf-94f5-5cce5a6c27c7"

rumTenantId :: Text
rumTenantId = "4395f4a7-e455-4f95-8a9f-1fbaef6384f9"

scope :: Text
scope = "user.read calendars.read.shared offline_access"

data DeviceCodeError
  = AuthorisationDeclined
  | AuthorisationPending
  | ExpiredToken
  | BadVerificationCode
  | UnknownResponseError Text
  | AesonError Text
  deriving (Eq, Show)

parseDeviceCodePollResponse :: Value -> UTCTime -> Either DeviceCodeError Token
parseDeviceCodePollResponse val now =
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
  case parseDeviceCodePollResponse (responseBody resp) currentTime of
    Left AuthorisationPending -> do
      threadDelay 250000
      pollForToken code
    Left err -> pure $ Left err
    Right token -> pure $ Right token

data DeviceCodeResponse = DeviceCodeResponse
  { deviceCode :: Text,
    userCode :: Text,
    verificationUrl :: Text,
    _expiresIn :: Int,
    _interval :: Int,
    _message :: Text
  }
  deriving (Eq, Show)

parseDeviceCodeResponse :: Value -> Either String DeviceCodeResponse
parseDeviceCodeResponse = parseEither . withObject "parseDeviceCodeResponse" $ \o -> do
  DeviceCodeResponse
    <$> o .: "device_code"
    <*> o .: "user_code"
    <*> o .: "verification_uri"
    <*> o .: "expires_in"
    <*> o .: "interval"
    <*> o .: "message"

getTokenFromCode :: IO Token
getTokenFromCode = do
  respJson <- runReq defaultHttpConfig $ do
    let url = https "login.microsoftonline.com" /: rumTenantId /: "oauth2" /: "v2.0" /: "devicecode"
    req POST url (ReqBodyUrlEnc ("client_id" =: rumClientId <> "scope" =: scope)) jsonResponse mempty

  let resp = parseDeviceCodeResponse (responseBody respJson)
  case resp of
    Left err -> prettyThrow $ T.pack $ "Failed to parse response from /devicecode: " <> err
    Right deviceCodeResponse -> do
      (copyExitCode, _, _) <- readProcessWithExitCode "pbcopy" [] (T.unpack $ userCode deviceCodeResponse)
      T.putStrLn $ "Please visit " <> verificationUrl deviceCodeResponse <> " and enter code " <> userCode deviceCodeResponse <> "."
      when (copyExitCode == ExitSuccess) $ do
        T.putStrLn $ "The code has also been copied to your clipboard."
      void $ spawnCommand ("open " <> T.unpack (verificationUrl deviceCodeResponse))

      eitherToken <- pollForToken (deviceCode deviceCodeResponse)
      case eitherToken of
        Left err -> prettyThrow $ T.pack $ show err
        Right token -> pure token

getToken :: IO Token
getToken = do
  tokenFromFile <- deserialiseToken
  case tokenFromFile of
    Nothing -> do
      T.putStrLn "No cached token found. Getting new token..."
      newToken <- getTokenFromCode
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
          refreshedToken <- getTokenFromRefreshToken token
          case refreshedToken of
            Just t' -> do
              serialiseToken t'
              pure t'
            Nothing -> do
              T.putStrLn "Failed to refresh cached token. Getting new token..."
              tokenFromCode <- getTokenFromCode
              serialiseToken tokenFromCode
              pure tokenFromCode

data DateTimeTimeZone = DateTimeTimeZone
  { dateTime :: Text,
    timeZone :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON DateTimeTimeZone

dttzFromUTCTime :: UTCTime -> DateTimeTimeZone
dttzFromUTCTime t =
  DateTimeTimeZone
    { dateTime = T.pack $ iso8601Show t,
      timeZone = "UTC"
    }

data SchedulePostBody = SchedulePostBody
  { schedules :: [Text],
    startTime :: DateTimeTimeZone,
    endTime :: DateTimeTimeZone,
    availabilityViewInterval :: Maybe Int -- Minutes
  }
  deriving (Eq, Show, Generic)

instance ToJSON SchedulePostBody

getAvailabilityText ::
  Token ->
  [Person] ->
  [Room] ->
  UTCTime ->
  UTCTime ->
  Minutes ->
  IO ([Either (Person, Text) (Schedule Person)], [Either (Room, Text) (Schedule Room)])
getAvailabilityText token ppl rooms start end itvl = do
  let peopleWithEmails = zip (map personEmail ppl) ppl
  let roomWithEmails = zip (map roomEmail rooms) rooms
  resp <- runReq defaultHttpConfig $ do
    let calendarUrl = https "graph.microsoft.com" /: "v1.0" /: "me" /: "calendar" /: "getSchedule"
    let postBody =
          SchedulePostBody
            { schedules = map fst peopleWithEmails ++ map fst roomWithEmails,
              startTime = dttzFromUTCTime start,
              endTime = dttzFromUTCTime end,
              availabilityViewInterval = Just (unMinutes itvl)
            }
    req POST calendarUrl (ReqBodyJson postBody) jsonResponse (withToken token)
  let entryParser :: Value -> Parser (Text, Either Text Text)
      entryParser = withObject "response.value" $ \o -> do
        email <- o .: "scheduleId"
        availability <- o .:? "availabilityView"
        case availability of
          Just a -> pure $ (email, Right a)
          Nothing -> do
            e <- o .: "error"
            message <- e .: "message"
            pure (email, Left message)
  let parser :: Value -> Parser [(Text, Either Text Text)]
      parser = withObject "calendar getSchedule response" $ \o -> do
        value <- o .: "value"
        V.toList <$> withArray "value" (mapM entryParser) value
  case parseEither parser (responseBody resp) of
    Left err -> prettyThrow $ T.pack err
    Right entries -> do
      -- Need to check that the Azure response contains all the same entities
      -- that we queried for.
      let checkEntry :: (HasSchedule a) => (Text, a) -> Either (a, Text) (Schedule a)
          checkEntry (email, ent) = first ((,) ent) $ case find ((== email) . fst) entries of
            Just (_, availT) -> Schedule ent <$> (availT >>= parseAvailabilityText)
            Nothing -> Left $ "MS Graph API did not return any data for this email. This should not happen."
      pure $ (map checkEntry peopleWithEmails, map checkEntry roomWithEmails)

parseAvailabilityText :: Text -> Either Text [Availability]
parseAvailabilityText = traverse parseChar . T.unpack
  where
    parseChar :: Char -> Either Text Availability
    parseChar c = case c of
      '0' -> Right Free
      '1' -> Right Tentative
      '2' -> Right Busy
      '3' -> Right OutOfOffice
      '4' -> Right WorkingElsewhere
      _ -> Left $ T.snoc "unexpected character returned by MS Graph API: " c

fetchSchedules ::
  Token ->
  [Person] ->
  [Room] ->
  UTCTime ->
  UTCTime ->
  Minutes ->
  IO ([Schedule Person], [Schedule Room])
fetchSchedules token ppl rooms start end itvl = do
  (personEitherSchedules, roomEitherSchedules) <- getAvailabilityText token ppl rooms start end itvl
  let (failurePeople, successPeople) = partitionEithers personEitherSchedules
      (failureRooms, successRooms) = partitionEithers roomEitherSchedules
      failures = map (first personEmail) failurePeople ++ map (first roomEmail) failureRooms
  -- Throw now if any of the emails failed
  when (not $ null failures) $ do
    let errMsg =
          T.intercalate
            "\n"
            ( "Failed to get calendar availability for the following email address(es)."
                : map (\(email, message) -> "<" <> email <> "> (message: " <> message <> ")") failures
            )
    prettyThrow errMsg
  -- Return the parsed schedules
  pure (successPeople, successRooms)
