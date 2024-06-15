{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Azure
  ( getToken,
    getTokenThrow,
    getAvailabilityText,
    getAvailabilityTextThrow,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (void, when)
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import Data.Time.Clock
import Data.Time.Format.ISO8601
import qualified Data.Vector as V
import GHC.Generics
import Network.HTTP.Req
import Print (prettyThrow)
import System.Process (spawnCommand)
import Types (Availability (..), Schedule (..), Minutes (..), getEntity)
import Utils (partitionTupledEither)

newtype Token = Token {_unToken :: Text}

instance Show Token where
  show t = T.unpack $ "Token: " <> T.take 10 (_unToken t) <> "..."

withToken :: Token -> Option 'Https
withToken token = oAuth2Bearer (TE.encodeUtf8 $ _unToken token)

rumClientId :: Text
rumClientId = "a462354f-fd23-4fdf-94f5-5cce5a6c27c7"

rumTenantId :: Text
rumTenantId = "4395f4a7-e455-4f95-8a9f-1fbaef6384f9"

data DeviceCodeError
  = AuthorisationDeclined
  | AuthorisationPending
  | ExpiredToken
  | BadVerificationCode
  | UnknownResponseError Text
  | AesonError Text
  deriving (Eq, Show)

parseDeviceCodePollResponse :: Value -> Either DeviceCodeError Token
parseDeviceCodePollResponse val =
  let parser :: Value -> Parser (Either DeviceCodeError Token)
      parser = withObject "DeviceCodeResponse" $ \o -> do
        errorText <- o .:? "error"
        tokenText <- o .:? "access_token"
        pure $ case (tokenText, errorText) of
          (Just token, _) -> Right $ Token token
          (_, Just "authorization_pending") -> Left AuthorisationPending
          (_, Just "authorization_declined") -> Left AuthorisationDeclined
          (_, Just "expired_token") -> Left ExpiredToken
          (_, Just "bad_verification_code") -> Left BadVerificationCode
          (_, Just err) -> Left $ UnknownResponseError err
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
  case parseDeviceCodePollResponse (responseBody resp) of
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

getToken :: IO (Either Text Token)
getToken = do
  respJson <- runReq defaultHttpConfig $ do
    let url = https "login.microsoftonline.com" /: rumTenantId /: "oauth2" /: "v2.0" /: "devicecode"
    req POST url (ReqBodyUrlEnc ("client_id" =: rumClientId <> "scope" =: ("user.read calendars.read.shared" :: Text))) jsonResponse mempty

  let resp = parseDeviceCodeResponse (responseBody respJson)
  case resp of
    Left err -> pure $ Left $ T.pack $ "Failed to parse response from /devicecode: " <> err
    Right deviceCodeResponse -> do
      T.putStrLn $ "Please visit " <> verificationUrl deviceCodeResponse <> " and enter code " <> userCode deviceCodeResponse <> ". The code has also been copied to your clipboard."
      void $ spawnCommand ("open " <> T.unpack (verificationUrl deviceCodeResponse))
      void $ spawnCommand ("pbcopy <<< " <> T.unpack (userCode deviceCodeResponse))

      eitherToken <- pollForToken (deviceCode deviceCodeResponse)
      pure $ case eitherToken of
        Left err -> Left $ T.pack $ show err
        Right token -> Right token

getTokenThrow :: IO Token
getTokenThrow = do
  eitherToken <- getToken
  case eitherToken of
    Left err -> do
      prettyThrow err
    Right token -> pure token

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

getAvailabilityText :: Token -> [Text] -> UTCTime -> UTCTime -> Minutes -> IO [(Text, Either Text Text)]
getAvailabilityText token emails start end itvl = do
  resp <- runReq defaultHttpConfig $ do
    let calendarUrl = https "graph.microsoft.com" /: "v1.0" /: "me" /: "calendar" /: "getSchedule"
    let postBody =
          SchedulePostBody
            { schedules = emails,
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
    Left err -> error err
    Right entries -> pure entries

parseAvailabilityText :: Text -> [Availability]
parseAvailabilityText = map parseChar . T.unpack
  where
    parseChar :: Char -> Availability
    parseChar c = case c of
      '0' -> Free
      '1' -> Tentative
      '2' -> Busy
      '3' -> OutOfOffice
      '4' -> WorkingElsewhere
      _ -> error $ "unexpected character returned by MS Graph API: " ++ [c]

toSchedule :: (Text, Text) -> Schedule
toSchedule t =
  Schedule
    { entity = getEntity (fst t),
      schedule = parseAvailabilityText (snd t)
    }

getAvailabilityTextThrow :: Token -> [Text] -> UTCTime -> UTCTime -> Minutes -> IO [Schedule]
getAvailabilityTextThrow token emails start end itvl = do
  strings <- getAvailabilityText token emails start end itvl
  let (successStrings, failureStrings) = partitionTupledEither strings
  -- Throw now if any of the emails failed
  when (not $ null failureStrings) $ do
    let errMsg =
          T.intercalate
            "\n"
            ( "Failed to get availability for the following email addresses."
                : map (\(email, message) -> " - " <> email <> " (Message: " <> message <> ")") failureStrings
            )
    prettyThrow errMsg
  -- Return the parsed schedules
  pure $ map toSchedule successStrings
