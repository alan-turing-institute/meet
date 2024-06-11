{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Azure
  ( getToken,
    printAuthenticatedUserName,
    getAvailabilityString,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import Data.Time.Clock
import Data.Time.Format.ISO8601
import Data.Time.LocalTime
import qualified Data.Vector as V
import GHC.Generics
import Network.HTTP.Req
import System.Environment (setEnv)
import System.Process (spawnCommand)
import Types (Person)

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

getToken :: IO (Either String Token)
getToken = do
  respJson <- runReq defaultHttpConfig $ do
    let url = https "login.microsoftonline.com" /: rumTenantId /: "oauth2" /: "v2.0" /: "devicecode"
    req POST url (ReqBodyUrlEnc ("client_id" =: rumClientId <> "scope" =: ("user.read calendars.read.shared" :: Text))) jsonResponse mempty

  let resp = parseDeviceCodeResponse (responseBody respJson)
  case resp of
    Left err -> pure $ Left $ "Failed to parse response from /devicecode: " <> err
    Right deviceCodeResponse -> do
      T.putStrLn $ "Please visit " <> verificationUrl deviceCodeResponse <> " and enter code " <> userCode deviceCodeResponse <> ". The code has also been copied to your clipboard."
      void $ spawnCommand ("open " <> T.unpack (verificationUrl deviceCodeResponse))
      void $ spawnCommand ("pbcopy <<< " <> T.unpack (userCode deviceCodeResponse))

      eitherToken <- pollForToken (deviceCode deviceCodeResponse)
      pure $ case eitherToken of
        Left err -> Left $ show err
        Right token -> Right token

printAuthenticatedUserName :: Token -> IO ()
printAuthenticatedUserName token = do
  let profile_url = https "graph.microsoft.com" /: "v1.0" /: "me"
  resp <- runReq defaultHttpConfig $ do
    req GET profile_url NoReqBody jsonResponse (withToken token)
  print (responseBody resp :: Value)

data DateTimeTimeZone = DateTimeTimeZone
  { dateTime :: Text,
    timeZone :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON DateTimeTimeZone

dttzFromUTCTime :: UTCTime -> IO DateTimeTimeZone
dttzFromUTCTime t = do
  now <- getCurrentTime
  setEnv "TZ" "Europe/London"
  tz <- getTimeZone now
  pure $
    DateTimeTimeZone
      { dateTime = T.pack $ iso8601Show (utcToLocalTime tz t),
        timeZone = "Europe/London"
      }

data SchedulePostBody = SchedulePostBody
  { schedules :: [Text],
    startTime :: DateTimeTimeZone,
    endTime :: DateTimeTimeZone,
    availabilityViewInterval :: Maybe Int -- Minutes
  }
  deriving (Eq, Show, Generic)

instance ToJSON SchedulePostBody

getAvailabilityString :: Token -> [Text] -> UTCTime -> UTCTime -> IO [(String, String)]
getAvailabilityString token emails start end = do
  start' <- dttzFromUTCTime start
  end' <- dttzFromUTCTime end
  resp <- runReq defaultHttpConfig $ do
    let calendarUrl = https "graph.microsoft.com" /: "v1.0" /: "me" /: "calendar" /: "getSchedule"
    let postBody =
          SchedulePostBody
            { schedules = emails,
              startTime = start',
              endTime = end',
              availabilityViewInterval = Just 30
            }
    req POST calendarUrl (ReqBodyJson postBody) jsonResponse (withToken token)
  let entryParser :: Value -> Parser (String, String)
      entryParser = withObject "response.value" $ \o -> do
        email <- o .: "scheduleId"
        availability <- o .: "availabilityView"
        pure (email, availability)
  let parser :: Value -> Parser [(String, String)]
      parser = withObject "calendar getSchedule response" $ \o -> do
        value <- o .: "value"
        V.toList <$> withArray "value" (mapM entryParser) value
  case parseEither parser (responseBody resp) of
    Left err -> error err
    Right entries -> pure entries
