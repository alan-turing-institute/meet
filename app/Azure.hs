module Azure (getPerson, getToken) where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP.Req
import System.Process (spawnCommand)
import Types

getPerson :: String -> IO (Maybe Person)
getPerson = undefined

newtype Token = Token {_unToken :: Text}

instance Show Token where
  show t = T.unpack $ "Token: " <> T.take 10 (_unToken t) <> "..."

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

parseDeviceCodeResponse :: Value -> Parser DeviceCodeResponse
parseDeviceCodeResponse = withObject "parseDeviceCodeResponse" $ \o -> do
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

  let resp = parseMaybe parseDeviceCodeResponse (responseBody respJson)
  case resp of
    Nothing -> pure $ Left "Failed to parse response from /devicecode"
    Just deviceCodeResponse -> do
      T.putStrLn $ "Please visit " <> verificationUrl deviceCodeResponse <> " and enter code " <> userCode deviceCodeResponse <> ". The code has also been copied to your clipboard."
      void $ spawnCommand ("open " <> T.unpack (verificationUrl deviceCodeResponse))
      void $ spawnCommand ("pbcopy <<< " <> T.unpack (userCode deviceCodeResponse))

      eitherToken <- pollForToken (deviceCode deviceCodeResponse)
      pure $ case eitherToken of
        Left err -> Left $ show err
        Right token -> Right token
