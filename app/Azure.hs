module Azure (getPerson) where

import Network.HTTP.Req
import Types

getPerson :: String -> IO (Maybe Person)
getPerson = undefined

newtype Token = Token {unToken :: String}

-- | rum
clientId :: String
clientId = "a462354f-fd23-4fdf-94f5-5cce5a6c27c7"

tenantId :: String
tenantId = "4395f4a7-e455-4f95-8a9f-1fbaef6384f9"

data DeviceCodeBody = DeviceCodeBody
  { clientId :: String,
    scope :: String
  }

getToken :: IO (Either String Token)
getToken = do
  let deviceCodeUrl = "https://login.microsoftonline.com/" ++ tenantId ++ "/oauth2/v2.0/devicecode"
  let postBody = DeviceCodeBody clientId "user.read calendars.read.shared"
  respBody <- runReq defaultHttpConfig $ do
    v <- req POST (https "login.microsoftonline.com" /: tenantId /: "oauth2" /: "v2.0" /: "devicecode") (ReqBodyJson postBody) jsonResponse mempty
    pure (responseBody v)

  print respBody
