{-# LANGUAGE DeriveGeneric #-}

module Azure (getPerson, getToken) where

import Data.Aeson
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Req
import Types

getPerson :: String -> IO (Maybe Person)
getPerson = undefined

newtype Token = Token {unToken :: Text}

rumClientId :: Text
rumClientId = "a462354f-fd23-4fdf-94f5-5cce5a6c27c7"

rumTenantId :: Text
rumTenantId = "4395f4a7-e455-4f95-8a9f-1fbaef6384f9"

getToken :: IO (Either String Token)
getToken = do
  resp <- runReq defaultHttpConfig $ do
    let url = https "login.microsoftonline.com" /: rumTenantId /: "oauth2" /: "v2.0" /: "devicecode"
    req POST url (ReqBodyUrlEnc ("client_id" =: rumClientId <> "scope" =: ("user.read calendars.read.shared" :: Text))) jsonResponse mempty
  print (responseBody resp :: Value)

  pure $ Left "oops"
