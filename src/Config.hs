{-# LANGUAGE TypeApplications #-}

module Config (Config (..)) where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Text (Text, pack)
import qualified Data.Text.IO as T
import Data.Yaml (FromJSON (..), (.:))
import qualified Data.Yaml as Y
import Text.RawString.QQ

data Group = Group
  { name :: Text,
    emails :: [Text]
  }
  deriving (Eq, Show)

instance FromJSON Group where
  parseJSON = Y.withObject "Group" $ \v ->
    Group
      <$> v .: "name"
      <*> v .: "emails"

newtype Config = Config
  { groups :: [Group]
  }
  deriving (Eq, Show)

instance FromJSON Config where
  parseJSON = Y.withObject "Config" $ \v ->
    Config <$> v .: "groups"

readConfig :: IO (Maybe Config)
readConfig = do
  result <- Y.decodeFileEither @Config "~/.meet-config.yaml"
  case result of
    Left e -> do
      T.putStrLn $ "Error parsing config: " <> pack (show e)
      return Nothing
    Right config -> return (Just config)
