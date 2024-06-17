{-# LANGUAGE TypeApplications #-}

module Config (readConfig, Config (..), Group (..)) where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Text (Text, pack)
import qualified Data.Text.IO as T
import Data.Yaml (FromJSON (..), (.:))
import qualified Data.Yaml as Y
import Text.RawString.QQ

data Group = Group
  { groupName :: Text,
    groupEmails :: [Text]
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

readConfig :: FilePath -> IO Config
readConfig cp = do
  result <- Y.decodeFileEither @Config cp
  case result of
    Left e -> do
      T.putStrLn $ "Error parsing config: " <> pack (show e)
      -- Return default (empty for now) config if parsing fails.
      return Config {groups = []}
    Right config -> return config
