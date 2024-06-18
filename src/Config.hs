{-# LANGUAGE TypeApplications #-}

module Config (readConfig, peopleAndGroups, Config (..), Group (..)) where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Text (Text, isInfixOf, pack, toLower)
import qualified Data.Text.IO as T
import Data.Yaml (FromJSON (..), (.:))
import qualified Data.Yaml as Y
import Entities (Person (..))
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

peopleAndGroups :: Config -> [Person] -> IO [Person]
peopleAndGroups config ppl = case config of
  Config {groups = []} -> return ppl
  _ -> do
    -- Create a new people list that contains the emails of the groups specified in the list of people.
    -- e.g. if I run `meet jausten friends`, and there's a group called "friends" in the config file with
    -- emails [a@turing.ac.uk, b@turing.ac.uk], we have to change the parsed inputs of
    -- [jausten@turing.ac.uk, friends@turing.ac.uk] to
    -- [jausten@turing.ac.uk, a@turing.ac.uk, b@turing.ac.uk].
    -- To do this, we will check if the group name is an infix of the corresponding person entry,
    -- since "@turing.ac.uk" is appended to the email address if it's not present,
    -- and then add the email addresses of the group to the list of people, filtering out the group name.
    -- TODO: auto-appending "@turing.ac.uk" to config group emails, allowing them to be username-only?
    let lowercaseEmails = map (toLower . personEmail) ppl
        validGroups = filter (\g -> any (isInfixOf (toLower (groupName g))) lowercaseEmails) (groups config)
        validGroupEmails = concatMap groupEmails validGroups
        validGroupNames = map (toLower . groupName) validGroups
        -- We want to filter out the placeholder Person entries that were added to the list of people as a result of the group email check.
        -- However, we want to check if the group name is an infix of the email address, not the other way around (as we did above).
        -- So we define a function that takes two strings and checks if the second string is an infix of the first string,
        -- which is used to filter out the placeholder Person entries.
        -- I originally thought this needed a map to work (which is why I did that), but this seemed to do the job with just `any`.
        -- If there's a cleaner way to do this, please feel free to suggest :')
        isInfixOfBackwards x y = isInfixOf y x
        ppl' = filter (\p -> not $ any (isInfixOfBackwards (toLower (personEmail p))) validGroupNames) ppl ++ map Person validGroupEmails
    if validGroups /= []
      then T.putStrLn "Adding emails from the following groups:" >> mapM_ T.putStrLn validGroupNames >> T.putStrLn " with the following emails:" >> mapM_ T.putStrLn validGroupEmails
      else T.putStrLn "No groups were specified in the list of people."
    return ppl'
