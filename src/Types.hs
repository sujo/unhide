module Types where

import Relude

data CommandConfig = CommandConfig
   { origPath :: Maybe FilePath
   , directories :: [FilePath]
   , storageBase :: FilePath
   , userId :: Maybe Text
   } deriving Generic

type KnownCommands = HashMap Text CommandConfig
type Command = Text
