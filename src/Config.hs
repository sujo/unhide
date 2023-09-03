module Config
   ( CommandConfig(..)
   , Options(..)
   , KnownCommands
   , configOpts
   , getBinDir
   , getConfigDir
   , loadCommands
   , saveCommands
   , storageDefault
   ) where

import Relude
import qualified Data.Aeson.Types as Yaml
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as HM
import Data.Maybe (Maybe(..), maybe)
import qualified Data.Yaml as Yaml
import GHC.Generics
import System.Directory (getAppUserDataDirectory)
import Turtle

import Types

instance Yaml.ToJSON CommandConfig where
   toJSON = Yaml.genericToJSON Yaml.defaultOptions
   toEncoding = Yaml.genericToEncoding Yaml.defaultOptions

instance Yaml.FromJSON CommandConfig

data Options = OptionsInit Command CommandConfig
             | OptionsRun Command [Text]
             | OptionsCleanUp { cleanupStorageBase :: FilePath }
             | OptionsRestore { restoreDirectory :: FilePath }

storageDefault = "/dev/shm" :: FilePath

getConfigDir :: IO FilePath
getConfigDir = do
   configDir <- getAppUserDataDirectory "unhide"
   mktree configDir
   pure configDir

getCommandsFile = (</> "commands.yaml") <$> getConfigDir :: IO FilePath

getBinDir :: IO FilePath
getBinDir = do
   configDir <- getConfigDir
   let
      d = configDir </> "bin"
   mktree d
   pure d


initOpts :: Parser Options
initOpts = OptionsInit <$> optText "command" 'c' "The wrapped command"
      <*> (CommandConfig Nothing
               <$> some (optPath "directory" 'd' "The directory location used by the command")
               <*> (optPath "storage" 's' "Temporary storage location. Should be a ramfs or tmpfs mountpoint." <|> pure storageDefault)
               <*> optional (optText "userid" 'u' "The GnuPG key id for encryption"))


runOpts :: Parser Options
runOpts = OptionsRun
            <$> argText "command" "The wrapped command"
            <*> some (argText "args" "command options")

restoreOpts :: Parser Options
restoreOpts = OptionsRestore <$> optPath "directory" 'd' "The directory location used by the command"


configOpts :: Parser Options
configOpts =
   subcommandGroup "Available actions:"
      [ ("init", "Move a directory into a secure storage", initOpts)
      , ("run", "Run a command after making the encrypted directory available temporarily", runOpts)
      , ("restore", "Restore an unencrypted directory and remove the encrypted file", restoreOpts)
      ]
                             --, ("cleanup", "Clean up a left-over unencrypted directory", cleanupOpts)
                             --, ("deinit", "Restore an unencrypted directory", deinitOpts)

saveCommands :: KnownCommands -> IO ()
saveCommands ccs = do
   commandsFile <- getCommandsFile
   B.writeFile commandsFile . Yaml.encode $ ccs


loadCommands :: IO KnownCommands
loadCommands = do
   configDir <- getConfigDir
   let
      fname = configDir </> "commands.yaml"
   exists <- testfile fname
   if exists then do
      bytes <- B.readFile fname
      case Yaml.decodeEither' bytes of
         Left ex -> do
            commandsFile <- getCommandsFile
            eprintf ("Error reading commands configuration from "%fp%": "%w) commandsFile $ Yaml.prettyPrintParseException ex
            pure HM.empty
         Right a  -> pure a
   else pure HM.empty
