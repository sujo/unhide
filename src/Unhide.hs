module Unhide
   ( run
   ) where

import Relude
import Control.Monad (forM_)
import qualified Data.HashMap.Strict as HM
import Data.Text (unpack)
import System.Directory (findExecutable)
import System.Environment (getExecutablePath, getProgName)
import UnliftIO (catch)
import Turtle

import Types
import Config
import Directory (Status(..), detect)
import Helper (calcDirectories, sameDirectoryContents)

-- | encrypts the directory, then removes it.
-- Replaces the package file after encrypting successfully.
encrypt :: FilePath -> FilePath -> Maybe Text -> IO ExitCode
encrypt dir targetFile maybeUserId = do
   let
      tmpFile = targetFile <.> ".new"
      cmd0 = format ("tar -c -C '"%fp%"' ./ | gpg --encrypt --output '"%fp%"'") dir tmpFile
      cmd = maybe cmd0 (\u -> cmd0 <> " --recipient " <> u) maybeUserId
   x <- shell cmd empty
   when (x == ExitSuccess) $
      mv tmpFile targetFile
   pure x


-- | decrypts the file, and creates the directory with contents.
-- Fails if the directory already exists.
-- Does not remove the encrypted file.
decrypt :: FilePath -> FilePath -> IO ExitCode
decrypt file dir = do
   mkdir dir
   shell (format ("gpg --decrypt '"%fp%"' | tar -xC '"%fp%"'") file dir) empty


hideDirectory :: Maybe Text -> FilePath -> FilePath -> IO ()
hideDirectory maybeUserId storageBaseDir dir = do
      (absDirectory, storageDir, targetFile) <- calcDirectories dir storageBaseDir
      status <- Directory.detect absDirectory storageDir targetFile
      case status of
         DsUntracked        -> hide absDirectory storageDir targetFile
         DsSecure           -> printf ("Skipping already secured directory "%fp%"\n") absDirectory
         DsMounted          -> printf ("Skipping already secured and mounted directory "%fp%"\n") absDirectory
         DsMissingLink      -> symlink storageDir absDirectory
         DsNewDir           -> Turtle.die $ format ("Both an encrypted file and a directory exist for "%fp%". Please remove one of them.") absDirectory
         DsNewDirMounted    -> do
            rmdir storageDir
            Turtle.die $ format ("Both an encrypted file and a directory exist for "%fp%". Please remove one of them.") absDirectory
         DsMissingFile      -> Turtle.die $ format ("Missing encrypted file for existing symbolic link at "%fp) absDirectory
         DsMissingDirectory -> Turtle.die $ format ("Missing directory: "%fp) absDirectory

   where
      hide absDirectory storageDir targetFile = do
         encrypt absDirectory targetFile maybeUserId .||.
            Turtle.die "encrypt failed"
         decrypt targetFile storageDir .||.
            Turtle.die "safety check: decrypt failed"
         -- sanity check that the decrypted content is the same
         sameDirectoryContents absDirectory storageDir .||.
            Turtle.die "safety check failed: decrypted content does not match original content"
         rmtree absDirectory
         symlink storageDir absDirectory
         rmtree storageDir


linkCommand :: Command -> IO ()
linkCommand command = do
   binDir <- getBinDir
   unhideExePath <- getExecutablePath
   catch (symlink unhideExePath $ binDir </> unpack command) $ \(_ :: SomeException) -> pure ()


run :: IO ()
run = do
   progName <- getProgName
   if progName == "unhide"
      then do
         args <- getArgs
         case args of
            "run":command:cmdArgs -> runCommand (repr command) $ fmap repr cmdArgs
            _                    -> runSelf
      else do
         cmdArgs <- getArgs
         runCommand (repr progName) $ fmap repr cmdArgs


initializeCommand c cc = do
   forM_ (directories cc) $ hideDirectory (userId cc) (storageBase cc)
   ccs <- loadCommands
   exe <- findExecutable $ unpack c
   if isNothing exe
      then eprintf ("command not found in path: "%s) c
      else do
         saveCommands $ HM.insert c (cc {origPath = exe}) ccs
         linkCommand c


restoreDir :: FilePath -> IO ()
restoreDir dir = do
      (absDirectory, storageDir, targetFile) <- calcDirectories dir storageDefault
      status <- Directory.detect absDirectory storageDir targetFile
      case status of
         DsUntracked        -> Turtle.die $ format ("The directory "%fp%" is has not been secured before.") absDirectory
         DsSecure           -> do
            rm absDirectory
            restore absDirectory targetFile
         DsMounted          -> do
            rm absDirectory
            rmtree storageDir
            restore absDirectory targetFile
         DsMissingLink      -> restore absDirectory targetFile
         DsMissingLinkMounted -> do
            rmtree storageDir
            restore absDirectory targetFile
         DsMissingFile      -> Turtle.die $ format ("Missing encrypted file for existing symbolic link at "%fp) absDirectory
         DsMissingDirectory -> Turtle.die $ format ("Missing directory: "%fp) absDirectory
   where
      restore absDirectory targetFile = do
         ex <- decrypt targetFile absDirectory
         case ex of
            ExitSuccess -> rm targetFile
            ExitFailure _ -> do
               rmdir absDirectory
               Turtle.die (format ("unable to decrypt file "%fp) targetFile)


runSelf :: IO ()
runSelf = do
      config <- options "Hide a directory in an encrypted file" configOpts
      case config of
         OptionsInit c cc   -> initializeCommand c cc
         OptionsRestore dir -> restoreDir dir


runCommand :: Command -> [Text] -> IO ()
runCommand command args = do
   ccs <- loadCommands
   let
      maybecc = HM.lookup command ccs
   case maybecc of
      Nothing -> Turtle.die . repr $ format ("command '"%s%"'not found in configuration") command
      Just cc -> do
         x <- go cc (directories cc)
         when (x == ExitSuccess) $ linkCommand command
   where
      go cc [] =
         case origPath cc of
            Nothing -> do
               eprintf ("missing executable path for "%s) command
               pure $ ExitFailure 1
            Just path -> do
               proc (repr path) args empty
      go cc (dir:dirs) = do
         (_absDirectory, storageDir, targetFile) <- calcDirectories dir (storageBase cc)
         x <- decrypt targetFile storageDir
         if x /= ExitSuccess then do
            eprintf ("decrypt failed: "%fp) dir
            rmtree storageDir
            pure x
         else do

            xx <- go cc dirs
            
            case xx of
               ExitSuccess -> do
                  xxx <- encrypt storageDir targetFile (userId cc)
                  unless (xxx == ExitSuccess) $ eprintf ("encrypt failed: "%fp) dir
               ExitFailure r -> eprintf ("command failed: "%w) r
            rmtree storageDir
            pure xx
