module Unhide
   ( run
   ) where

import Relude
import Control.Monad (forM_)
import qualified Data.HashMap.Strict as HM
import Data.Text (pack, unpack)
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


-- | decrypts the file
-- Does not remove the encrypted file.
decrypt :: FilePath -> FilePath -> IO ExitCode
decrypt file dir = do
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
         mkdir storageDir
         decrypt targetFile storageDir .||. do
            rmtree storageDir
            Turtle.die "safety check: decrypt failed"
         -- sanity check that the decrypted content is the same
         sameDirectoryContents absDirectory storageDir .||.
            Turtle.die "safety check failed: decrypted content does not match original content"
         rmtree absDirectory
         symlink storageDir absDirectory
         rmtree storageDir


linkCommandPath :: Command -> IO FilePath
linkCommandPath command = do
   binDir <- getBinDir
   pure $ binDir </> unpack command


linkCommand :: Command -> IO ()
linkCommand command = do
   unhideExePath <- getExecutablePath
   linkTarget <- linkCommandPath command
   catch (symlink unhideExePath linkTarget) $ \(_ :: SomeException) -> pure ()


run :: IO ()
run = do
   progName <- getProgName
   if progName == "unhide"
      then do
         args <- getArgs
         case args of
            "run":command:cmdArgs -> runCommand (pack command) $ fmap pack cmdArgs
            _                    -> runSelf
      else do
         cmdArgs <- getArgs
         runCommand (pack progName) $ fmap pack cmdArgs


initializeCommand c cc = do
   forM_ (directories cc) $ hideDirectory (userId cc) (storageBase cc)
   ccs <- loadCommands
   linkTarget <- linkCommandPath c
   exists <- testfile linkTarget
   when exists $ rm linkTarget -- We must remove it in order to find the real path.
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
         mkdir absDirectory
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
      Nothing -> Turtle.die $ format ("unhide: command '"%s%"' not found in configuration") command
      Just cc -> do
         x <- go cc (directories cc)
         when (x == ExitSuccess) $ linkCommand command
   where
      go cc [] = do
         case origPath cc of
            Nothing -> do
               eprintf ("missing executable path for "%s%"\n") command
               pure $ ExitFailure 1
            Just path -> do
               proc (pack path) args empty

      go cc (dir:dirs) = do
         (_absDirectory, storageDir, targetFile) <- calcDirectories dir (storageBase cc)
         mkdir storageDir
         x <- decrypt targetFile storageDir
         if x /= ExitSuccess then do
            eprintf ("decrypt failed: "%fp%"\n") dir
            rmtree storageDir
            pure x
         else do

            xx <- go cc dirs
            
            case xx of
               ExitSuccess -> do
                  xxx <- encrypt storageDir targetFile (userId cc)
                  unless (xxx == ExitSuccess) $ eprintf ("encrypt failed: "%fp%"\n") dir
               ExitFailure r -> eprintf "command failed\n"
            rmtree storageDir
            pure xx
