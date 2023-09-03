module Helper where

import Relude
import Data.Hashable (hash)
import System.Directory (makeAbsolute, getAppUserDataDirectory)
import Turtle


calcDirectories :: FilePath -> FilePath -> IO (FilePath, FilePath, FilePath)
calcDirectories dir storageBaseDir = do
   absDirectory <- makeAbsolute dir
   let
      directoryHash = repr . abs . hash $ absDirectory
      storageDir = storageBaseDir </> "unhide" </> directoryHash
      targetFile = parent absDirectory </> directoryHash <.> "uh"
   pure (absDirectory, storageDir, targetFile)


sameDirectoryContents :: FilePath -> FilePath -> IO ExitCode
sameDirectoryContents a b = do
   lsa <- Turtle.sort (cd a >> lstree ".")
   lsb <- Turtle.sort (cd b >> lstree ".")
   pure $ if lsa == lsb then ExitSuccess else ExitFailure 1

