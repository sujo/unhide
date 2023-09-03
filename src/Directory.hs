module Directory
   ( Status(..)
   , detect
   ) where

import Relude
import Turtle
import UnliftIO (catch)

import Types
import Helper (calcDirectories)


data Status = DsSecure -- ^ Both the directory symlink and the encrypted file exist, and it is not mounted.
                     | DsMounted -- ^ Both the directory symlink and the encrypted file exist, and it is mounted.
                     
                     | DsMissingLinkMounted -- ^ The encrypted file exists and is mounted, but the directory symlink is missing.
                     | DsMissingLink -- ^ The encrypted file exists, but the directory symlink is missing.
                     | DsNewDirMounted -- ^ The encrypted file exists and is mounted, but the directory is a real directory.
                                   -- We must assume that someone has replaced the symlink with a new directory that we should not delete.
                     | DsNewDir -- ^ The encrypted file exists, but the directory is a real directory.
                                   -- We must assume that someone has replaced the symlink with a new directory that we should not delete.
                     | DsUntracked -- ^ The directory is not a symlink and there is no encrypted file.
                     | DsMissingFile -- ^ The directory is a symlink and there is no encrypted file.
                     | DsMissingDirectory -- ^ Neither the directory nor the encrypted file exist.
                                          -- We assume that this is a configuration error.


detect :: FilePath -> FilePath -> FilePath -> IO Status
detect absDirectory storageDir targetFile = do
      fileExists <- testfile targetFile
      linkTarget <- catch (readlink absDirectory) $ \(_ :: SomeException) -> pure ""
      storageDirExists <- testdir storageDir
      dirExists <- testdir absDirectory
      pure $ case (fileExists, linkTarget == storageDir, storageDirExists, dirExists) of
         (True , True , False, _    ) -> DsSecure
         (True , True , True , _    ) -> DsMounted
         (True , False, True , True ) -> DsNewDirMounted
         (True , False, True , False) -> DsMissingLinkMounted
         (True , False, False, True ) -> DsNewDir
         (True , False, False, False) -> DsMissingLink
         (False, True , _    , _    ) -> DsMissingFile
         (False, False, _    , False) -> DsMissingDirectory
         (False, False, _    , True ) -> DsUntracked
