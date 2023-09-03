{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
module HCat.FileInfo where
import Data.Time.Clock (UTCTime)
import System.Directory
import HCat.Types

getFileInfo :: FilePath -> IO FileInfo
getFileInfo fname = do
  mtime <- getModificationTime fname
  size <- getFileSize fname
  permissions <- getPermissions fname
  pure $ FileInfo
    { filePath = fname
    , fileSize = fromIntegral size
    , fileMTime = mtime
    , fileReadable = readable permissions
    , fileWriteable = writable permissions
    , fileExecutable = executable permissions
    }
