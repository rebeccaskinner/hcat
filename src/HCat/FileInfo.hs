module HCat.FileInfo where
import HCat.ByteUnits
import System.Directory
import Data.Time.Clock (UTCTime)

data FileInfo = FileInfo
  { filePath  :: FilePath
  , fileSize  :: Bytes
  , fileMTime :: UTCTime
  , fileReadable :: Bool
  , fileWriteable :: Bool
  , fileExecutable :: Bool
  } deriving stock Show

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
