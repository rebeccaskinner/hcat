module HCat.FileInfo where
-- import System.Directory
import HCat.Foo

fileInfoTest :: Int
fileInfoTest = fooTest
--
-- getFileInfo :: FilePath -> IO FileInfo
-- getFileInfo fname = do
--   mtime <- getModificationTime fname
--   size <- getFileSize fname
--   permissions <- getPermissions fname
--   pure $ FileInfo
--     { filePath = fname
--     , fileSize = fromIntegral size
--     , fileMTime = mtime
--     , fileReadable = readable permissions
--     , fileWriteable = writable permissions
--     , fileExecutable = executable permissions
--     }
