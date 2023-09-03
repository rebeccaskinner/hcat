{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
module HCat.TerminalSize where
import System.Process (readProcess)
import Data.Word
import Text.Read
import Control.Exception

newtype TPutError = TPutError String deriving Show
instance Exception TPutError

newtype TermSizeError = TermSizeError String deriving Show
instance Exception TermSizeError

data TermSize = TermSize
  { termRows :: Word16
  , termCols :: Word16
  } deriving (Eq, Show)

defaultTermSize :: TermSize
defaultTermSize =
  TermSize
  { termRows = 24
  , termCols = 80
  }

getTerminalSize :: IO TermSize
getTerminalSize = do
  termRows <- parseWord16 =<< tput "lines"
  termCols <- parseWord16 =<< tput "cols"
  pure TermSize{..}
  where
    newTPutError e = TPutError (show e)
    tput' dim = readProcess "tput" [dim] ""
    tput dim = catch @IOException (tput' dim) $ throwIO . newTPutError
    parseWord16 val =
      case readEither val of
        Left err -> throw $ TermSizeError err
        Right parsed -> pure parsed
