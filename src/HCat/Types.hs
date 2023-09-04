{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
module HCat.Types where
import Data.Time.Clock (UTCTime)
import Data.Word

data ByteUnit = Byte | KiB | MiB | GiB | TiB | PiB
  deriving (Eq, Ord, Enum, Bounded, Show)

class KnownByteUnit (u :: ByteUnit) where
  byteUnitVal :: proxy u -> ByteUnit

instance KnownByteUnit Byte where byteUnitVal = const Byte
instance KnownByteUnit KiB where byteUnitVal = const KiB
instance KnownByteUnit MiB where byteUnitVal = const MiB
instance KnownByteUnit GiB where byteUnitVal = const GiB
instance KnownByteUnit TiB where byteUnitVal = const TiB

newtype Bytes = Bytes { getBytes :: Word64 }
  deriving newtype (Eq, Show, Num, Enum, Ord, Real, Integral)

newtype Header a = Header {getHeader :: [a]}
  deriving newtype (Eq, Show, Foldable)

newtype Footer a = Footer {getFooter :: [a]}
  deriving newtype (Eq, Show, Foldable)

newtype PageWidth = PageWidth {getPageWidth :: Word32 }
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral)

newtype PageHeight = PageHeight {getPageHeight :: Word32 }
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral)

data FileInfo = FileInfo
  { filePath  :: FilePath
  , fileSize  :: Bytes
  , fileMTime :: UTCTime
  , fileReadable :: Bool
  , fileWriteable :: Bool
  , fileExecutable :: Bool
  } deriving stock Show
