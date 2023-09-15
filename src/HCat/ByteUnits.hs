{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
module HCat.ByteUnits where
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

unitMultiplier :: ByteUnit -> Word64
unitMultiplier unit = 1024 ^ fromEnum unit

convertBytes :: ByteUnit -> ByteUnit -> Word64 -> (Word64, Word64)
convertBytes fromUnit toUnit n
  | fromUnit > toUnit = upConvert
  | otherwise = downConvert
  where
    upConvert =
      let m = unitMultiplier fromUnit `div` unitMultiplier toUnit
      in (n * m, 0)
    downConvert =
      let d = unitMultiplier toUnit `div` unitMultiplier fromUnit
      in n `divMod` d

convertBytesTrunc :: ByteUnit -> ByteUnit -> Word64 -> Word64
convertBytesTrunc from to n = fst $ convertBytes from to n

kiB, miB, giB, tiB :: Word64 -> Bytes
kiB = Bytes . convertBytesTrunc KiB Byte
miB = Bytes . convertBytesTrunc MiB Byte
giB = Bytes . convertBytesTrunc GiB Byte
tiB = Bytes . convertBytesTrunc TiB Byte

unitSuffix :: ByteUnit -> String
unitSuffix unit =
  case unit of
    Byte -> "B"
    KiB -> "KiB"
    MiB -> "MiB"
    GiB -> "GiB"
    TiB -> "TiB"
    PiB -> "PiB"

optimalUnit :: Bytes -> ByteUnit
optimalUnit = snd . convertOptimal

convertOptimal :: Bytes -> (Word64, ByteUnit)
convertOptimal (Bytes b) = go TiB b
  where
    go :: ByteUnit -> Word64 -> (Word64, ByteUnit)
    go Byte n = (n, Byte)
    go u n =
      let n' = convertBytesTrunc Byte u n
      in if n' > 1 then (n', u) else go (pred u) n

formatUnit :: ByteUnit -> Bytes -> String
formatUnit u (Bytes b) = show b <> " " <> unitSuffix u

formatOptimal :: Bytes -> String
formatOptimal bytes = formatUnit (optimalUnit bytes) bytes
