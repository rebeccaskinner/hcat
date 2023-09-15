{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module HCat.Paginate where
import Data.Text (Text)
import Data.Char
import Data.Text qualified as Text
import Data.Text.Unsafe (reverseIter, Iter(..))
import Data.Word

newtype Header a = Header {getHeader :: [a]}
  deriving newtype (Eq, Show, Foldable)

newtype Footer a = Footer {getFooter :: [a]}
  deriving newtype (Eq, Show, Foldable)

newtype PageWidth = PageWidth {getPageWidth :: Word32 }
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral)

newtype PageHeight = PageHeight {getPageHeight :: Word32 }
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral)

headerText :: Header Text -> Text
headerText = Text.unlines . getHeader

footerText :: Footer Text -> Text
footerText = Text.unlines . getFooter

wrapLine :: Int -> Text -> [Text]
wrapLine lineLength line
  | Text.length line <= lineLength = [line]
  | otherwise = hardWrap lineLength line

hardWrap :: Int -> Text -> [Text]
hardWrap lineLength line =
  let (maxLine, rest) = Text.splitAt lineLength line
      (wrapped, remainder) = softWrap maxLine
  in wrapped : wrapLine  lineLength (remainder <> rest)

softWrap :: Text -> (Text, Text)
softWrap line =
  case findLastSpace line (Text.length line - 1) of
    Nothing -> (line, Text.empty)
    Just idx -> Text.splitAt idx line

findLastSpace :: Text -> Int -> Maybe Int
findLastSpace line idx
  | idx <= 0 = Nothing
  | otherwise =
    let (Iter nextChar offset) = reverseIter line idx
    in if isSeparator nextChar then Just idx else findLastSpace line (idx + offset)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h,t) = splitAt n xs in h : chunksOf n t

paddedPage :: PageHeight -> Text -> Header Text -> [Text] -> Footer Text -> Text
paddedPage height paddingText header body footer =
  let
    headerSize = length header
    bodySize = length body
    footerSize = length footer
    paddingSize = fromIntegral height - (headerSize + bodySize + footerSize)
    padding = Text.unlines $ replicate paddingSize paddingText
  in headerText header <> Text.unlines body <> padding <> footerText footer

pagesWith :: PageHeight -> Header a -> [a] -> Footer a -> [[a]]
pagesWith pageSize header contents footer =
  let pageSize' = fromIntegral pageSize - (length header + length footer)
      rawPages = chunksOf pageSize' contents
  in map (\p -> getHeader header <> p <> getFooter footer) rawPages

pages :: PageWidth -> PageHeight -> Header Text -> Text -> Footer Text -> [Text]
pages width height header contents footer =
  let wrappedContents = concatMap (wrapLine $ fromIntegral width) (Text.lines contents)
  in map Text.unlines $ pagesWith height header wrappedContents footer
