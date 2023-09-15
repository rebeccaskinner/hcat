{-# LANGUAGE OverloadedStrings #-}
module HCat.CommandKey where
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List
import Data.Char
import Data.Text (Text)
import Data.Word
import Data.Text qualified as Text

data CommandBuffer = CommandBuffer
  { commandBufferMaxSize :: !Word8
  , commandBuffer :: !ByteString
  }

emptyCommandBuffer :: Word8 -> CommandBuffer
emptyCommandBuffer maxSize = CommandBuffer maxSize BS.empty

commandBufferCons :: Word8 -> CommandBuffer -> CommandBuffer
commandBufferCons x (CommandBuffer maxSize buff) =
  CommandBuffer maxSize buff'
  where
    buff' = BS.take (fromIntegral maxSize) $ BS.cons x buff

commandBufferFindKey :: CommandBuffer -> (a -> CommandKey) -> [a] -> Maybe a
commandBufferFindKey (CommandBuffer _ buffer) getKey = find keyInBuffer
  where
    keyInBuffer a = commandKeyCode (getKey a) `BS.isInfixOf` buffer

evalCommandBuffer :: Word8 -> CommandBuffer -> [a] -> (a -> CommandKey) -> (CommandBuffer -> Maybe a -> r) -> r
evalCommandBuffer nextInput buffer commands getKey f = f finalBuffer command
  where
    buffer' = commandBufferCons nextInput buffer
    command = commandBufferFindKey buffer' getKey commands
    finalBuffer =
      case command of
        Nothing -> buffer'
        Just _command -> buffer' { commandBuffer = BS.empty }

data CommandKey = CommandKey
  { commandKeyCode :: !ByteString
  , commandKeyDescription :: !Text
  }

asciiKey :: Char -> CommandKey
asciiKey c = CommandKey
  { commandKeyCode = BS.singleton . fromIntegral . ord $ c
  , commandKeyDescription = Text.singleton c
  }

upArrowKey :: CommandKey
upArrowKey = CommandKey
  { -- Store keys in reverse order so we can search them in the buffer
    commandKeyCode = BS.pack [72, 224]
  , commandKeyDescription = "Up Arrow Key (↑)"
  }

leftArrowKey :: CommandKey
leftArrowKey = CommandKey
  { -- Store keys in reverse order so we can search them in the buffer
    commandKeyCode = BS.pack [75, 224]
  , commandKeyDescription = "Left Arrow Key (←)"
  }

rightArrowKey :: CommandKey
rightArrowKey = CommandKey
  { -- Store keys in reverse order so we can search them in the buffer
    commandKeyCode = BS.pack [77, 224]
  , commandKeyDescription = "Right Arrow Key (→)"
  }

downArrowKey :: CommandKey
downArrowKey = CommandKey
  { -- Store keys in reverse order so we can search them in the buffer
    commandKeyCode = BS.pack [80, 224]
  , commandKeyDescription = "Down Arrow Key (↓)"
  }

escapeKey :: CommandKey
escapeKey = CommandKey
  { commandKeyCode = BS.singleton 27
  , commandKeyDescription = "Escape Key"
  }

tabKey :: CommandKey
tabKey = CommandKey
  { commandKeyCode = BS.singleton 9
  , commandKeyDescription = "Tab Key"
  }

spaceKey :: CommandKey
spaceKey = CommandKey
  { commandKeyCode = BS.singleton 20
  , commandKeyDescription = "spaceKey Key"
  }
