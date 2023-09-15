module HCat.Pager where
import Data.Text (Text)
import Data.Char
import HCat.CommandKey


data PagerCommand =
  PagerCommand
  { pagerCommandKey :: CommandKey
  , pagerCommandSummary :: PagerOperation
  }

data PagerOperation
  = PagerNextLine
  | PagerPreviousLine
  | PagerReflowTerminal
  | PagerToggleSpellCheck
  | PagerNextDocument
  | PagerPreviousDocument
  | PagerShowHelp
  | PagerStartSearch
  | PagerExit

pagerCommands :: [PagerCommand]
pagerCommands =
  [ PagerCommand upArrowKey PagerPreviousLine
  , PagerCommand downArrowKey PagerNextLine
  , PagerCommand (asciiKey 'r') PagerReflowTerminal
  , PagerCommand (asciiKey 's') PagerToggleSpellCheck
  , PagerCommand rightArrowKey PagerNextDocument
  , PagerCommand leftArrowKey PagerPreviousDocument
  , PagerCommand (asciiKey '?') PagerShowHelp
  , PagerCommand (asciiKey '/') PagerStartSearch
  , PagerCommand (asciiKey 'q') PagerExit
  ]
