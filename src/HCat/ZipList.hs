module HCat.ZipList where
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NL
import GHC.Stack

data ZipList a = ZipList [a] a [a]
  deriving (Eq, Show)

fromNonEmpty :: NonEmpty a -> ZipList a
fromNonEmpty (a:|as) = ZipList [] a as

toNonEmpty :: ZipList a -> NonEmpty a
toNonEmpty (ZipList t c h) = NL.prependList t' $ c :| h
  where t' = reverse t

fromList :: [a] -> Maybe (ZipList a)
fromList = fmap fromNonEmpty . NL.nonEmpty

toList :: ZipList a -> [a]
toList = NL.toList . toNonEmpty

insertBack :: a -> ZipList a -> ZipList a
insertBack a (ZipList t c h) = ZipList (c:t) a h

insertFront :: a -> ZipList a -> ZipList a
insertFront a (ZipList t c h) = ZipList t a (c:h)

insertAtCursor :: a -> ZipList a -> ZipList a
insertAtCursor a (ZipList t c h) = ZipList (c:t) a h

dropAtCursor :: ZipList a -> Maybe (ZipList a)
dropAtCursor (ZipList t _c h)
  | (x:xs) <- h = Just $ ZipList t x xs
  | otherwise = Nothing

unsafeDropAtCursor :: HasCallStack => ZipList a -> ZipList a
unsafeDropAtCursor (ZipList t _c h) = ZipList t (head h) (tail h)

isAtStart :: ZipList a -> Bool
isAtStart (ZipList t _c _h) = null t

isAtEnd :: ZipList a -> Bool
isAtEnd (ZipList _t _c h) = null h

cursorToStart :: ZipList a -> ZipList a
cursorToStart z
  | isAtStart z = z
  | otherwise = cursorToStart $ cursorLast z

cursorToEnd :: ZipList a -> ZipList a
cursorToEnd z
  | isAtEnd z = z
  | otherwise = cursorToEnd $ cursorNext z

cursorValue :: ZipList a -> a
cursorValue (ZipList _ c _) = c

cursorNext :: ZipList a -> ZipList a
cursorNext z@(ZipList _t _c []) = z
cursorNext (ZipList t c (x:xs)) = ZipList (c:t) x xs

cursorLast :: ZipList a -> ZipList a
cursorLast z@(ZipList [] _c _h) = z
cursorLast (ZipList (x:xs) c h) = ZipList xs x (c:h)
