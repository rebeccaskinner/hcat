module HCat.ZipList where
import Data.List.NonEmpty (NonEmpty(..))

data ZipList a = ZipList [a] a [a]


fromNonEmpty :: NonEmpty a -> ZipList a
fromNonEmpty (a :| as) = ZipList [] a as

fromList :: a -> [a] -> ZipList a
fromList = ZipList []

unsafeFromList :: [a] -> ZipList a
unsafeFromList (a:as) = ZipList [] a as
unsafeFromList _ = error "unsafeFromList: unexpected empty list"
