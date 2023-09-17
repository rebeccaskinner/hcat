module HCat.ZipListSpec where
import Prelude hiding (drop)
import HCat.ZipList
import Control.Exception
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NL
import Test.Hspec

spec :: Spec
spec = do
  describe "fromNonEmpty" $ do
    it "creates a ZipList with the correct values" $ do
      fromNonEmpty (1 :| [2,3,4,5]) `shouldBe` ZipList [] 1 [2,3,4,5]
    it "correctly handles a singleton list" $ do
      fromNonEmpty (NL.singleton 0) `shouldBe` ZipList [] 0 []
    it "correctly handles an infinite list" $ do
      let z = fromNonEmpty (1 :| [2..])
      isStart z `shouldBe` True
      cursorValue z `shouldBe` 1
      cursorValue (cursorNext z) `shouldBe` 2

  describe "toNonEmpty" $ do
    it "works for a singleton ziplist" $ do
      toNonEmpty (ZipList [] 0 []) `shouldBe` NL.singleton 0
    it "works when at the head of a ziplist" $ do
      toNonEmpty (ZipList [] 0 [1,2,3]) `shouldBe` 0 :| [1,2,3]
    it "works when at the tail of a ziplist" $ do
      toNonEmpty (ZipList [2,1,0] 3 []) `shouldBe` 0 :| [1,2,3]
    it "works when in the middle of a ziplist" $ do
      toNonEmpty (ZipList [2,1,0] 3 [4,5,6]) `shouldBe` 0 :| [1,2,3,4,5,6]
    it "works with an infinite zipList" $ do
      let nl = toNonEmpty $ ZipList ([3,2,1,0]) 4 [5..]
      NL.take 10 nl `shouldBe` [0..9]

  describe "fromList" $ do
    it "returns Nothing when given an empty list" $ do
      fromList @Int [] `shouldBe` Nothing
    it "creates a ZipList with the correct values" $ do
      fromList [1,2,3,4,5] `shouldBe` Just (ZipList [] 1 [2,3,4,5])
    it "correctly handles a singleton list" $ do
      fromList [0] `shouldBe` Just (ZipList [] 0 [])
    it "correctly handles an infinite list" $ do
      let z = fromList [1..]
      isStart <$> z `shouldBe` Just True
      cursorValue <$> z `shouldBe` Just 1
      cursorValue . cursorNext <$> z `shouldBe` Just 2

  describe "toList" $ do
    it "works for a singleton ziplist" $ do
      toList (ZipList [] 0 []) `shouldBe` [0]
    it "works when at the head of a ziplist" $ do
      toList (ZipList [] 0 [1,2,3]) `shouldBe` [0,1,2,3]
    it "works when at the tail of a ziplist" $ do
      toList (ZipList [2,1,0] 3 []) `shouldBe` [0,1,2,3]
    it "works when in the middle of a ziplist" $ do
      toList (ZipList [2,1,0] 3 [4,5,6]) `shouldBe` [0,1,2,3,4,5,6]
    it "works with an infinite zipList" $ do
      let l = toList $ ZipList ([3,2,1,0]) 4 [5..]
      take 10 l `shouldBe` [0..9]

  describe "insertFront" $ do
    it "inserts an element when the cursor is at the front of the list" $ do
      insertFront 0 (ZipList [] 1 [2,3]) `shouldBe` ZipList [] 0 [1,2,3]
    it "inserts an element when the cursor is at the back of the list" $ do
      insertFront 4 (ZipList [2,1] 3 []) `shouldBe` ZipList [2,1] 4 [3]
    it "inserts an element when the cursor in the middle of the list" $ do
      insertFront 4 (ZipList [2,1] 3 [5,6,7]) `shouldBe` ZipList [2,1] 4 [3,5,6,7]

  describe "insert" $ do
    it "inserts an element when the cursor is at the front of the list" $ do
      insert 0 (ZipList [] 1 [2,3]) `shouldBe` ZipList [] 0 [1,2,3]
    it "inserts an element when the cursor is at the back of the list" $ do
      insert 4 (ZipList [2,1] 3 []) `shouldBe` ZipList [2,1] 4 [3]
    it "inserts an element when the cursor in the middle of the list" $ do
      insert 4 (ZipList [2,1] 3 [5,6,7]) `shouldBe` ZipList [2,1] 4 [3,5,6,7]

  describe "insertBack" $ do
    it "inserts an element when the cursor is at the front of the list" $ do
      insertBack 0 (ZipList [] 1 [2,3]) `shouldBe` ZipList [1] 0 [2,3]
    it "inserts an element when the cursor is at the back of the list" $ do
      insertBack 4 (ZipList [2,1] 3 []) `shouldBe` ZipList [3,2,1] 4 []
    it "inserts an element when the cursor in the middle of the list" $ do
      insertBack 4 (ZipList [2,1] 3 [5,6,7]) `shouldBe` ZipList [3,2,1] 4 [5,6,7]

  describe "append" $ do
    it "inserts an element when the cursor is at the front of the list" $ do
      append 0 (ZipList [] 1 [2,3]) `shouldBe` ZipList [1] 0 [2,3]
    it "inserts an element when the cursor is at the back of the list" $ do
      append 4 (ZipList [2,1] 3 []) `shouldBe` ZipList [3,2,1] 4 []
    it "inserts an element when the cursor in the middle of the list" $ do
      append 4 (ZipList [2,1] 3 [5,6,7]) `shouldBe` ZipList [3,2,1] 4 [5,6,7]

  describe "update" $ do
    it "updates the element in a singleton list" $ do
      update 0 (ZipList [] 1 []) `shouldBe` ZipList [] 0 []
    it "updates the element when the cursor is at the front of the list" $ do
      update 0 (ZipList [] 1 [2,3,4]) `shouldBe` ZipList [] 0 [2,3,4]
    it "updates the element when the cursor is at the back of the list" $ do
      update 0 (ZipList [4,3,2] 1 []) `shouldBe` ZipList [4,3,2] 0 []

  describe "drop" $ do
    it "removes and element and moves forward when at the start of the list" $ do
      drop (ZipList [] 0 [1,2,3]) `shouldBe` Just (ZipList [] 1 [2,3])
    it "removes and element and moves forward when in the list" $ do
      drop (ZipList [3,2,1] 4 [5,6,7]) `shouldBe` Just (ZipList [3,2,1] 5 [6,7])
    it "removes and element and moves back when at the end of the list" $ do
      drop (ZipList [3,2,1] 4 []) `shouldBe` Just (ZipList [2,1] 3 [])
    it "returns Nothing when dropping the last element" $ do
      drop (ZipList [] 0 []) `shouldBe` Nothing

  describe "unsafeDrop" $ do
    it "removes and element and moves forward when at the start of the list" $ do
      unsafeDrop (ZipList [] 0 [1,2,3]) `shouldBe` ZipList [] 1 [2,3]
    it "removes and element and moves forward when in the list" $ do
      unsafeDrop (ZipList [3,2,1] 4 [5,6,7]) `shouldBe` ZipList [3,2,1] 5 [6,7]
    it "removes and element and moves back when at the end of the list" $ do
      unsafeDrop (ZipList [3,2,1] 4 []) `shouldBe` ZipList [2,1] 3 []
    it "returns Nothing when dropping the last element" $ do
      let z = unsafeDrop (ZipList [] 0 [])
      evaluate z `shouldThrow` anyException

  describe "isStart" $ do
    pure ()

  describe "isEnd" $ do
    pure ()

  describe "cursorToStart" $ do
    pure ()

  describe "cursorToEnd" $ do
    pure ()

  describe "cursorValue" $ do
    pure ()

  describe "cursorNext" $ do
    pure ()

  describe "cursorLast" $ do
    pure ()
