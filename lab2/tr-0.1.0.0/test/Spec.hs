-- | Test Haskell tr implementation.
--
-- We provide a few very simple tests here as a demonstration. You should add
-- more of your own tests!
--
-- Run the tests with `stack test`.
module Main (main) where

import Test.Hspec
import Test.QuickCheck

import Tr

type CharSet' = NonEmptyList Char

tr' :: CharSet -> CharSet -> String -> String
tr' "-d" inset = tr inset Nothing
tr' inset outset = tr inset (Just outset)


-- | Test harness.
main :: IO ()
main = hspec $ describe "Testing tr" $ do
    describe "single translate" $
      it "a -> b" $
        tr' "a" "b" "a" `shouldBe` "b"

    describe "stream translate" $
      it "a -> b" $
        tr' "a" "b" "aaaa" `shouldBe` "bbbb"

    describe "extend input set" $
      it "abc -> d" $
        tr' "abc" "d" "abcd" `shouldBe` "dddd"

    describe "input number" $
      it "13 -> 24" $
        tr' "13" "24" "13613" `shouldBe` "24624"

    describe "Upper case" $
      it "abc -> d" $
        tr' "HW" "hw" "Hello World!" `shouldBe` "hello world!"

    describe "Delete" $
      it "-d HW" $
        tr' "-d" "HW" "Hello World!" `shouldBe` "ello orld!"

    describe "Delete v2" $
      it "-d l" $
       tr' "-d" "l" "Hello world!" `shouldBe` "Heo word!"

    describe "Reverse delete" $
      it "d- l" $
       tr' "d-" "l" "ads-sda" `shouldBe` "alslsla"

    describe "Delete all" $
      it "-d abc" $
       tr' "-d" "abc" "abcabcabc" `shouldBe` ""    

    describe "tr quick-check" $
      it "empty input is identity" $ property prop_empty_id
      
-- | An example QuickCheck test. Tests the invariant that `tr` with an empty
-- input string should produce and empty output string.
prop_empty_id :: CharSet' -> CharSet' -> Bool
prop_empty_id (NonEmpty set1) (NonEmpty set2)
  = tr' set1 set2 "" == ""

