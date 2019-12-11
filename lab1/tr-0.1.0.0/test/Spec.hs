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
tr' inset outset = tr inset (Just outset)

tr'' :: CharSet -> String -> String
tr'' inset = tr inset Nothing

-- | Test harness.
main :: IO ()
main = hspec $ describe "Testing tr" $ do
    describe "single translate" $
      it "a -> b" $
        tr' "a" "b" "a" `shouldBe` "b"

    describe "stream translate" $
      it "a -> b" $
        tr' "a" "b" "aaaa" `shouldBe` "bbbb"

    describe "more than one input set" $
      it "abc -> def" $
        tr' "abc" "def" "abcd" `shouldBe` "defd"
  
    describe "extend input set" $
      it "abc -> d" $
        tr' "abc" "d" "abcd" `shouldBe` "dddd"

    describe "single delete" $
      it "a" $
        tr'' "a" "abcd" `shouldBe` "bcd"
  
    describe "stream delete" $
      it "a" $
        tr'' "a" "aaaa" `shouldBe` ""

    describe "delete" $
      it "abc" $
        tr'' "abc" "abcd" `shouldBe` "d"
  
    describe "tr quick-check" $
      it "empty input is identity" $ property prop_empty_id
      
    describe "tr quick-check non empty" $
      it "non empty" $ property prop_nonempty

-- | An example QuickCheck test. Tests the invariant that `tr` with an empty
-- input string should produce and empty output string.
prop_empty_id :: CharSet' -> CharSet' -> Bool
prop_empty_id (NonEmpty set1) (NonEmpty set2)
  = tr' set1 set2 "" == ""

prop_nonempty :: CharSet' -> CharSet' -> Bool
prop_nonempty (NonEmpty set1) (NonEmpty set2)
  = tr' set1 set2 "abc" == "abc"
