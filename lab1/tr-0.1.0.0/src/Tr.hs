{-# LANGUAGE OverloadedStrings #-}
-- | Haskell tr implementation. Just supports the swap and delete modes:
-- * tr string1 string2
-- * tr -d string1
--
-- PLEASE DON'T CHANGE THE INTERFACE OF THIS FILE AS WE WILL EXPECT IT TO BE
-- THE SAME WHEN TESTING!
module Tr
    ( CharSet
    , tr
    ) where

import Course.Parser
import Course.List
import Course.Functor
import Course.Applicative
import Utils

-- | Just to give `tr` a more descriptive type
type CharSet = String

-- | 'tr' - the characters in the first argument are translated into characters
-- in the second argument, where first character in the first CharSet is mapped
-- to the first character in the second CharSet. If the first CharSet is longer
-- than the second CharSet, the last character found in the second CharSet is
-- duplicated until it matches in length.
--
-- If the second CharSet is a `Nothing` value, then 'tr' should run in delete
-- mode where any characters in the input string that match in the first
-- CharSet should be removed.
--
-- The third argument is the string to be translated (i.e., STDIN) and the
-- return type is the output / translated-string (i.e., STDOUT).
-- 
-- translate mode: tr "eo" (Just "oe") "hello" -> "holle"
-- delete mode: tr "e" Nothing "hello" -> "hllo"
--
-- It's up to you how to handle the first argument being the empty string, or
-- the second argument being `Just ""`, we will not be testing this edge case.
tr :: CharSet -> Maybe CharSet -> String -> String
tr (ii : iis) (Just (o : os)) xs = case parse parser (listh xs) of
                                    Result input a -> hlist a
                                    _ -> "Error!!"
        where parser = list $ rep ii o ||| character
              rep a b = const b Course.Functor.<$> is a
tr (ii : iis) Nothing xs = case parse parser (listh xs) of
                            Result input a -> hlist a
                            _ -> "Error!!"
        where parser = rep ii ||| list character
              rep a = list1 (is a) Course.Applicative.*> (parser ||| eos)
tr _ (Just "") _ = ""
tr [] _ xs = xs
tr _ _ "" = ""

eos ::
  Parser Chars
eos = P p
    where p "" = Result "" ""
          p a = UnexpectedString a
