{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.Anagrams where

import Course.Core
import Course.List
import Course.Functor

{-

Functions you will need
--
* fmap :: (a -> b) -> IO a -> IO b
* readFile :: FilePath -> IO Str
* lines :: Str -> [Str]
* permutations :: [a] -> [[a]]
* intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
* toLower :: Char -> Char

Functions that might help
-
* on :: (b -> b -> c) -> (a -> b) -> a -> a -> c

-}


-- Return all anagrams of the given string
-- that appear in the given dictionary file.

anagramsEq :: (Chars -> Chars -> Bool) -> Chars
  -> FilePath
  -> IO (List Chars)
anagramsEq f as p = unique <$> intersectBy f (permutations as) <$> lines <$> readFile p
  where unique = foldRight (\curr acc -> if elem curr acc then acc else curr :. acc) Nil

anagrams ::
  Chars
  -> FilePath
  -> IO (List Chars)
anagrams = anagramsEq (==)

-- Compare two strings for equality, ignoring case
equalIgnoringCase ::
  Chars
  -> Chars
  -> Bool
equalIgnoringCase a b = (toLower <$> a) == (toLower <$> b)
