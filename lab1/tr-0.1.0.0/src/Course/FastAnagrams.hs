{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> FilePath
  -> IO (List Chars)
fastAnagrams as p = setToList <$> toSet <$> intersectBy (\a b -> NoCaseString a == NoCaseString b) (permutations as) <$> lines <$> readFile p
  where toSet :: List Chars -> S.Set Chars
        toSet = foldRight S.insert S.empty
        setToList :: S.Set Chars -> List Chars
        setToList = S.fold (:.) Nil

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
