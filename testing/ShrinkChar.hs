{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Char
import System.Random
import Test.QuickCheck hiding ((.&.))
import Data.List

newtype BigChar = Big Char
                deriving (Eq, Show, Random)

instance Arbitrary BigChar where
  arbitrary = choose (Big '0', Big '\x10FFFF')
  shrink (Big c) = map Big (shrinkChar c)

{- Write a body for shrinkChar below.

   Suggested pattern:
   0, c * 0.5, c * 0.75, c * 0.875, ... -}
  
shrinkChar :: Char -> [Char]
shrinkChar c = chr <$> floor <$> (ordc *) <$> se 
  where se = fst <$> iterate (\(t, p) -> (t + p, p * 0.5)) (0, 0.5)
        ordc = fromIntegral . ord $ c
