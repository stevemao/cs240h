{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}

import Data.Bits ((.&.), shiftR)
import Data.Char (chr, ord)
import Data.Word (Word16)
import System.Random (Random)
import Test.QuickCheck hiding ((.&.))

encodeChar :: Char -> [Word16]
encodeChar x
  | w < 0x10000 = [fromIntegral w]
  | otherwise   = [fromIntegral a, fromIntegral b]
  where w = ord x
        a = ((w - 0x10000) `shiftR` 10) + 0xD800
        b = (w .&. 0x3FF) + 0xDC00

encodeUtf16 :: [Char] -> [Word16]
encodeUtf16 = concatMap encodeChar

newtype BigChar = Big Char
                deriving (Eq, Ord, Show, Random)

instance Arbitrary BigChar where
    arbitrary = choose (Big '\0',Big '\x10FFFF')
    shrink (Big c) = map (Big . chr) . shrink . ord $ c

-- TODO: write a proper body for decodeUtf16

decodeUtf16 :: [Word16] -> [Char]
decodeUtf16 _ = []

-- TODO: write properties
