{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-

Write a function (dollars) that accepts a `String` and returns a `String`.
It will accept a numeric value as input, representing an amount of money, and convert to its transcribed English.

For example, the input "1.11" will result in a return value of "one dollar and eleven cents"

Invalid characters should be ignored, meaning that every input string has an output string.
The empty string produces "zero dollars and zero cents"

There is a `test` function below that lists more examples of input and output. There are also functions and
data structures that may assist you in deriving the result. It is not compulsory that they are used.

-}

module Course.Cheque where

import Course.Core
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.Parser
import Course.MoreParser

-- $setup
-- >>> :set -XOverloadedStrings

-- The representation of the grouping of each exponent of one thousand. ["thousand", "million", ...]
illion ::
  List Chars
illion =
  let preillion ::
        List (Chars -> Chars)
      preillion =
        listh [
          const ""
        , const "un"
        , const "do"
        , const "tre"
        , const "quattuor"
        , const "quin"
        , const "sex"
        , const "septen"
        , const "octo"
        , \q -> if "n" `isPrefixOf` q then "novem" else "noven"
        ]
      postillion ::
        List Chars
      postillion =
        listh [
          "vigintillion"
        , "trigintillion"
        , "quadragintillion"
        , "quinquagintillion"
        , "sexagintillion"
        , "septuagintillion"
        , "octogintillion"
        , "nonagintillion"
        , "centillion"
        , "decicentillion"
        , "viginticentillion"
        , "trigintacentillion"
        , "quadragintacentillion"
        , "quinquagintacentillion"
        , "sexagintacentillion"
        , "septuagintacentillion"
        , "octogintacentillion"
        , "nonagintacentillion"
        , "ducentillion"
        , "deciducentillion"
        , "vigintiducentillion"
        , "trigintaducentillion"
        , "quadragintaducentillion"
        , "quinquagintaducentillion"
        , "sexagintaducentillion"
        , "septuagintaducentillion"
        , "octogintaducentillion"
        , "nonagintaducentillion"
        , "trecentillion"
        , "decitrecentillion"
        , "vigintitrecentillion"
        , "trigintatrecentillion"
        , "quadragintatrecentillion"
        , "quinquagintatrecentillion"
        , "sexagintatrecentillion"
        , "septuagintatrecentillion"
        , "octogintatrecentillion"
        , "nonagintatrecentillion"
        , "quadringentillion"
        , "deciquadringentillion"
        , "vigintiquadringentillion"
        , "trigintaquadringentillion"
        , "quadragintaquadringentillion"
        , "quinquagintaquadringentillion"
        , "sexagintaquadringentillion"
        , "septuagintaquadringentillion"
        , "octogintaquadringentillion"
        , "nonagintaquadringentillion"
        , "quingentillion"
        , "deciquingentillion"
        , "vigintiquingentillion"
        , "trigintaquingentillion"
        , "quadragintaquingentillion"
        , "quinquagintaquingentillion"
        , "sexagintaquingentillion"
        , "septuagintaquingentillion"
        , "octogintaquingentillion"
        , "nonagintaquingentillion"
        , "sescentillion"
        , "decisescentillion"
        , "vigintisescentillion"
        , "trigintasescentillion"
        , "quadragintasescentillion"
        , "quinquagintasescentillion"
        , "sexagintasescentillion"
        , "septuagintasescentillion"
        , "octogintasescentillion"
        , "nonagintasescentillion"
        , "septingentillion"
        , "deciseptingentillion"
        , "vigintiseptingentillion"
        , "trigintaseptingentillion"
        , "quadragintaseptingentillion"
        , "quinquagintaseptingentillion"
        , "sexagintaseptingentillion"
        , "septuagintaseptingentillion"
        , "octogintaseptingentillion"
        , "nonagintaseptingentillion"
        , "octingentillion"
        , "decioctingentillion"
        , "vigintioctingentillion"
        , "trigintaoctingentillion"
        , "quadragintaoctingentillion"
        , "quinquagintaoctingentillion"
        , "sexagintaoctingentillion"
        , "septuagintaoctingentillion"
        , "octogintaoctingentillion"
        , "nonagintaoctingentillion"
        , "nongentillion"
        , "decinongentillion"
        , "vigintinongentillion"
        , "trigintanongentillion"
        , "quadragintanongentillion"
        , "quinquagintanongentillion"
        , "sexagintanongentillion"
        , "septuagintanongentillion"
        , "octogintanongentillion"
        , "nonagintanongentillion"
        ]
  in listh [
       ""
     , "thousand"
     , "million"
     , "billion"
     , "trillion"
     , "quadrillion"
     , "quintillion"
     , "sextillion"
     , "septillion"
     , "octillion"
     , "nonillion"
     , "decillion"
     , "undecillion"
     , "duodecillion"
     , "tredecillion"
     , "quattuordecillion"
     , "quindecillion"
     , "sexdecillion"
     , "septendecillion"
     , "octodecillion"
     , "novemdecillion"
     ] ++ lift2 ((++) =<<) preillion postillion

-- A data type representing the digits zero to nine.
data Digit =
  Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Eq, Ord, Show)

showDigit ::
  Digit
  -> Chars
showDigit Zero =
  "zero"
showDigit One =
  "one"
showDigit Two =
  "two"
showDigit Three =
  "three"
showDigit Four =
  "four"
showDigit Five =
  "five"
showDigit Six =
  "six"
showDigit Seven =
  "seven"
showDigit Eight =
  "eight"
showDigit Nine =
  "nine"

-- A data type representing one, two or three digits, which may be useful for grouping.
data Digit3 =
  D1 Digit
  | D2 Digit Digit
  | D3 Digit Digit Digit
  deriving (Eq, Show)

-- Possibly convert a character to a digit.
fromChar ::
  Char
  -> Optional Digit
fromChar '0' =
  Full Zero
fromChar '1' =
  Full One
fromChar '2' =
  Full Two
fromChar '3' =
  Full Three
fromChar '4' =
  Full Four
fromChar '5' =
  Full Five
fromChar '6' =
  Full Six
fromChar '7' =
  Full Seven
fromChar '8' =
  Full Eight
fromChar '9' =
  Full Nine
fromChar _ =
  Empty

-- | Take a numeric value and produce its English output.
--
-- >>> dollars "0"
-- "zero dollars and zero cents"
--
-- >>> dollars "1"
-- "one dollar and zero cents"
--
-- >>> dollars "0.1"
-- "zero dollars and ten cents"
--
-- >>> dollars "1."
-- "one dollar and zero cents"
--
-- >>> dollars "0."
-- "zero dollars and zero cents"
--
-- >>> dollars "0.0"
-- "zero dollars and zero cents"
--
-- >>> dollars ".34"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "0.3456789"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "1.0"
-- "one dollar and zero cents"
--
-- >>> dollars "1.01"
-- "one dollar and one cent"
--
-- >>> dollars "a1a"
-- "one dollar and zero cents"
--
-- >>> dollars "a1a.a0.7b"
-- "one dollar and seven cents"
--
-- >>> dollars "100"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.0"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00000"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "1000456.13"
-- "one million four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "1001456.13"
-- "one million one thousand four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "16000000456.13"
-- "sixteen billion four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "100.45"
-- "one hundred dollars and forty-five cents"
--
-- >>> dollars "100.07"
-- "one hundred dollars and seven cents"
--
-- >>> dollars "9abc9def9ghi.jkl9mno"
-- "nine hundred and ninety-nine dollars and ninety cents"
--
-- >>> dollars "12345.67"
-- "twelve thousand three hundred and forty-five dollars and sixty-seven cents"
--
-- >>> dollars "456789123456789012345678901234567890123456789012345678901234567890.12"
-- "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion one hundred and twenty-three octodecillion four hundred and fifty-six septendecillion seven hundred and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion six hundred and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion five hundred and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion four hundred and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion three hundred and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion two hundred and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents"

tenth :: Digit -> Digit -> Chars
tenth Zero a = showDigit a
tenth One One = "eleven"
tenth One Two = "twelve"
tenth One Three = "thirteen"
tenth One Four = "fourteen"
tenth One Five = "fifteen"
tenth One Six = "sixteen"
tenth One Seven = "seventeen"
tenth One Eight = "eighteen"
tenth One Nine = "nineteen"
tenth a b = showTenthHyphen a b

showTenth :: Digit -> Chars
showTenth One = "ten"
showTenth Two = "twenty"
showTenth Three = "thirty"
showTenth Four = "forty"
showTenth Five = "fifty"
showTenth Six = "sixty"
showTenth Seven = "seventy"
showTenth Eight = "eighty"
showTenth Nine = "ninety"
showTenth _ = ""

showHundred :: Digit -> Chars
showHundred a = showDigit a ++ " hundred"

hundreds :: Digit -> Digit -> Digit -> Chars
hundreds Zero b c = tenth b c
hundreds a Zero Zero = showHundred a
hundreds a b c = showHundred a ++ " and " ++ tenth b c

showTenthHyphen :: Digit -> Digit -> Chars
showTenthHyphen a Zero = showTenth a
showTenthHyphen a b = showTenth a ++ "-" ++ showDigit b

fromChar' :: Char -> (Char, Optional Digit)
fromChar' c = (c, fromChar c)

notDigit :: Parser (List Char)
notDigit = list . satisfyAll $ not . isDigit :. (/= '.') :. Nil

digitParser :: Parser Digit
digitParser = between notDigit notDigit (f =<< fromChar' <$> digit)
  where f (_, Full a) = pure a
        f (c, Empty) = unexpectedCharParser c
        
digitParserOrZero :: Parser Digit
digitParserOrZero = digitParser ||| pure Zero

data FullNumber a b = FullNumber a b
  deriving Show

digitI :: Parser (List Digit3)
digitI = foldRight f Nil <$> list digitParser
  where f curr (D1 a :. rest) = D2 curr a :. rest
        f curr (D2 a b :. rest) = D3 curr a b :. rest
        f curr acc = D1 curr :. acc

digitF :: Parser (Digit, Digit)
digitF = (,) <$> digitParserOrZero <*> digitParserOrZero

dollarsParser :: Parser (FullNumber (List Digit3) (Digit, Digit))
dollarsParser = FullNumber <$> digitI <* list (is '.') <*> digitF

d32Cs :: Digit3 -> Chars
d32Cs (D3 d1 d2 d3) = hundreds d1 d2 d3
d32Cs (D2 d1 d2) = hundreds Zero d1 d2
d32Cs (D1 d1) = hundreds Zero Zero d1

allZeros :: Digit3 -> Bool
allZeros (D3 Zero Zero Zero) = True
allZeros (D2 Zero Zero) = True
allZeros (D1 Zero) = True
allZeros _ = False

isOne :: Digit3 -> Bool
isOne (D3 Zero Zero One) = True
isOne (D2 Zero One) = True
isOne (D1 One) = True
isOne _ = False

integerGen :: List Digit3 -> Chars
integerGen fs = n' ++ d fs
  where n' = if n == "" then showDigit Zero ++ " " else n
        n = foldRight f "" . reverse . zip (reverse fs) $ illion
        f (d3, i) acc = optionalD3 ++ optionalSpace ++ acc
                      where optionalD3
                              | allZeros d3 = ""
                              | otherwise = d32Cs d3 ++ " " ++ i
                            optionalSpace
                              | acc == "" = ""
                              | allZeros d3 = ""
                              | otherwise = " "
        d as = case reverse as of
          (a :. rest) -> if all allZeros rest && isOne a then "dollar" else "dollars"
          _ -> "dollars"

centGen :: Digit -> Digit -> Chars
centGen d1 d2 = tenth d1 d2 ++ cents d1 d2
  where cents Zero One = " cent"
        cents _ _ = " cents"

generator :: ParseResult (FullNumber (List Digit3) (Digit, Digit)) -> Chars
generator (Result _ (FullNumber fs (d1, d2))) = integerGen fs ++ " and " ++ centGen d1 d2
generator err = show' err

dollars ::
  Chars
  -> Chars
dollars = generator . parse dollarsParser
