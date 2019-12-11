import Data.Char
import Data.List
import Data.Maybe

type Parser s a = s -> Maybe (a,s)

string :: String -> Parser String String
string pat input =
  case stripPrefix pat input of
    Nothing   -> Nothing
    Just rest -> Just (pat, rest)

number :: Parser String Int
number s = case reads h of
             [(n,_)] -> Just (n,t)
             _       -> Nothing
  where (h,t) = span isDigit s

versionDumb i0 =
  case string "HTTP/" i0 of
    Nothing -> Nothing
    Just (_,i1) ->
      case number i1 of
        Nothing -> Nothing
        Just (maj,i2) ->
          case string "." i2 of
            Nothing -> Nothing
            Just (n,i3) ->
              case number i3 of
                Nothing -> Nothing
                Just (min,i4) -> Just ((maj,min),i4)

andThen :: Parser s a -> (a -> Parser s b) -> Parser s b
andThen parse next = \input ->
  case parse input of
    Nothing          -> Nothing
    Just (a, input') -> next a input'

stuff :: a -> Parser s a
stuff a = \input -> Just (a, input)

version2 =
  string "HTTP/" `andThen` \_ ->
  number `andThen` \maj ->
  string "." `andThen` \_ ->
  number `andThen` \min ->
  stuff (maj,min)
