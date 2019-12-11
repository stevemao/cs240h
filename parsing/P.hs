import Control.Monad
import Control.Applicative
import Data.List
import Data.Char

newtype Parser s a = P { runP :: s -> Maybe (a, s) }

instance Monad (Parser s) where
  (>>=)  = bind
  return = shove

bind :: Parser s a -> (a -> Parser s b) -> Parser s b
bind parse next = P $ \input ->
  case runP parse input of
    Nothing          -> Nothing
    Just (a, input') -> runP (next a) input'

shove :: a -> Parser s a
shove a = P $ \input -> Just (a, input)

instance Functor (Parser s) where
  fmap f p = P $ \input ->
    case runP p input of
      Nothing          -> Nothing
      Just (a, input') -> Just (f a, input')

string :: String -> Parser String String
string pat = P $ \input ->
  case stripPrefix pat input of
    Nothing   -> Nothing
    Just rest -> Just (pat, rest)

number :: Parser String Int
number = P $ \input ->
  let (h,t) = span isDigit input
  in case reads h of
       [(n,_)] -> Just (n,t)
       _       -> Nothing

version3 = do
  string "HTTP/"
  maj <- number
  string "."
  min <- number
  return (maj,min)

instance Applicative (Parser s) where
  pure  = return
  (<*>) = ap

version4 = (,) <$>
           (string "HTTP/" *> number <* string ".") <*>
           number

version5 = (,) <$>
           ("HTTP/" .*> number <*. ".") <*>
           number

(.*>) :: String -> Parser String b -> Parser String b
a .*> b = string a *> b

(<*.) :: Parser String a -> String -> Parser String a
a <*. b = a <* string b

infixl 4 .*>
infixl 4 <*.

instance Alternative (Parser s) where
    empty = P $ \_ -> Nothing

    f <|> g = P $ \input ->
      case runP f input of
        Nothing -> runP g input
        result  -> result
