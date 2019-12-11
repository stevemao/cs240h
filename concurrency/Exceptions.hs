{-# LANGUAGE DeriveDataTypeable #-}
module Exceptions where
import Control.Exception
import Data.Typeable
import System.IO.Error

data MyError = MyError String deriving (Show, Typeable)
instance Exception MyError

catcher :: IO a -> IO (Maybe a)
catcher action = fmap Just action `catch` handler
    where handler (MyError msg) = do putStrLn msg; return Nothing

pureCatcher :: a -> IO (Maybe a)
pureCatcher a = (a `seq` return (Just a))
                `catch` \(SomeException _) -> return Nothing

seqList :: [a] -> b -> b
seqList = flip . foldl . flip $ seq

readFileIfExists f = catchJust p (readFile f) (\_ -> return "")
  where p e = if isDoesNotExistError e then Just e else Nothing
