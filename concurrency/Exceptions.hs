{-# LANGUAGE DeriveDataTypeable #-}

import Control.Exception
import Data.Typeable

data MyError = MyError String deriving (Show, Typeable)
instance Exception MyError

catcher :: IO a -> IO (Maybe a)
catcher action = fmap Just action `catch` handler
    where handler (MyError msg) = do putStrLn msg; return Nothing

pureCatcher :: a -> IO (Maybe a)
pureCatcher a = (a `seq` return (Just a))
                `catch` \(SomeException _) -> return Nothing

seqList :: [a] -> b -> b
seqList [] = id
seqList (a : as) = seq a . seqList as
