{-# LANGUAGE Trustworthy #-}
module RIO (RIO(), runRIO, RIO.readFile) where
import Control.Applicative
import Control.Monad
import Data.List

-- Notice that symbol UnsafeRIO is not exported from this module!
newtype RIO a = UnsafeRIO (IO a)

runRIO :: RIO a -> IO a
runRIO (UnsafeRIO io) = io

-- XXX You need to implement these three methods
instance Monad RIO where
  return = UnsafeRIO . return
  UnsafeRIO ioa >>= k = UnsafeRIO (ioa >>= f)
    where f a = case k a of
            UnsafeRIO iob -> iob
  fail = undefined

instance Functor RIO where
  fmap = liftM
instance Applicative RIO where
  pure = return
  (<*>) = ap

-- Returns True iff access is allowed to file name
pathOK :: FilePath -> IO Bool
pathOK file =
  return $ not (isInfixOf "/.." file) && isPrefixOf "/tmp" file

readFile :: FilePath -> RIO String
readFile file = UnsafeRIO $ do
  ok <- pathOK file
  if ok then Prelude.readFile file else return ""
