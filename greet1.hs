module Main where
import System.IO

greet h = do
  hPutStrLn h "What is your name?"
  hGetLine h >>= hPutStrLn h . ("Hi, " ++)

withTty = withFile "/dev/tty" ReadWriteMode

main = withTty greet
