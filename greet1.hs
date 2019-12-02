module Main where
import System.IO

-- Desugared version of original greet:
greet h = hPutStrLn h "What is your name?" >>= \_ ->
  hGetLine h >>= \name ->
  hPutStrLn h ("Hi, " ++ name)

withTty = withFile "/dev/tty" ReadWriteMode

main = withTty greet
