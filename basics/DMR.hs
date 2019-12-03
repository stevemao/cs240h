module DMR where

veryExpensive = pure

superExpensive val = len $ veryExpensive (val :: Int)
    where len [] = 0
          len (x:xs) = 1 + len xs
cachedResult = superExpensive 5
